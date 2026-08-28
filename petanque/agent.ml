(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors          *)
(************************************************************************)
(* Flèche => RL agent: petanque                                         *)
(************************************************************************)

module SM = Lang.Compat.String.Map

module State = struct
  type t = Coq.State.t

  let to_coq x = x
  let of_coq x = x
  let hash = Coq.State.hash
  let name = "state"

  module Inspect = struct
    type t =
      | Physical  (** Flèche-based "almost physical" state eq *)
      | Goals
          (** Full goal equality; must faster than calling goals as it won't
              unelaborate them. Note that this may not fully capture proof state
              equality (it is possible to have similar goals but different
              evar_maps, but should be enough for all practical users. *)
  end

  let equal ?(kind = Inspect.Physical) =
    match kind with
    | Physical -> Coq.State.equal
    | Goals ->
      fun st1 st2 ->
        let st1, st2 = (Coq.State.lemmas ~st:st1, Coq.State.lemmas ~st:st2) in
        Option.equal Coq.Goals.Equality.equal_goals st1 st2

  module Proof = struct
    type t = Coq.State.Proof.t

    let equal ?(kind = Inspect.Physical) =
      match kind with
      | Physical -> Coq.State.Proof.equal
      | Goals -> Coq.Goals.Equality.equal_goals

    let hash = Coq.State.Proof.hash
  end

  let lemmas st = Coq.State.lemmas ~st
end

(** Petanque errors *)
module Error = struct
  type t =
    | Interrupted
    | Parsing of string
    | Coq of string
    | Anomaly of string
    | System of string
    | Theorem_not_found of string
    | No_node_at_point

  let to_string = function
    | Interrupted -> Format.asprintf "Interrupted"
    | Parsing msg -> Format.asprintf "Parsing: %s" msg
    | Coq msg -> Format.asprintf "Coq: %s" msg
    | Anomaly msg -> Format.asprintf "Anomaly: %s" msg
    | System msg -> Format.asprintf "System: %s" msg
    | Theorem_not_found msg -> Format.asprintf "Theorem_not_found: %s" msg
    | No_node_at_point -> Format.asprintf "No node at point"

  (* JSON-RPC server reserved codes *)
  let to_code = function
    | Interrupted -> -32001
    | Parsing _ -> -32002
    | Coq _ -> -32003
    | Anomaly _ -> -32004
    | System _ -> -32005
    | Theorem_not_found _ -> -32006
    | No_node_at_point -> -32007

  let coq e = Coq e
  let system e = System e
  let make e ~feedback = Request.Error.make (to_code e) e ~feedback
  let make_request e = make e ~feedback:[]
end

module R = struct
  type 'a t = ('a, Error.t) Request.R.t
end

module Run_opts = struct
  type t =
    { memo : bool [@default true]
    ; hash : bool [@default true]
    }
end

module Run_result = struct
  type 'a t =
    { st : 'a
    ; hash : int option [@default None]
    ; proof_finished : bool
    ; feedback : (int * string) list
    }

  let map ~f r = { r with st = f r.st }
end

let find_thm ~(doc : Fleche.Doc.t) ~thm =
  let { Fleche.Doc.toc; _ } = doc in
  let feedback = [] in
  match SM.find_opt thm toc with
  | None ->
    let msg = Format.asprintf "@[[find_thm] Theorem not found!@]" in
    Error Error.(make (Theorem_not_found msg) ~feedback)
  | Some node -> (
    (* If the node has an error we cannot assume the state is valid *)
    match List.filter Lang.Diagnostic.is_error node.diags with
    | [] -> Ok node
    | err :: _ ->
      let msg =
        Format.asprintf
          "@[[find_thm] Theorem found but failed with Coq error:@\n @[%a@]!@]"
          Coq.Pp_t.pp_with err.message
      in
      Error Error.(make (Theorem_not_found msg) ~feedback))

let execute_precommands ~token ~memo ~pre_commands ~(node : Fleche.Doc.Node.t) =
  match (pre_commands, node.prev, node.ast) with
  | Some pre_commands, Some prev, Some ast ->
    let st = prev.state in
    let open Coq.Protect.E.O in
    let* st = Fleche.Doc.run ~token ~memo ?loc:None ~st pre_commands in
    (* We re-interpret the lemma statement *)
    Fleche.Memo.Interp.eval ~token (st, ast.v)
  | _, _, _ -> Coq.Protect.E.ok node.state

(* XXX Fix better by making protect errors and request errors share the loc
   type, so we can execute with Coq locations *)
let clean_fb fbs =
  List.map
    (fun (lvl, { Coq.Message.Payload.msg; _ }) ->
      (lvl, { Coq.Message.Payload.range = None; quickFix = None; msg }))
    fbs

let protect_to_result (r : _ Coq.Protect.E.t) : (_, _) Result.t =
  match r with
  | { r = Interrupted; feedback } ->
    let feedback = clean_fb feedback in
    Error Error.(make Interrupted ~feedback)
  | { r = Completed (Error (User { msg; _ })); feedback } ->
    let feedback = clean_fb feedback in
    Error Error.(make (Coq (Coq.Pp_t.to_string msg)) ~feedback)
  | { r = Completed (Error (Anomaly { msg; _ })); feedback } ->
    let feedback = clean_fb feedback in
    Error Error.(make (Anomaly (Coq.Pp_t.to_string msg)) ~feedback)
  | { r = Completed (Ok r); feedback } -> Ok (r feedback)

let proof_finished { Coq.Goals.goals; stack; shelf; given_up; _ } =
  let check_stack stack =
    List.(
      for_all (fun (l, r) ->
          Lang.Compat.List.is_empty l && Lang.Compat.List.is_empty r))
      stack
  in
  List.for_all Lang.Compat.List.is_empty [ goals; shelf; given_up ]
  && check_stack stack

(* At some point we want to return both hashes *)
module Hash_kind = struct
  type t =
    | Full
    | Proof
  [@@warning "-37"]

  let hash ~kind st =
    match kind with
    | Full -> Some (State.hash st)
    | Proof -> Option.map State.Proof.hash (State.lemmas st)
end

let hash_mode = Hash_kind.Proof

(* XXX: duplicated with Request.R.of_execution, refactoring planned (not
   trivial) *)
let fb_print_string (lvl, { Coq.Message.Payload.msg; _ }) =
  (lvl, Coq.Pp_t.to_string msg)

let analyze_after_run ~hash st feedback =
  let proof_finished =
    let goals = Fleche.Info.Goals.get_goals_unit ~compact:false ~st in
    match goals with
    | None -> true
    | Some goals when proof_finished goals -> true
    | _ -> false
  in
  let hash = if hash then Hash_kind.hash ~kind:hash_mode st else None in
  let feedback = List.map fb_print_string feedback in
  Run_result.{ st; hash; proof_finished; feedback }

(* Would be nice to keep this in sync with the type annotations. *)
let default_opts = function
  | None -> { Run_opts.memo = true; hash = true }
  | Some opts -> opts

let get_root_state ?opts ~doc () =
  let opts = default_opts opts in
  let hash = opts.hash in
  let state = doc.Fleche.Doc.root in
  Ok (analyze_after_run ~hash state [])

let get_state_at_pos ?opts ~doc ~point () =
  match Fleche.Info.(LC.node ~doc ~point PrevIfEmpty) with
  | Some { Fleche.Doc.Node.state; _ } ->
    let opts = default_opts opts in
    let hash = opts.hash in
    Ok (analyze_after_run ~hash state [])
  | None -> Error (Error.make_request No_node_at_point)

let start ~token ~doc ?opts ?pre_commands ~thm () =
  let open Coq.Compat.Result.O in
  let* node = find_thm ~doc ~thm in
  (* Usually single shot, so we don't memoize *)
  let opts = default_opts opts in
  let memo, hash = (opts.memo, opts.hash) in
  let execution =
    let open Coq.Protect.E.O in
    let+ st = execute_precommands ~token ~memo ~pre_commands ~node in
    (* Note this runs on the resulting state, anyways it is purely functional *)
    analyze_after_run ~hash st
  in
  protect_to_result execution

let run ~token ?opts ~st ~tac () : (_ Run_result.t, Error.t) Request.R.t =
  let opts = default_opts opts in
  (* Improve with thm? *)
  let memo, hash = (opts.memo, opts.hash) in
  let execution =
    let open Coq.Protect.E.O in
    let+ st = Fleche.Doc.run ~token ~memo ?loc:None ~st tac in
    (* Note this runs on the resulting state, anyways it is purely functional *)
    analyze_after_run ~hash st
  in
  protect_to_result execution

(* Use a trans *)
let run_at_pos ~token ?opts ~doc ~point ~command () :
    (_ Run_result.t, Error.t) Request.R.t =
  match Fleche.Info.(LC.node ~doc ~point PrevIfEmpty) with
  | Some { Fleche.Doc.Node.state = st; _ } ->
    let open Coq.Compat.Result.O in
    let+ res = run ~token ?opts ~st ~tac:command () in
    (* Return more info eventually? *)
    Run_result.map ~f:(fun _ -> ()) res
  | None -> Error (Error.make_request No_node_at_point)

module Goal_opts = struct
  type t = { compact : bool }

  let default = { compact = true }
end

let goals ~token ~st ?(opts = Goal_opts.default) () =
  let f goals =
    let f = Coq.Goals.Reified_goal.map ~f:Coq.Pp_t.to_string in
    let g = Coq.Pp_t.to_string in
    (* XXX: Fixme, take feedback into account *)
    fun _feedback -> Option.map (Coq.Goals.map ~f ~g) goals
  in
  let pr = Fleche.Info.Goals.to_pp in
  let { Goal_opts.compact } = opts in
  Coq.Protect.E.map ~f (Fleche.Info.Goals.goals ~token ~compact ~pr ~st)
  |> protect_to_result

module Premise = struct
  module Info = struct
    type t =
      { kind : string (* type of object *)
      ; range : Lang.Range.t option (* a range *)
      ; offset : int * int (* a offset in the file *)
      ; raw_text : (string, string) Result.t (* raw text of the premise *)
      }
  end

  type t =
    { full_name : string
          (* should be a Coq DirPath, but let's go step by step *)
    ; file : string (* file (in FS format) where the premise is found *)
    ; info : (Info.t, string) Result.t (* Info about the object, if available *)
    }
end

(* We need some caching here otherwise it is very expensive to re-parse the glob
   files all the time.

   XXX move this caching to Flèche. *)
module Memo = struct
  (* String.hash added in 5.0 *)
  module MyString = struct
    include String

    let hash = Stdlib.Hashtbl.hash
  end

  module H = Hashtbl.Make (MyString)

  let table_glob = H.create 1000

  let open_file glob =
    match H.find_opt table_glob glob with
    | Some g -> g
    | None ->
      let g = Coq.Glob.open_file glob in
      H.add table_glob glob g;
      g

  let table_source = H.create 1000

  let input_source file =
    match H.find_opt table_source file with
    | Some res -> res
    | None ->
      if Sys.file_exists file then (
        let res =
          Ok Coq.Compat.Ocaml_414.In_channel.(with_open_text file input_all)
        in
        H.add table_source file res;
        res)
      else
        let res = Error "source file is not available" in
        H.add table_source file res;
        res
end

let info_of ~glob ~name =
  let open Coq.Compat.Result.O in
  let* g = Memo.open_file glob in
  Ok
    (Option.map
       (fun { Coq.Glob.Info.kind; offset } -> (kind, offset))
       (Coq.Glob.get_info g name))

let raw_of ~file ~offset =
  let bp, ep = offset in
  let open Coq.Compat.Result.O in
  let* c = Memo.input_source file in
  if String.length c < ep then Error "offset out of bounds"
  else Ok (String.sub c bp (ep - bp + 1))

let to_premise (p : Coq.Library_file.Entry.t) : Premise.t =
  let { Coq.Library_file.Entry.name; typ = _; file } = p in
  let file = Filename.(remove_extension file ^ ".v") in
  let glob = Filename.(remove_extension file ^ ".glob") in
  let info =
    match info_of ~glob ~name with
    | Ok None -> Error "not in glob table"
    | Error err -> Error err
    | Ok (Some (kind, offset)) ->
      let range = None in
      let raw_text = raw_of ~file ~offset in
      Ok { Premise.Info.kind; range; offset; raw_text }
  in
  { Premise.full_name = name; file; info }

let premises ~token ~st =
  (let open Coq.Protect.E.O in
   let* all_libs = Coq.Library_file.loaded ~token ~st in
   let+ all_premises = Coq.Library_file.toc ~token ~st all_libs in
   (* XXX: Fixme, take feedback into account *)
   fun _feedback -> List.map to_premise all_premises)
  |> protect_to_result

let simple_run_result res feedback =
  let proof_finished = false in
  let hash = None in
  let feedback = List.map fb_print_string feedback in
  Run_result.{ st = res; hash; proof_finished; feedback }

let list_notations_in_statement ~token ~st ~statement () :
    Coq.Notation_analysis.Info.t list Run_result.t R.t =
  (let open Coq.Protect.E.O in
   let pa = Coq.Parsing.Parsable.make Gramlib.Stream.(of_string statement) in
   let* ast = Coq.Parsing.parse ~token ~st pa in
   let+ ntnl =
     match ast with
     | Some ast ->
       let intern = Vernacinterp.fs_intern in
       Coq.Notation_analysis.notations_in_statement ~token ~intern ~st ast
     | None -> Coq.Protect.E.ok []
   in
   simple_run_result ntnl)
  |> protect_to_result

let ast ~token ~st ~text () : Coq.Ast.t option Run_result.t R.t =
  (let open Coq.Protect.E.O in
   let pa = Coq.Parsing.(Parsable.make Stream.(of_string text)) in
   let+ ast = Coq.Parsing.parse ~token ~st pa in
   simple_run_result ast)
  |> protect_to_result

let ast_at_pos ~doc ~point () =
  match Fleche.Info.(LC.node ~doc ~point Exact) with
  | Some { Fleche.Doc.Node.ast; _ } ->
    Ok (Option.map (fun ast -> ast.Fleche.Doc.Node.Ast.v) ast)
  | None -> Error (Error.make_request No_node_at_point)

module Proof_info = struct
  (** Take into account that there may be a mutual proof open. *)
  type t =
    { name : string
    ; statements : string list
    ; range : Lang.Range.t option
    }
end

let proof_info ~token ~st () : Proof_info.t option R.t =
  Format.eprintf "pi path2@\n%!";
  match Coq.State.lemmas ~st with
  | Some pst ->
    let name = Coq.State.Proof.name pst in
    let execution =
      let open Coq.Protect.E.O in
      let+ statements = Coq.State.Proof.statements ~token pst in
      fun _feedback -> Some { Proof_info.name; statements; range = None }
    in
    protect_to_result execution
  | None -> Ok None

let proof_info_from_ast_node ~token (node : Fleche.Doc.Node.t) =
  match Coq.State.lemmas ~st:node.state with
  | None -> Ok None
  | Some pst ->
    let name = Coq.State.Proof.name pst in
    let range = Some node.range in
    let execution =
      let open Coq.Protect.E.O in
      let+ statements = Coq.State.Proof.statements ~token pst in
      fun _feedback -> Some { Proof_info.name; statements; range }
    in
    protect_to_result execution

(* Flèche / petanque invariant, when this function is called, [doc] has the
   metadata ready for [point] *)
let proof_info_at_pos ~token ~doc ~point () =
  match Fleche.Info.(LC.node ~doc ~point PrevIfEmpty) with
  | Some node -> (
    match Fleche.Doc.Analysis.find_proof_start node with
    | None -> proof_info ~token ~st:node.state ()
    | Some node -> proof_info_from_ast_node ~token node)
  | None -> Error (Error.make_request No_node_at_point)

(* See PROTOCOL.md for details on versioning *)
let version = 4
