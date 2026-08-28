(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors          *)
(************************************************************************)
(* Flèche => RL agent: petanque                                         *)
(************************************************************************)

module Lsp = Fleche_lsp
open Petanque_json

(* Client wrap *)
module type Chans = sig
  val ic : in_channel
  val oc : Format.formatter
  val trace : ?verbose:string -> string -> unit
  val message : lvl:int -> message:string -> unit
end

(* Display incoming requests *)
let display_requests = false

let maybe_display_request method_ =
  if display_requests then Format.eprintf "received request: %s@\n%!" method_

let do_trace ~trace params =
  match Lsp.Base.TraceParams.of_yojson (`Assoc params) with
  | Ok { message; verbose } -> trace ?verbose message
  | Error _ -> ()

let do_message ~message params =
  match Lsp.Base.MessageParams.of_yojson (`Assoc params) with
  | Ok { type_; message = msg } -> message ~lvl:type_ ~message:msg
  | Error _ -> ()

(* Read until we find a response *)
let rec read_response ~trace ~message ic =
  match Lsp.Io.read_message ic with
  | Some (Ok (Lsp.Base.Message.Response r)) -> Ok r
  | Some (Ok (Notification { method_; params }))
    when String.equal method_ Lsp.Base.TraceParams.method_ ->
    do_trace ~trace params;
    read_response ~trace ~message ic
  | Some (Ok (Notification { method_; params }))
    when String.equal method_ Lsp.Base.MessageParams.method_ ->
    do_message ~message params;
    read_response ~trace ~message ic
  | Some (Ok (Request { method_; _ })) | Some (Ok (Notification { method_; _ }))
    ->
    maybe_display_request method_;
    read_response ~trace ~message ic
  | Some (Error err) -> Error err
  | None -> assert false (* not in our testing setup *)

let id_counter = ref 0

let get_id () =
  incr id_counter;
  !id_counter

module Wrap (R : Protocol.Request.S) (C : Chans) : sig
  val call : R.Params.t -> (R.Response.t, string) Result.t
end = struct
  let trace = C.trace
  let message = C.message

  let call params =
    let id = get_id () in
    let method_ = R.method_ in
    let params = Yojson.Safe.Util.to_assoc (R.Params.to_yojson params) in
    let request = Lsp.Base.Request.make ~id ~method_ ~params () in
    let () = Lsp.Io.send_message C.oc (Lsp.Base.Message.Request request) in
    read_response ~trace ~message C.ic |> fun r ->
    Result.bind r (function
      | Ok { id = _; result } -> R.Response.of_yojson result
      | Error { id = _; code = _; message; data = _ } -> Error message)
end

module S (C : Chans) = struct
  open Protocol
  open Protocol_shell

  (* Shell calls (they do have an equivalent version in LSP) *)
  let set_workspace =
    let module M = Wrap (SetWorkspace) (C) in
    M.call

  let toc =
    let module M = Wrap (TableOfContents) (C) in
    M.call

  (* Standard calls *)
  let get_root_state =
    let module M = Wrap (GetRootState) (C) in
    M.call

  let get_state_at_pos =
    let module M = Wrap (GetStateAtPos) (C) in
    M.call

  let start =
    let module M = Wrap (Start) (C) in
    M.call

  let run =
    let module M = Wrap (RunTac) (C) in
    M.call

  let run_at_pos =
    let module M = Wrap (RunAtPoint) (C) in
    M.call

  let goals =
    let module M = Wrap (Goals) (C) in
    M.call

  let premises =
    let module M = Wrap (Premises) (C) in
    M.call

  let state_equal =
    let module M = Wrap (StateEqual) (C) in
    M.call

  let state_hash =
    let module M = Wrap (StateHash) (C) in
    M.call

  let state_proof_equal =
    let module M = Wrap (StateProofEqual) (C) in
    M.call

  let state_proof_hash =
    let module M = Wrap (StateProofHash) (C) in
    M.call

  let state_free =
    let module M = Wrap (StateFree) (C) in
    M.call

  let state_stats =
    let module M = Wrap (StateStats) (C) in
    M.call

  let cache_trim =
    let module M = Wrap (CacheTrim) (C) in
    M.call

  let list_notations_in_statement =
    let module M = Wrap (ListNotations) (C) in
    M.call

  let ast =
    let module M = Wrap (PetAst) (C) in
    M.call

  let ast_at_pos =
    let module M = Wrap (AstAtPos) (C) in
    M.call

  let proof_info =
    let module M = Wrap (ProofInfo) (C) in
    M.call

  let proof_info_at_pos =
    let module M = Wrap (ProofInfoAtPos) (C) in
    M.call
end
