(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors           *)
(************************************************************************)
(* Flèche => document manager: Command Memoization                      *)
(************************************************************************)

module CS = Stats

(* XXX: We are missing good error handling here! Fix submitted upstream. *)
module Intern = struct
  let hc : (Names.DirPath.t, _) Hashtbl.t = Hashtbl.create 1000
  let use_cache = true

  exception LocateError of (Names.DirPath.t * Loadpath.Error.t)

  let reason = function
    | Loadpath.Error.LibUnmappedDir ->
      "Logical path was not found (missing _CoqProject settings)"
    | LibNotFound -> "Library wasn't found (no .vo in place?)"

  let () =
    CErrors.register_handler (function
      | LocateError (dp, error) ->
        Some
          Pp.(
            str "Couldn't find .vo file for "
            ++ Names.DirPath.print dp ++ str " : "
            ++ str (reason error))
      | _ -> None)

  let intern : Library.Intern.t =
   fun dp ->
    if use_cache then
      match Hashtbl.find_opt hc dp with
      | Some lib -> lib
      | None -> (
        match Loadpath.locate_absolute_library dp with
        | Error err ->
          let info = Exninfo.reify () in
          (Error (LocateError (dp, err), info), ("loadpath", "DP"))
        | Ok file ->
          let lib = Library.intern_from_file file in
          let () = Hashtbl.add hc dp lib in
          lib)
    else Vernacinterp.fs_intern dp

  let clear () = Hashtbl.clear hc
end

let intern = Intern.intern

(* Regular memo tables *)
module Stats = struct
  type t =
    { stats : Stats.t
    ; time_hash : float
          (** Time in hashing consumed in the original execution *)
    ; cache_hit : bool  (** Whether we had a cache hit *)
    }

  let make ~stats ?(cache_hit = false) ~time_hash () =
    (* This is quite slow, to the point it is not really usable, but a more
       precise option *)
    (* let memory = Obj.magic res |> Obj.reachable_words in *)
    { stats; time_hash; cache_hit }

  let zero =
    { stats = { Stats.time = 0.0; memory = 0.0 }
    ; time_hash = 0.0
    ; cache_hit = false
    }
end

module GlobalCacheStats = struct
  let nhit, ntotal = (ref 0, ref 0)

  let reset () =
    nhit := 0;
    ntotal := 0

  let hit () =
    incr nhit;
    incr ntotal

  let miss () = incr ntotal

  let stats () =
    if !ntotal = 0 then "no stats"
    else
      let hit_rate =
        Stdlib.Float.of_int !nhit /. Stdlib.Float.of_int !ntotal *. 100.0
      in
      Format.asprintf "cache hit rate: %3.2f" hit_rate
end

(* XXX: Move elsewhere *)
module Loc_utils : sig
  val adjust_offset :
       stm_loc:Coq.Loc_t.t
    -> cached_loc:Coq.Loc_t.t
    -> ('a, Coq.Loc_t.t) Coq.Protect.E.t
    -> ('a, Coq.Loc_t.t) Coq.Protect.E.t
end = struct
  let loc_offset (l1 : Coq.Loc_t.t) (l2 : Coq.Loc_t.t) =
    let line_offset = l2.line_nb - l1.line_nb in
    let bol_offset = l2.bol_pos - l1.bol_pos in
    let line_last_offset = l2.line_nb_last - l1.line_nb_last in
    let bol_last_offset = l2.bol_pos_last - l1.bol_pos_last in
    let bp_offset = l2.bp - l1.bp in
    let ep_offset = l2.ep - l1.ep in
    ( line_offset
    , bol_offset
    , line_last_offset
    , bol_last_offset
    , bp_offset
    , ep_offset )

  let loc_apply_offset
      ( line_offset
      , bol_offset
      , line_last_offset
      , bol_last_offset
      , bp_offset
      , ep_offset ) (loc : Coq.Loc_t.t) =
    { loc with
      line_nb = loc.line_nb + line_offset
    ; bol_pos = loc.bol_pos + bol_offset
    ; line_nb_last = loc.line_nb_last + line_last_offset
    ; bol_pos_last = loc.bol_pos_last + bol_last_offset
    ; bp = loc.bp + bp_offset
    ; ep = loc.ep + ep_offset
    }

  let adjust_offset ~stm_loc ~cached_loc res =
    let offset = loc_offset cached_loc stm_loc in
    let f = loc_apply_offset offset in
    Coq.Protect.E.map_loc ~f res
end

(* Results are stored with their output state replaced by a store id, so the
   caches themselves retain no Rocq states; a hit whose state has been evicted
   is simply a miss. *)
let store_res res = Coq.Protect.E.map ~f:States.register res

(* A stored result is usable only while the state it names is still in the
   store; [None] tells the caller to treat the entry as a miss. *)
let restore_res res =
  match res.Coq.Protect.E.r with
  | Coq.Protect.R.Completed (Ok id) ->
    States.get id
    |> Option.map (fun st -> Coq.Protect.E.map ~f:(fun _ -> st) res)
  | Coq.Protect.R.Completed (Error _) | Coq.Protect.R.Interrupted ->
    (* There is no id in these, so [map] cannot call [f]; it only moves the
       result over to the state type. *)
    Some (Coq.Protect.E.map ~f:(fun _ -> assert false) res)

(* The store id a result names, if any, so that an entry can be indexed on the
   state it produces as well as on the one it extends. *)
let res_id res =
  match res.Coq.Protect.E.r with
  | Coq.Protect.R.Completed (Ok id) -> Some id
  | Coq.Protect.R.Completed (Error _) | Coq.Protect.R.Interrupted -> None

module type EvalType = sig
  (** Input, as callers see it *)
  type t

  (** Cache key: the input with its Rocq state replaced by a store id *)
  type key

  val key : t -> key
  val key_equal : key -> key -> bool
  val key_hash : key -> int

  (** The state a key is relative to, so entries can be pruned on eviction *)
  val key_id : key -> States.id

  val name : string

  val eval :
    token:Coq.Limits.Token.t -> t -> (Coq.State.t, Coq.Loc_t.t) Coq.Protect.E.t

  val input_info : t -> string
end

(** Flèche memo / cache tables, with some advanced features *)
module type S = sig
  type input

  (** For now, to generalize later if needed *)
  type output

  (** [eval i] Eval an input [i] *)
  val eval :
    token:Coq.Limits.Token.t -> input -> (output, Coq.Loc_t.t) Coq.Protect.E.t

  (** [eval i] Eval an input [i] and produce stats *)
  val evalS :
       token:Coq.Limits.Token.t
    -> input
    -> (output, Coq.Loc_t.t) Coq.Protect.E.t * Stats.t

  (** [size ()] Return the cache size in words, expensive *)
  val size : unit -> int

  (** [freqs ()]: (sorted) histogram *)
  val all_freqs : unit -> int list

  (** [stats ()]: hashtbl stats *)
  val stats : unit -> Hashtbl.statistics

  (** debug data for input *)
  val input_info : input -> string

  (** clears the cache *)
  val clear : unit -> unit
end

(* The caches differ only in what they record beside a result, and in whether
   they are traced and counted in the global hit rate. *)
module type Payload = sig
  type input
  type t

  val make : input -> t

  (** Adapt a cached result to the input it is being reused for *)
  val repair :
       t (* of the input at hand *)
    -> t (* of the input the result was produced for *)
    -> (Coq.State.t, Coq.Loc_t.t) Coq.Protect.E.t
    -> (Coq.State.t, Coq.Loc_t.t) Coq.Protect.E.t

  val reported : bool
end

module Eval (E : EvalType) (P : Payload with type input = E.t) :
  S with type input = E.t and type output = Coq.State.t = struct
  type input = E.t
  type output = Coq.State.t

  module HC = Hashtbl.Make (struct
    type t = E.key

    let equal = E.key_equal
    let hash = E.key_hash
  end)

  module IM = Hashtbl.Make (struct
    type t = States.id

    let equal = States.equal
    let hash = States.hash
  end)

  type entry =
    { payload : P.t
    ; res : (States.id, Coq.Loc_t.t) Coq.Protect.E.t
    ; stats : CS.t
    ; mutable hits : int
    }

  let cache : entry HC.t = HC.create 1000

  (* The keys of the entries that mention a given state, either as the one they
     extend or as the one they produce, so that an eviction visits those entries
     alone instead of the whole cache.

     A key is a member exactly while its entry is cached, and every removal
     unlists it: were one to skip that, a key re-evaluated after an eviction
     would pile up one stale member per evaluation for as long as the process
     runs. Membership is a hashtable rather than a list because unlisting has to
     be cheap: many entries extend the same hot state, and scanning a list of
     keys compares ASTs, which would make a batched eviction quadratic exactly
     under the fan-out workloads the store exists for. *)
  let keys_by_state : unit HC.t IM.t = IM.create 1000

  (* This is very expensive *)
  let size () = Obj.reachable_words (Obj.magic cache)
  let input_info = E.input_info
  let stats () = HC.stats cache

  let clear () =
    HC.clear cache;
    IM.clear keys_by_state

  let all_freqs () =
    HC.fold (fun _ e acc -> e.hits :: acc) cache []
    |> List.sort (fun x y -> -Int.compare x y)

  (* The states an entry mentions: the one its key extends, and the one its
     result produces when that is a different state. *)
  let states_of key res =
    let key_id = E.key_id key in
    match res_id res with
    | Some out when not (States.equal out key_id) -> [ key_id; out ]
    | Some _ | None -> [ key_id ]

  let index key res =
    let list_at id =
      let keys =
        match IM.find_opt keys_by_state id with
        | None ->
          let keys = HC.create 4 in
          IM.replace keys_by_state id keys;
          keys
        | Some keys -> keys
      in
      HC.replace keys key ()
    in
    List.iter list_at (states_of key res)

  (* The one way out of the cache, so that an entry is never left listed under a
     state it no longer has an entry for. *)
  let remove key entry =
    let unlist_at id =
      match IM.find_opt keys_by_state id with
      | None -> ()
      | Some keys ->
        HC.remove keys key;
        if HC.length keys = 0 then IM.remove keys_by_state id
    in
    HC.remove cache key;
    List.iter unlist_at (states_of key entry.res)

  (* An entry mentioning an evicted state can never hit again *)
  let prune ids =
    let evict id =
      match IM.find_opt keys_by_state id with
      | None -> ()
      | Some keys ->
        (* Detached up front: [remove] unlists under both states an entry
           mentions, and this table is going away wholesale anyway, so its own
           binding must already be gone when [remove] looks for it. *)
        IM.remove keys_by_state id;
        let remove_cached key () =
          match HC.find_opt cache key with
          | None -> ()
          | Some entry -> remove key entry
        in
        HC.iter remove_cached keys
    in
    List.iter evict ids

  let () = States.on_evict prune

  (* Interrupted executions are not cached *)
  let add key entry =
    match entry.res.Coq.Protect.E.r with
    | Coq.Protect.R.Interrupted -> ()
    | _ ->
      index key entry.res;
      HC.replace cache key entry

  let in_cache i =
    let kind = CS.Kind.Hashing in
    let f i =
      let key = E.key i in
      (key, HC.find_opt cache key)
    in
    CS.record ~kind ~f i

  let miss ~token ~time_hash ~key ~payload i =
    if P.reported then (
      if Debug.cache then Io.Log.trace "memo" "cache miss";
      GlobalCacheStats.miss ());
    let kind = CS.Kind.Exec in
    let f i = E.eval ~token i in
    let res, stats = CS.record ~kind ~f i in
    let () = add key { payload; res = store_res res; stats; hits = 0 } in
    (res, Stats.make ~stats ~cache_hit:false ~time_hash ())

  let evalS ~token i =
    let payload = P.make i in
    match in_cache i with
    | (key, Some entry), { time = time_hash; memory = _ } -> (
      match restore_res entry.res with
      | Some res ->
        if P.reported then (
          if Debug.cache then Io.Log.trace "memo" "cache hit";
          GlobalCacheStats.hit ());
        entry.hits <- entry.hits + 1;
        let res = P.repair payload entry.payload res in
        (res, Stats.make ~stats:entry.stats ~cache_hit:true ~time_hash ())
      | None ->
        (* Not redundant with the [add] in [miss]: an interrupted re-execution
           is not cached, and the entry we know is stale has to go anyway. *)
        remove key entry;
        miss ~token ~time_hash ~key ~payload i)
    | (key, None), { time = time_hash; memory = _ } ->
      miss ~token ~time_hash ~key ~payload i

  let evalS ~token i =
    let name = "Memo." ^ E.name in
    NewProfile.profile name (fun () -> evalS ~token i) ()

  let eval ~token i = evalS ~token i |> fst
end

(* Caches with nothing to record beside the result *)
module SEval (E : EvalType) :
  S with type input = E.t and type output = Coq.State.t =
  Eval
    (E)
    (struct
      type input = E.t
      type t = unit

      let make _ = ()
      let repair () () res = res
      let reported = false
    end)

module type LocEvalType = sig
  include EvalType

  val loc_of_input : t -> Coq.Loc_t.t
end

(* Caches that record the location a result was produced at, so that it can be
   shifted to the one it is reused at *)
module CEval (E : LocEvalType) :
  S with type input = E.t and type output = Coq.State.t =
  Eval
    (E)
    (struct
      type input = E.t
      type t = Coq.Loc_t.t

      let make = E.loc_of_input

      let repair stm_loc cached_loc res =
        Loc_utils.adjust_offset ~stm_loc ~cached_loc res

      let reported = true
    end)

module VernacEval = struct
  let name = "Interp"

  type t = Coq.State.t * Coq.Ast.t
  type key = States.id * Coq.Ast.t

  let key (st, v) = (States.register st, v)
  let key_id (id, _) = id

  (* This crutially relies on our ppx to ignore the CAst location *)
  let key_equal (id1, v1) (id2, v2) =
    Coq.Ast.compare v1 v2 = 0 && States.equal id1 id2

  let key_hash (id, v) = Hashtbl.hash (Coq.Ast.hash v, States.hash id)
  let loc_of_input (_, stm) = Coq.Ast.loc stm |> Option.get

  let input_info (st, v) =
    Format.asprintf "stm: %d | st %d" (Coq.Ast.hash v) (Hashtbl.hash st)

  let eval ~token (st, stm) = Coq.Interp.interp ~token ~intern ~st stm
end

module Interp = CEval (VernacEval)

module RequireEval = struct
  let name = "Require"

  type t = Coq.State.t * Coq.Files.t * Coq.Ast.Require.t
  type key = States.id * Coq.Files.t * Coq.Ast.Require.t

  let key (st, f, r) = (States.register st, f, r)
  let key_id (id, _, _) = id

  (* This crutially relies on our ppx to ignore the CAst location *)
  let key_equal (id1, f1, r1) (id2, f2, r2) =
    Coq.Ast.Require.compare r1 r2 = 0
    && Coq.Files.compare f1 f2 = 0
    && States.equal id1 id2

  let key_hash (id, f, v) =
    Hashtbl.hash (Coq.Ast.Require.hash v, Coq.Files.hash f, States.hash id)

  let input_info (st, f, v) =
    Format.asprintf "stm: %d | st %d | f %d" (Coq.Ast.Require.hash v)
      (Hashtbl.hash st) (Coq.Files.hash f)

  let loc_of_input (_, _, stm) = Option.get stm.Coq.Ast.Require.loc

  let eval ~token (st, files, stm) =
    Coq.Interp.Require.interp ~token ~intern ~st files stm
end

module Require = CEval (RequireEval)

module AdmitEval = struct
  let name = "Admit"

  type t = Coq.State.t
  type key = States.id

  let key st = States.register st
  let key_id id = id
  let key_equal = States.equal
  let key_hash = States.hash
  let input_info st = Format.asprintf "st %d" (Hashtbl.hash st)
  let eval ~token st = Coq.State.admit ~token ~st
end

module Admit = SEval (AdmitEval)

module InitEval = struct
  let name = "Init"

  type t = Coq.State.t * Coq.Workspace.t * Coq.Files.t * Lang.LUri.File.t
  type key = States.id * Coq.Workspace.t * Coq.Files.t * Lang.LUri.File.t

  let key (st, w, f, uri) = (States.register st, w, f, uri)
  let key_id (id, _, _, _) = id

  let key_equal (i1, w1, f1, u1) (i2, w2, f2, u2) : bool =
    Lang.LUri.File.compare u1 u2 = 0
    && Coq.Workspace.compare w1 w2 = 0
    && Coq.Files.compare f1 f2 = 0
    && States.equal i1 i2

  let key_hash (id, w, f, uri) =
    Hashtbl.hash
      ( States.hash id
      , Coq.Workspace.hash w
      , Coq.Files.hash f
      , Lang.LUri.File.hash uri )

  let eval ~token (root_state, workspace, _files, uri) =
    Coq.Init.doc_init ~token ~intern ~root_state ~workspace ~uri

  let input_info (st, ws, files, file) =
    Format.asprintf "st %d | ws %d | fs: %a| file %s" (Hashtbl.hash st)
      (Hashtbl.hash ws) Coq.Files.pp files
      (Lang.LUri.File.to_string_file file)
end

module Init = SEval (InitEval)

let all_size () =
  Init.size () + Interp.size () + Require.size () + Admit.size ()
