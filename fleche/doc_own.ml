(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors          *)
(************************************************************************)
(* Flèche => document manager: what states a document keeps alive       *)
(************************************************************************)

module UM = Hashtbl.Make (struct
  type t = Lang.LUri.File.t

  let equal = Lang.LUri.File.equal
  let hash = Lang.LUri.File.hash
end)

(* How many documents keep a claim on their states. Past this the least recently
   used document releases its claims; the states then age out of the store's
   cache rather than vanishing, so coming back to the file stays cheap until
   memory pressure says otherwise. *)
let max_documents = 32

(* Each owned state next to the id it was registered under, so that a later
   check can recognise its own states without hashing them, and release exactly
   what it registered. *)
let owned : (Coq.State.t * States.id) list UM.t = UM.create 39

(* Most recently used first *)
let order : Lang.LUri.File.t list ref = ref []
let without uri = Lang.Compat.List.remove Lang.LUri.File.equal uri !order

let release_pairs pairs =
  List.iter (fun (_, id) -> ignore (States.release id States.Doc : bool)) pairs

let forget uri =
  match UM.find_opt owned uri with
  | None -> ()
  | Some pairs ->
    release_pairs pairs;
    UM.remove owned uri

let remove uri =
  forget uri;
  order := without uri

(* The limit is not configurable, so say when it bites: a cold re-check with no
   edit to explain it is otherwise indistinguishable from a cache bug. *)
let drop uri =
  Io.Log.trace "states" "over %d documents, releasing the states of %a"
    max_documents Lang.LUri.File.pp uri;
  forget uri

(* Make [uri] the most recently used, releasing what that pushes past the
   limit. *)
let bump uri =
  let rec split n = function
    | [] -> ([], [])
    | l when n = 0 -> ([], l)
    | u :: tl ->
      let keep, dropped = split (n - 1) tl in
      (u :: keep, dropped)
  in
  let keep, dropped = split max_documents (uri :: without uri) in
  order := keep;
  List.iter drop dropped

(* A check keeps the nodes before the change point, so the previous version's
   list and the new one share a physical prefix; recognising it by pointer
   comparison reuses those ids and claims without hashing a single state. Only
   past the first difference is there anything to do, and there each state is
   claimed the moment it is registered: registering the whole suffix first would
   leave the early ids unclaimed while the later registrations run the cache
   bound, which could evict them, and a retain after the fact is a silent no-op
   on a dead id. The suffix is claimed before the old tail is released, so a
   state both versions reach never drops to zero claims in between. *)
let rec diff prev states =
  match (prev, states) with
  | ((pst, _) as hd) :: ptl, st :: stl when Coq.State.equal pst st ->
    let now, released = diff ptl stl in
    (hd :: now, released)
  | prev, states ->
    let claim st =
      let id = States.register st in
      States.retain id States.Doc;
      (st, id)
    in
    (List.map claim states, prev)

let set uri states =
  let prev =
    match UM.find_opt owned uri with
    | None -> []
    | Some pairs -> pairs
  in
  let now, released = diff prev states in
  UM.replace owned uri now;
  release_pairs released;
  bump uri
