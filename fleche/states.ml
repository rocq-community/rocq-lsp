(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors          *)
(************************************************************************)
(* Flèche => document manager: Rocq state store                         *)
(************************************************************************)

type id = int

let equal (x : id) (y : id) = Int.equal x y
let hash (x : id) = Hashtbl.hash x
let to_int (x : id) = x

type owner =
  | Root
  | Doc
  | Client

type entry =
  { state : Coq.State.t
  ; hash : int  (** [Coq.State.hash state] as of registration, see [by_state] *)
  ; mutable rooted : bool
  ; mutable docs : int
  ; mutable claimed : bool
  ; mutable used : int  (** last use, for the cache's eviction order *)
  }

type stats =
  { live : int
  ; roots : int
  ; docs : int
  ; clients : int
  ; unowned : int
  }

(* Index by state so [register] is idempotent on physically equal states.
   [Coq.State.hash] is structural over a value holding refs and lazies, so a
   state's hash moves as Rocq mutates what it reaches; a [Hashtbl.Make
   (Coq.State)] would then look for a binding in a bucket it was never put in,
   which both strands the binding (and with it the state it keys, forever) and
   risks hitting a different, live id for the same state. So we bucket by hand
   on the hash taken at registration, kept in the entry: insertion and removal
   always agree on where a binding lives. [Coq.State.equal] (physical) settles
   collisions inside a bucket. *)
let by_id : (id, entry) Hashtbl.t = Hashtbl.create 1000
let by_state : (int, (Coq.State.t * id) list) Hashtbl.t = Hashtbl.create 1000
let last_id = ref 0
let evict_hooks : (id list -> unit) list ref = ref []
let on_evict f = evict_hooks := f :: !evict_hooks
let is_live id = Hashtbl.mem by_id id
let of_int id = if is_live id then Some id else None

(* How many states no owner retains we keep around as a cache, and the clock
   ordering their eviction. The count is maintained incrementally as claims come
   and go, so the hot paths pay a comparison, not a fold. *)
let clock = ref 0
let unowned_count = ref 0

let unowned ({ rooted; docs; claimed; _ } : entry) =
  (not rooted) && docs = 0 && not claimed

let touch entry =
  incr clock;
  entry.used <- !clock

let bucket h =
  match Hashtbl.find_opt by_state h with
  | None -> []
  | Some bindings -> bindings

let rec find_binding state = function
  | [] -> None
  | (st, id) :: tl ->
    if Coq.State.equal st state then Some id else find_binding state tl

(* Unbind by id rather than by state: two ids can name the same state if its
   hash moved between two [register] calls, and dropping one must not take the
   other's binding with it. *)
let unbind id entry =
  let rec remove = function
    | [] -> []
    | ((_, id') as binding) :: tl ->
      if equal id id' then tl else binding :: remove tl
  in
  match remove (bucket entry.hash) with
  | [] -> Hashtbl.remove by_state entry.hash
  | bindings -> Hashtbl.replace by_state entry.hash bindings

(* Only unowned entries are ever evicted *)
let evict id entry =
  Hashtbl.remove by_id id;
  unbind id entry;
  decr unowned_count

let notify_evicted = function
  | [] -> ()
  | _ :: _ as ids -> List.iter (fun hook -> hook ids) !evict_hooks

(* At least 1 so that a state is never evicted between its registration and the
   retain that usually follows: the newest state survives any batch. *)
let cache_size () = max 1 !Config.v.state_cache_size

(* Batched: evicting down to a fraction of the bound means a store sitting at
   the bound pays for a sweep once per batch of insertions, not per
   insertion. *)
let enforce_bound () =
  let bound = cache_size () in
  if !unowned_count > bound then (
    let target = max 1 (bound * 3 / 4) in
    let excess = !unowned_count - target in
    let cached =
      Hashtbl.fold
        (fun id entry acc ->
          if unowned entry then (entry.used, id, entry) :: acc else acc)
        by_id []
    in
    let oldest_first =
      List.sort (fun (u1, _, _) (u2, _, _) -> Int.compare u1 u2) cached
    in
    let rec take n = function
      | x :: tl when n > 0 -> x :: take (n - 1) tl
      | _ -> []
    in
    let dead = take excess oldest_first in
    List.iter (fun (_, id, entry) -> evict id entry) dead;
    notify_evicted (List.map (fun (_, id, _) -> id) dead))

let fresh state h =
  incr last_id;
  let id = !last_id in
  let entry =
    { state; hash = h; rooted = false; docs = 0; claimed = false; used = 0 }
  in
  touch entry;
  Hashtbl.replace by_id id entry;
  Hashtbl.replace by_state h ((state, id) :: bucket h);
  incr unowned_count;
  enforce_bound ();
  id

let register state =
  let h = Coq.State.hash state in
  match find_binding state (bucket h) with
  | Some id ->
    Option.iter touch (Hashtbl.find_opt by_id id);
    id
  | None -> fresh state h

let get id =
  match Hashtbl.find_opt by_id id with
  | Some entry ->
    touch entry;
    Some entry.state
  | None -> None

(* Keep [unowned_count] in step across a claim change *)
let counted entry f =
  let before = unowned entry in
  f ();
  let after = unowned entry in
  if before && not after then decr unowned_count
  else if (not before) && after then incr unowned_count

let retain id owner =
  match Hashtbl.find_opt by_id id with
  | None -> ()
  | Some entry ->
    counted entry (fun () ->
        match owner with
        | Root -> entry.rooted <- true
        | Doc -> entry.docs <- entry.docs + 1
        | Client -> entry.claimed <- true)

let release id owner =
  match Hashtbl.find_opt by_id id with
  | None -> false
  | Some entry ->
    let held = ref false in
    counted entry (fun () ->
        match owner with
        | Root ->
          held := entry.rooted;
          entry.rooted <- false
        | Doc ->
          held := entry.docs > 0;
          entry.docs <- max 0 (entry.docs - 1)
        | Client ->
          held := entry.claimed;
          entry.claimed <- false);
    enforce_bound ();
    !held

let stats () =
  let live = ref 0
  and roots = ref 0
  and docs = ref 0
  and clients = ref 0
  and free = ref 0 in
  let count _id (entry : entry) =
    incr live;
    if entry.rooted then incr roots;
    if entry.docs > 0 then incr docs;
    if entry.claimed then incr clients;
    if unowned entry then incr free
  in
  Hashtbl.iter count by_id;
  { live = !live
  ; roots = !roots
  ; docs = !docs
  ; clients = !clients
  ; unowned = !free
  }

(* Gather before dropping: [Hashtbl.fold] does not say what it does to an entry
   removed under it. *)
let collect () =
  let dead =
    Hashtbl.fold
      (fun id entry acc -> if unowned entry then (id, entry) :: acc else acc)
      by_id []
  in
  List.iter (fun (id, entry) -> evict id entry) dead;
  notify_evicted (List.map fst dead)

let gc () =
  collect ();
  stats ()

let size () = Obj.reachable_words (Obj.magic by_id)
