(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors          *)
(************************************************************************)
(* Flèche => document manager: Rocq state store                         *)
(************************************************************************)

(** The single owner of Rocq states.

    Every [Coq.State.t] that outlives the call producing it lives here, and
    everything else (memo tables, protocol handles) refers to it by [id]. That
    makes retention a property of one table instead of an emergent property of
    several, and eviction a policy decision taken in one place.

    Holders that need a state to stay alive declare it with {!retain}; caches
    hold ids {e without} retaining, so an evicted id degrades to a cache miss.
    Dropping a state is never a correctness problem: states are immutable
    values, so anyone still holding one keeps it alive, and the worst case is
    recomputation.

    States nobody retains are kept as a cache, in bounded number: past
    [Config.v.state_cache_size] of them, the least recently used are dropped.
    Retained states are never dropped, so a protocol handle cannot dangle and a
    document's chain cannot be evicted under it. *)

type id

val equal : id -> id -> bool
val hash : id -> int

(** Wire representation; [of_int] yields [None] for an unknown or evicted id. *)

val to_int : id -> int
val of_int : int -> id option

(** [register st] is the id of [st], allocating one if needed. Idempotent on
    physically equal states, best effort: the index is keyed on
    [Coq.State.hash], which can move as Rocq mutates what a state reaches, and a
    moved hash means a fresh id for the same state. The stale id is not retained
    by this and ages out of the cache like any other. Does {e not} retain. *)
val register : Coq.State.t -> id

(** [get id] is [None] if [id] was evicted; callers must recompute. A hit counts
    as a use for the cache's eviction order. *)
val get : id -> Coq.State.t option

val is_live : id -> bool

(** Who is keeping a state alive.

    [Root] and [Client] are claims rather than counts: taking one twice is the
    same as taking it once, and a single {!release} gives it up. For [Client]
    that is what lets the same id be handed to a client repeatedly and freed
    once. [Doc] is counted, as several documents may share a state.

    A [Client] claim is per-state, not per-connection, so the store assumes a
    single client: were two of them to hold the same handle, one freeing it
    would drop the claim for both. [Root] is in practice permanent, as root
    states are built once per workspace; nothing releases one today. *)
type owner =
  | Root  (** initial / root states, never evicted *)
  | Doc  (** part of a checked document, see {!Doc_own} *)
  | Client  (** a protocol handle, released explicitly *)

val retain : id -> owner -> unit

(** [release id owner] gives up [owner]'s claim on [id] and tells whether there
    was one to give up; releasing a claim nobody took is a no-op returning
    [false]. A state left with no claims stays cached until the cache bound or a
    {!gc} drops it. *)
val release : id -> owner -> bool

type stats =
  { live : int  (** states in the store *)
  ; roots : int
  ; docs : int
  ; clients : int
  ; unowned : int  (** retained by nobody, i.e. cache *)
  }

val stats : unit -> stats

(** [gc ()] drops every state no owner retains, then reports. The bounded cache
    makes calling this optional; it remains the manual way to give memory back
    right now. *)
val gc : unit -> stats

(** [on_evict f] registers [f] to run on evicted ids, whether the cache bound or
    a {!gc} dropped them, so that caches keyed on ids can prune themselves. *)
val on_evict : (id list -> unit) -> unit

(** [size ()] is [Obj.reachable_words] over the store, expensive. *)
val size : unit -> int
