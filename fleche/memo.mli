(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors           *)
(************************************************************************)
(* Flèche => document manager: Command Memoization                      *)
(************************************************************************)

module Stats : sig
  type t =
    { stats : Stats.t
    ; time_hash : float
          (** Time in hashing consumed in the original execution *)
    ; cache_hit : bool  (** Whether we had a cache hit *)
    }

  val zero : t
end

module Intern : sig
  val clear : unit -> unit
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

(** Document creation cache *)
module Init :
  S
    with type input =
      Coq.State.t * Coq.Workspace.t * Coq.Files.t * Lang.LUri.File.t
     and type output = Coq.State.t

(** Vernacular evaluation cache, invariant w.r.t. Coq's Ast locations, results
    are repaired. *)
module Interp :
  S with type input = Coq.State.t * Coq.Ast.t and type output = Coq.State.t

(** Require evaluation cache, also invariant w.r.t. locations inside
    [Coq.Ast.Require.t] *)
module Require :
  S
    with type input = Coq.State.t * Coq.Files.t * Coq.Ast.Require.t
     and type output = Coq.State.t

(** Admit evaluation cache *)
module Admit : S with type input = Coq.State.t and type output = Coq.State.t

(** [clear_all ()] empties every cache, drops the states nobody claims, and
    reports what the store is left holding. Documents keep theirs, as they did
    before the store existed. *)
val clear_all : unit -> States.stats

module GlobalCacheStats : sig
  val reset : unit -> unit

  (** Returns the hit ratio of the cache, etc... *)
  val stats : unit -> string
end

(** Size of all caches and of the states they name, very expensive *)
val all_size : unit -> int
