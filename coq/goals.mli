(*************************************************************************)
(* Copyright 2015-2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2019-2024 Inria           -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2024-2025 Emilio J. Gallego Arias  -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                     -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors            *)
(*************************************************************************)
(* Rocq Language Server Protocol: Rocq Goals API                         *)
(*************************************************************************)

module Reified_goal : sig
  type 'a hyp =
    { names : string list
    ; def : 'a option
    ; ty : 'a
    }

  type info =
    { evar : Evar.t
    ; name : Libnames.full_path option
    }

  type 'a t =
    { info : info
    ; hyps : 'a hyp list
    ; ty : 'a
    }

  val map : f:('a -> 'b) -> 'a t -> 'b t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

type ('a, 'pp) t =
  { goals : 'a list
  ; stack : ('a list * 'a list) list
  ; bullet : 'pp option
  ; shelf : 'a list
  ; given_up : 'a list
  }

val equal :
     ('a -> 'a -> bool)
  -> ('pp -> 'pp -> bool)
  -> ('a, 'pp) t
  -> ('a, 'pp) t
  -> bool

val map : f:('a -> 'b) -> g:('pp -> 'pp') -> ('a, 'pp) t -> ('b, 'pp') t

type ('goals, 'pp) reified = ('goals Reified_goal.t, 'pp) t

(** Goal processing, [compact] will determine whether we group the hypotheses *)
val reify :
     ppx:(Environ.env -> Evd.evar_map -> EConstr.t -> 'goals)
  -> compact:bool
  -> State.Proof.t
  -> ('goals, Pp.t) reified

(* equality functions with heuristics *)
module Equality : sig
  (** Goal-based eq heuristic, will return [true] when goals are "equal", in a
      proof search sense *)
  val equal_goals : State.Proof.t -> State.Proof.t -> bool
end
