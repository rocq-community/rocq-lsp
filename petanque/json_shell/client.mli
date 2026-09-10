(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors          *)
(************************************************************************)
(* Flèche => RL agent: petanque                                         *)
(************************************************************************)

open Petanque_json

module type Chans = sig
  val ic : in_channel
  val oc : Format.formatter
  val trace : ?verbose:string -> string -> unit
  val message : lvl:int -> message:string -> unit
end

open Protocol
open Protocol_shell

module S (C : Chans) : sig
  val set_workspace :
    SetWorkspace.Params.t -> (SetWorkspace.Response.t, string) result

  val toc :
    TableOfContents.Params.t -> (TableOfContents.Response.t, string) result

  val get_root_state :
    GetRootState.Params.t -> (GetRootState.Response.t, string) result

  val get_state_at_pos :
    GetStateAtPos.Params.t -> (GetStateAtPos.Response.t, string) result

  val start : Start.Params.t -> (Start.Response.t, string) result
  val run : RunTac.Params.t -> (RunTac.Response.t, string) result
  val run_at_pos : RunAtPoint.Params.t -> (RunAtPoint.Response.t, string) result
  val goals : Goals.Params.t -> (Goals.Response.t, string) result
  val premises : Premises.Params.t -> (Premises.Response.t, string) result

  val state_equal :
    StateEqual.Params.t -> (StateEqual.Response.t, string) result

  val state_hash : StateHash.Params.t -> (StateHash.Response.t, string) result

  val state_proof_equal :
    StateProofEqual.Params.t -> (StateProofEqual.Response.t, string) result

  val state_proof_hash :
    StateProofHash.Params.t -> (StateProofHash.Response.t, string) result

  val state_free : StateFree.Params.t -> (StateFree.Response.t, string) result

  val state_stats :
    StateStats.Params.t -> (StateStats.Response.t, string) result

  val cache_trim : CacheTrim.Params.t -> (CacheTrim.Response.t, string) result

  val list_notations_in_statement :
    ListNotations.Params.t -> (ListNotations.Response.t, string) result

  val ast : PetAst.Params.t -> (PetAst.Response.t, string) result
  val ast_at_pos : AstAtPos.Params.t -> (AstAtPos.Response.t, string) result
  val proof_info : ProofInfo.Params.t -> (ProofInfo.Response.t, string) result

  val proof_info_at_pos :
    ProofInfoAtPos.Params.t -> (ProofInfoAtPos.Response.t, string) result
end
