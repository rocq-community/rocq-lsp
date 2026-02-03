(*************************************************************************)
(* Copyright 2015-2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2019-2024 Inria           -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2024-2025 Emilio J. Gallego Arias  -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                     -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors            *)
(*************************************************************************)
(* Rocq Language Server Protocol: Rocq Goals API                         *)
(*************************************************************************)

let equal_option = Option.equal

type full_path = Libnames.full_path

let equal_full_path = Libnames.eq_full_path

module Reified_goal = struct
  type 'a hyp =
    { names : String.t List.t
    ; def : 'a option
    ; ty : 'a
    }
  [@@deriving equal]

  let map_hyp ~f { names; def; ty } =
    let def = Option.map f def in
    let ty = f ty in
    { names; def; ty }

  type info =
    { evar : Evar.t
    ; name : full_path option
    }
  [@@deriving equal]

  type 'a t =
    { info : info
    ; hyps : 'a hyp List.t
    ; ty : 'a
    }
  [@@deriving equal]

  let map ~f { info; ty; hyps } =
    let ty = f ty in
    let hyps = List.map (map_hyp ~f) hyps in
    { info; ty; hyps }
end

type ('a, 'pp) t =
  { goals : 'a List.t
  ; stack : ('a List.t * 'a List.t) List.t
  ; bullet : 'pp option
  ; shelf : 'a List.t
  ; given_up : 'a List.t
  }
[@@deriving equal]

let map ~f ~g { goals; stack; bullet; shelf; given_up } =
  let goals = List.map f goals in
  let stack = List.map (fun (s, r) -> (List.map f s, List.map f r)) stack in
  let bullet = Option.map g bullet in
  let shelf = List.map f shelf in
  let given_up = List.map f given_up in
  { goals; stack; bullet; shelf; given_up }

type ('goals, 'pp) reified = ('goals Reified_goal.t, 'pp) t

(** XXX: Do we need to perform evar normalization? *)
module CDC = Context.Compacted.Declaration

type cdcl = EConstr.compacted_declaration

let binder_name n = Context.binder_name n |> Names.Id.to_string

let to_tuple ppx : cdcl -> 'pc Reified_goal.hyp =
  let open CDC in
  function
  | LocalAssum (idl, tm) ->
    let names = List.map binder_name idl in
    { Reified_goal.names; def = None; ty = ppx tm }
  | LocalDef (idl, tdef, tm) ->
    let names = List.map binder_name idl in
    { names; def = Some (ppx tdef); ty = ppx tm }

(** gets a hypothesis *)
let get_hyp (ppx : EConstr.t -> 'pc) (_sigma : Evd.evar_map) (hdecl : cdcl) :
    'pc Reified_goal.hyp =
  to_tuple ppx hdecl

(** gets the constr associated to the type of the current goal *)
let get_goal_type (ppx : EConstr.t -> 'pc) (env : Environ.env)
    (sigma : Evd.evar_map) (g : Evar.t) : _ =
  let (EvarInfo evi) = Evd.find sigma g in
  let concl =
    match Evd.evar_body evi with
    | Evd.Evar_empty -> Evd.evar_concl evi
    | Evd.Evar_defined body -> Retyping.get_type_of env sigma body
  in
  ppx concl

let build_info sigma g =
  { Reified_goal.evar = g; name = Evd.evar_ident g sigma }

(** Generic processor *)
let process_goal_gen ~ppx ~compact sigma g : 'a Reified_goal.t =
  (* XXX This looks cumbersome *)
  let env = Global.env () in
  let (EvarInfo evi) = Evd.find sigma g in
  let env = Evd.evar_filtered_env env evi in
  (* why is compaction neccesary... ? [eg for better display] *)
  let ctx =
    if compact then
      Termops.compact_named_context sigma (EConstr.named_context env)
    else List.map CDC.of_named_decl (EConstr.named_context env)
  in
  let ppx = ppx env sigma in
  let hyps = List.map (get_hyp ppx sigma) ctx |> List.rev in
  let info = build_info sigma g in
  { info; ty = get_goal_type ppx env sigma g; hyps }

let if_not_empty (pp : Pp.t) =
  if Pp.(repr pp = Ppcmd_empty) then None else Some pp

let reify ~ppx ~compact lemmas =
  let lemmas = State.Proof.to_coq lemmas in
  let proof =
    Vernacstate.LemmaStack.with_top lemmas ~f:(fun pstate ->
        Declare.Proof.get pstate)
  in
  let { Proof.goals; stack; sigma; _ } = Proof.data proof in
  let ppx = List.map (process_goal_gen ~ppx ~compact sigma) in
  { goals = ppx goals
  ; stack = List.map (fun (g1, g2) -> (ppx g1, ppx g2)) stack
  ; bullet = if_not_empty @@ Proof_bullet.suggest proof
  ; shelf = Evd.shelf sigma |> ppx
  ; given_up = Evd.given_up sigma |> Evar.Set.elements |> ppx
  }

module Equality = struct
  let eq_constr (_env1, evd1, c1) (_env2, evd2, c2) =
    (* XXX Fixme, can be much faster using the advance compare functions *)
    let c1 = EConstr.to_constr evd1 c1 in
    let c2 = EConstr.to_constr evd2 c2 in
    Constr.equal c1 c2

  let eq_pp pp1 pp2 = pp1 = pp2
  let eq_rgoal = Reified_goal.equal eq_constr

  let equal_goals st1 st2 =
    let ppx env evd c = (env, evd, c) in
    let compact = false in
    let g1 = reify ~ppx ~compact st1 in
    let g2 = reify ~ppx ~compact st2 in
    equal eq_rgoal eq_pp g1 g2
end
