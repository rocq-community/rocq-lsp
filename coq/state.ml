(*************************************************************************)
(* Copyright 2015-2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2019-2024 Inria           -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2024-2025 Emilio J. Gallego Arias  -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                     -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors            *)
(*************************************************************************)
(* Rocq Language Server Protocol: Rocq State API                         *)
(*************************************************************************)

type t = Vernacstate.t

(* EJGA: This requires patches to Coq, they are in the lsp_debug branch

   let any_out oc (a : Summary.Frozen.any) = (* let (Summary.Frozen.Any (tag,
   _value)) = a in *) (* let name = Summary.Dyn.repr tag in *) (*
   Lsp.Io.log_error "marshall" name; *) Marshal.to_channel oc a []

   let _frozen_out oc (s : Summary.Frozen.t) = Summary.Frozen.iter (any_out oc)
   s

   let summary_out oc (s : Summary.frozen) = let { Summary.summaries; ml_module
   } = s in (* frozen_out oc summaries; *) Marshal.to_channel oc summaries [];
   Marshal.to_channel oc ml_module []; ()

   let summary_in ic : Summary.frozen = let summaries = Marshal.from_channel ic
   in let ml_module = Marshal.from_channel ic in { Summary.summaries; ml_module
   }

   let system_out oc ((l : Lib.frozen), (s : Summary.frozen)) = (* Both parts of
   system have functional values !! Likely due to Lib.frozen having a
   Summary.frozen inside? *) Marshal.to_channel oc l [ Closures ]; summary_out
   oc s; ()

   let system_in ic : Vernacstate.System.t = let l : Lib.frozen =
   Marshal.from_channel ic in let s : Summary.frozen = summary_in ic in (l, s)

   let _marshal_out oc st = let { Vernacstate.parsing; system; lemmas; program;
   opaques; shallow } = st in Marshal.to_channel oc parsing []; system_out oc
   system; (* lemmas doesn't !! *) Marshal.to_channel oc lemmas [];
   Marshal.to_channel oc program []; Marshal.to_channel oc opaques [];
   Marshal.to_channel oc shallow []; ()

   let _marshal_in ic = let parsing = Marshal.from_channel ic in let system =
   system_in ic in let lemmas = Marshal.from_channel ic in let program =
   Marshal.from_channel ic in let opaques = Marshal.from_channel ic in let
   shallow = Marshal.from_channel ic in { Vernacstate.parsing; system; lemmas;
   program; opaques; shallow } *)

let marshal_in ic : t = Marshal.from_channel ic
let marshal_out oc st = Marshal.to_channel oc st []
let of_coq x = x
let to_coq x = x

(* let compare x y = compare x y *)
let compare (x : t) (y : t) =
  let open Vernacstate in
  let { synterp = ss1
      ; interp = { system = is1; lemmas = l1; program = g1; opaques = o1; captured_output = co1 }
      } =
    x
  in
  let { synterp = ss2
      ; interp = { system = is2; lemmas = l2; program = g2; opaques = o2; captured_output = co2 }
      } =
    y
  in
  if ss1 == ss2 && is1 == is2 && l1 == l2 && g1 == g2 && o1 == o2 && co1 == co2 then 0 else 1

let equal x y = compare x y = 0

let hash x =
  (* OCaml's defaults are 10, 100, but not so good for us, much improved
     settings are below (best try so far) *)
  let meaningful, total = (256, 256) in
  Hashtbl.hash_param meaningful total x

(* XXX: This should restore the state properly before calling
   get_default_proof_mode, as of now promotes buggy use. *)
let mode ~st =
  Option.map
    (fun _ -> Synterp.get_default_proof_mode ())
    st.Vernacstate.interp.lemmas

let parsing ~st = Vernacstate.(Synterp.parsing st.synterp)

module Proof_ = Proof

module Proof = struct
  type t = Vernacstate.LemmaStack.t

  let to_coq x = x
  let equal x y = x == y

  (* OCaml's defaults are 10, 100, we use these values as to give better
     precision for petanque-like users, it should not impact interactive use but
     we gotta measure it *)
  let hash x =
    let meaningful, total = (128, 256) in
    Hashtbl.hash_param meaningful total x

  let name (pst : t) =
    Vernacstate.LemmaStack.get_top pst
    |> Declare.Proof.get_name |> Names.Id.to_string

  let pp_st ~token env sigma
      ( (_ctx : Environ.named_context_val)
      , (_term : EConstr.constr)
      , (typ : EConstr.types) ) =
    let goal_concl_style = true in
    let f t =
      Printer.pr_letype_env ~goal_concl_style env sigma t |> Pp.string_of_ppcmds
    in
    Protect.eval ~token ~f typ

  let statements ~token (pst : t) =
    let pf = Vernacstate.LemmaStack.get_top pst |> Declare.Proof.get in
    let { Proof.sigma; entry; _ } = Proof.data pf in
    let _, env = Proof.get_proof_context pf in
    let sl = Proofview.initial_goals entry in
    Protect.E.mapM ~f:(pp_st ~token env sigma) sl

  module Program = struct
    module Obl = Declare.OblState.View.Obl

    type t = Declare.OblState.View.t = private
      { opaque : bool
      ; remaining : int
      ; obligations : Obl.t array
      }
  end
end

let lemmas ~st = st.Vernacstate.interp.lemmas

let program ~st =
  NeList.head st.Vernacstate.interp.program |> Declare.OblState.view

let drop_proof ~st =
  let open Vernacstate in
  let interp =
    { st.interp with
      lemmas =
        Option.cata
          (fun s -> snd @@ Vernacstate.LemmaStack.pop s)
          None st.interp.lemmas
    }
  in
  { st with interp }

let drop_all_proofs ~st =
  let open Vernacstate in
  let interp = { st.interp with lemmas = None } in
  { st with interp }

let in_state ~token ~st ~f a =
  let f a =
    Vernacstate.unfreeze_full_state st;
    f a
  in
  Protect.eval ~token ~f a

let in_stateM ~token ~st ~f a =
  let open Protect.E.O in
  let* () = Protect.eval ~token ~f:Vernacstate.unfreeze_full_state st in
  f a

let admit ~st () =
  let () = Vernacstate.unfreeze_full_state st in
  match st.Vernacstate.interp.lemmas with
  | None -> st
  | Some lemmas ->
    let pm = NeList.head st.Vernacstate.interp.program in
    let proof, lemmas = Vernacstate.(LemmaStack.pop lemmas) in
    let pm = Declare.Proof.save_admitted ~pm ~proof in
    let program = NeList.map_head (fun _ -> pm) st.Vernacstate.interp.program in
    let st = Vernacstate.freeze_full_state () in
    { st with interp = { st.interp with lemmas; program } }

let admit ~token ~st = Protect.eval ~token ~f:(admit ~st) ()

let admit_goal ~st () =
  let () = Vernacstate.unfreeze_full_state st in
  match st.Vernacstate.interp.lemmas with
  | None -> st
  | Some lemmas ->
    let f pf = Declare.Proof.by (Global.env ()) Proofview.give_up pf |> fst in
    let lemmas = Some (Vernacstate.LemmaStack.map_top ~f lemmas) in
    { st with interp = { st.interp with lemmas } }

let admit_goal ~token ~st = Protect.eval ~token ~f:(admit_goal ~st) ()

let count_edges univ =
  let univ = UGraph.repr univ in
  Univ.Level.Map.fold
    (fun _ node acc ->
      acc
      +
      match node with
      | UGraph.Alias _ -> 1
      | Node m -> Univ.Level.Map.cardinal m)
    univ
    (Univ.Level.Map.cardinal univ)

let info_universes ~token ~st =
  let open Protect.E.O in
  let+ univ = in_state ~token ~st ~f:Global.universes () in
  let univs = UGraph.domain univ in
  let nuniv = Univ.Level.Set.cardinal univs in
  let nconst = count_edges univ in
  (nuniv, nconst)
