(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(************************************************************************)
(* SerAPI: Coq interaction protocol with bidirectional serialization    *)
(************************************************************************)
(* Copyright 2016-2019 MINES ParisTech -- License LGPL 2.1+             *)
(* Copyright 2019-2023 Inria           -- License LGPL 2.1+             *)
(* Written by: Emilio J. Gallego Arias and others                       *)
(************************************************************************)

(* Experimental: Analysis part *)

let tacdef_names (t : Ltac_plugin.Tacexpr.tacdef_body) =
  match t with
  | TacticDefinition (lid, _) ->
    [CAst.map (fun id -> Names.Name id) lid]
  | TacticRedefinition (qid, _) ->
    let id = Names.Name (Libnames.string_of_qualid qid |> Names.Id.of_string_soft) in
    [CAst.map (fun _ -> id) qid]

let tacdef_names = {
    Serlib.Ser_analysis.NameAnalysis.raw = tacdef_names
  ; glb = (fun _ -> [])
  ; top = (fun _ -> [])
}

let () =
  Serlib.Ser_analysis.NameAnalysis.register Ltac_plugin.G_ltac.wit_ltac_tacdef_body tacdef_names;
  ()

let tacnot_names (t : (string * string option) Ltac_plugin.Tacentries.grammar_tactic_prod_item_expr) = match t with
  | Ltac_plugin.Pptactic.TacTerm s -> [ CAst.make (Names.(Name (Id.of_string_soft s))) ]
  | Ltac_plugin.Pptactic.TacNonTerm _ -> []

let tacnot_names = {
    Serlib.Ser_analysis.NameAnalysis.raw = tacnot_names
  ; glb = (fun _ -> [])
  ; top = (fun _ -> [])
}

let () =
  Serlib.Ser_analysis.NameAnalysis.register Ltac_plugin.G_ltac.wit_ltac_production_item tacnot_names;
  ()


(* Serialization part *)

open Sexplib
open Sexplib.Std
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

let hash_fold_array = hash_fold_array_frozen

open Serlib

module C = CAst

module Loc       = Ser_loc
module CAst      = Ser_cAst
module Names     = Ser_names
module Locus     = Ser_locus
module Constr    = Ser_constr
module EConstr   = Ser_eConstr
module Constr_matching = Ser_constr_matching
module Libnames   = Ser_libnames
module Namegen    = Ser_namegen
module Genarg     = Ser_genarg
module Stdarg     = Ser_stdarg
module Genredexpr = Ser_genredexpr
module Redexpr    = Ser_redexpr
module Genintern  = Ser_genintern
module Goal_select = Ser_goal_select
module Pattern    = Ser_pattern
module Constrexpr = Ser_constrexpr
module Vernacexpr = Ser_vernacexpr
module Tactypes   = Ser_tactypes
module Tactics    = Ser_tactics
module Equality   = Ser_equality
module Inv        = Ser_inv
module Evaluable  = Ser_evaluable

module Ltac_plugin = struct
  module Tacexpr = Ltac_plugin.Tacexpr
  module RewriteStratAst = Ser_rewriteStratAst
end

type direction_flag =
  [%import: Ltac_plugin.Tacexpr.direction_flag]
  [@@deriving sexp,yojson,hash,compare]

type lazy_flag =
  [%import: Ltac_plugin.Tacexpr.lazy_flag]
  [@@deriving sexp,yojson,hash,compare]

type global_flag =
  [%import: Ltac_plugin.Tacexpr.global_flag]
  [@@deriving sexp,yojson,hash,compare]

type evars_flag =
  [%import: Ltac_plugin.Tacexpr.evars_flag]
  [@@deriving sexp,yojson,hash,compare]

type rec_flag =
  [%import: Ltac_plugin.Tacexpr.rec_flag]
  [@@deriving sexp,yojson,hash,compare]

type advanced_flag =
  [%import: Ltac_plugin.Tacexpr.advanced_flag]
  [@@deriving sexp,yojson,hash,compare]

type letin_flag =
  [%import: Ltac_plugin.Tacexpr.letin_flag]
  [@@deriving sexp,yojson,hash,compare]

type clear_flag =
  [%import: Ltac_plugin.Tacexpr.clear_flag]
  [@@deriving sexp,yojson,hash,compare]

type check_flag =
  [%import: Ltac_plugin.Tacexpr.check_flag]
  [@@deriving sexp,yojson,hash,compare]

type ltac_constant =
  [%import: Ltac_plugin.Tacexpr.ltac_constant]
  [@@deriving sexp,yojson,hash,compare]

type ('c,'d,'id) inversion_strength =
  [%import: ('c,'d,'id) Ltac_plugin.Tacexpr.inversion_strength]
  [@@deriving sexp,yojson,hash,compare]

type 'id message_token =
  [%import: ('id) Ltac_plugin.Tacexpr.message_token]
  [@@deriving sexp,yojson,hash,compare]

type ('dconstr,'id) induction_clause =
  [%import: ('dconstr,'id) Ltac_plugin.Tacexpr.induction_clause]
  [@@deriving sexp,yojson,hash,compare]

type ('constr,'dconstr,'id) induction_clause_list =
  [%import: ('constr,'dconstr,'id) Ltac_plugin.Tacexpr.induction_clause_list]
  [@@deriving sexp,yojson,hash,compare]

type 'a with_bindings_arg =
  [%import: 'a Ltac_plugin.Tacexpr.with_bindings_arg]
  [@@deriving sexp,yojson,hash,compare]

type 'a match_pattern =
  [%import: 'a Ltac_plugin.Tacexpr.match_pattern]
  [@@deriving sexp,yojson,hash,compare]

type 'a match_context_hyps =
  [%import: 'a Ltac_plugin.Tacexpr.match_context_hyps]
  [@@deriving sexp,yojson,hash,compare]

type ('a,'t) match_rule =
  [%import: ('a, 't) Ltac_plugin.Tacexpr.match_rule]
  [@@deriving sexp,yojson,hash,compare]

type ml_tactic_name =
  [%import: Ltac_plugin.Tacexpr.ml_tactic_name]
  [@@deriving sexp,yojson,hash,compare]

type ml_tactic_entry =
  [%import: Ltac_plugin.Tacexpr.ml_tactic_entry]
  [@@deriving sexp,yojson,hash,compare]

type ('a, 'b, 'c, 'd, 'e) may_eval = [%import: ('a,'b,'c,'d,'e) Ltac_plugin.Tacexpr.may_eval]
  [@@deriving sexp,yojson,hash,compare]

(* type dyn = Ser_Dyn [@@deriving sexp] *)
(* let to_dyn _   = Ser_Dyn *)
(* let from_dyn _ = fst (Dyn.create "dyn_tac") 0 *)

(* This is beyond import and sexp for the moment, see:
 * https://github.com/janestreet/ppx_sexp_conv/issues/6
 *)
(* We thus iso-project the tactic definition in a virtually identical copy (but for the Dyn) *)
module ITac = struct

[@@@ocaml.warning "-27"]
type ('trm, 'dtrm, 'pat, 'redpat, 'cst, 'ref, 'nam, 'occvar, 'tacexpr, 'lev) gen_atomic_tactic_expr =
  | TacIntroPattern of evars_flag * 'dtrm Tactypes.intro_pattern_expr CAst.t list
  | TacApply of advanced_flag * evars_flag * 'trm with_bindings_arg list *
      ('nam * 'dtrm Tactypes.intro_pattern_expr CAst.t option) list
  | TacElim of evars_flag * 'trm with_bindings_arg * 'trm Tactypes.with_bindings option
  | TacCase of evars_flag * 'trm with_bindings_arg
  | TacMutualFix of Names.Id.t * int * (Names.Id.t * int * 'trm) list
  | TacMutualCofix of Names.Id.t * (Names.Id.t * 'trm) list
  | TacAssert of
      evars_flag * bool * 'tacexpr option option *
      'dtrm Tactypes.intro_pattern_expr CAst.t option * 'trm
  | TacGeneralize of (('occvar Locus.occurrences_gen * 'trm) * Names.Name.t) list
  | TacLetTac of evars_flag * Names.Name.t * 'trm * 'nam Locus.clause_expr * letin_flag *
      Namegen.intro_pattern_naming_expr CAst.t option
  | TacInductionDestruct of
      rec_flag * evars_flag * ('trm,'dtrm,'nam) induction_clause_list
  | TacReduce of ('trm,'cst,'redpat,'occvar,'lev Redexpr.user_red_expr) Genredexpr.red_expr_gen * 'nam Locus.clause_expr
  | TacChange of check_flag * 'redpat option * 'dtrm * 'nam Locus.clause_expr
  | TacRewrite of evars_flag *
      (bool * Equality.multi * 'dtrm with_bindings_arg) list * 'nam Locus.clause_expr *
      'tacexpr option
  | TacInversion of ('trm,'dtrm,'nam) inversion_strength * Tactypes.quantified_hypothesis

and ('trm, 'dtrm, 'pat, 'redpat, 'cst, 'ref, 'nam, 'occvar, 'tacexpr, 'lev) gen_tactic_arg =
  | TacGeneric     of string option * 'lev Genarg.generic_argument
  | ConstrMayEval  of ('trm,'cst,'redpat,'occvar,'lev Redexpr.user_red_expr) may_eval
  | Reference      of 'ref
  | TacCall        of ('ref *
      ('trm, 'dtrm, 'pat, 'redpat, 'cst, 'ref, 'nam, 'occvar, 'tacexpr, 'lev) gen_tactic_arg list) CAst.t
  | TacFreshId of string Locus.or_var list
  | Tacexp of 'tacexpr
  | TacPretype of 'trm
  | TacNumgoals
and ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr_r =
  | TacAtom of ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_atomic_tactic_expr
  | TacThen of
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacDispatch of
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr list
  | TacExtendTac of
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr array *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr array
  | TacThens of
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr list
  | TacThens3parts of
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr array *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr array
  | TacFirst of ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr list
  | TacSolve of ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr list
  | TacTry of ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacOr of
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacOnce of
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacExactlyOnce of
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacIfThenCatch of
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacOrelse of
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacDo of int Locus.or_var * ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacTimeout of int Locus.or_var * ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacTime of string option * ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacRepeat of ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacProgress of ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  (* | TacShowHyps of ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *)
  | TacAbstract of
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr * Names.Id.t option
  | TacId of 'n message_token list
  | TacFail of global_flag * int Locus.or_var * 'n message_token list
  (* | TacInfo of ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *)
  | TacLetIn of rec_flag *
      (Names.lname * ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_arg) list *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacMatch of lazy_flag *
      ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr *
      ('p,('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr) match_rule list
  | TacMatchGoal of lazy_flag * direction_flag *
      ('p,('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr) match_rule list
  | TacFun of ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_fun_ast
  | TacArg of ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_arg
  | TacSelect of Goal_select.t * ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
  | TacML     of (ml_tactic_entry * ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_arg list)
  | TacAlias  of (Names.KerName.t * ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_arg list)

and ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr =
  ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr_r CAst.t

and ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_fun_ast =
    Names.Name.t list * ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) gen_tactic_expr
   [@@deriving sexp,yojson,hash,compare]

end
[@@@ocaml.warning "+27"]

let rec _gen_atomic_tactic_expr_put (t : 'a Ltac_plugin.Tacexpr.gen_atomic_tactic_expr) :
  ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) ITac.gen_atomic_tactic_expr = match t with
  | Ltac_plugin.Tacexpr.TacIntroPattern(a,b)         -> ITac.TacIntroPattern(a,b)
  | Ltac_plugin.Tacexpr.TacApply (a,b,c,d)           -> ITac.TacApply (a,b,c,d)
  | Ltac_plugin.Tacexpr.TacElim (a,b,c)              -> ITac.TacElim (a,b,c)
  | Ltac_plugin.Tacexpr.TacCase (a,b)                -> ITac.TacCase (a,b)
  | Ltac_plugin.Tacexpr.TacMutualFix (a,b,c)         -> ITac.TacMutualFix (a,b,c)
  | Ltac_plugin.Tacexpr.TacMutualCofix (a,b)         -> ITac.TacMutualCofix (a,b)
  | Ltac_plugin.Tacexpr.TacAssert (a,b,c,d,e)        -> ITac.TacAssert (a,b,c,d,e)
  | Ltac_plugin.Tacexpr.TacGeneralize a              -> ITac.TacGeneralize a
  | Ltac_plugin.Tacexpr.TacLetTac (a,b,c,d,e,f)      -> ITac.TacLetTac (a,b,c,d,e,f)
  | Ltac_plugin.Tacexpr.TacInductionDestruct (a,b,c) -> ITac.TacInductionDestruct (a,b,c)
  | Ltac_plugin.Tacexpr.TacReduce (a,b)              -> ITac.TacReduce (a,b)
  | Ltac_plugin.Tacexpr.TacChange (a,b,c,d)          -> ITac.TacChange (a,b,c,d)
  | Ltac_plugin.Tacexpr.TacRewrite (a,b,c,d)         -> ITac.TacRewrite (a,b,c,d)
  | Ltac_plugin.Tacexpr.TacInversion (a,b)           -> ITac.TacInversion (a,b)
and _gen_tactic_arg_put (t : 'a Ltac_plugin.Tacexpr.gen_tactic_arg) :
  ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) ITac.gen_tactic_arg = match t with
  | Ltac_plugin.Tacexpr.TacGeneric (a,b)  -> ITac.TacGeneric (a,b)
  | Ltac_plugin.Tacexpr.ConstrMayEval a   -> ITac.ConstrMayEval a
  | Ltac_plugin.Tacexpr.Reference a       -> ITac.Reference a
  | Ltac_plugin.Tacexpr.TacCall l         -> ITac.TacCall C.(map (fun (b,c) -> (b, List.map _gen_tactic_arg_put c)) l)
  | Ltac_plugin.Tacexpr.TacFreshId a      -> ITac.TacFreshId a
  | Ltac_plugin.Tacexpr.Tacexp a          -> ITac.Tacexp a
  | Ltac_plugin.Tacexpr.TacPretype a      -> ITac.TacPretype a
  | Ltac_plugin.Tacexpr.TacNumgoals       -> ITac.TacNumgoals
and _gen_tactic_expr_r_put (t : 'a Ltac_plugin.Tacexpr.gen_tactic_expr_r) :
  ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) ITac.gen_tactic_expr_r =
  let u  x = _gen_tactic_expr_put x in
  let uu x = List.map u x           in
  let ua x = Array.map u x          in
  match t with
  | Ltac_plugin.Tacexpr.TacAtom l                -> ITac.TacAtom (_gen_atomic_tactic_expr_put l)
  | Ltac_plugin.Tacexpr.TacThen (a,b)            -> ITac.TacThen (u a, u b)
  | Ltac_plugin.Tacexpr.TacDispatch a            -> ITac.TacDispatch (uu a)
  | Ltac_plugin.Tacexpr.TacExtendTac (a,b,c)     -> ITac.TacExtendTac (ua a, u b, ua c)
  | Ltac_plugin.Tacexpr.TacThens (a,b)           -> ITac.TacThens (u a, uu b)
  | Ltac_plugin.Tacexpr.TacThens3parts (a,b,c,d) -> ITac.TacThens3parts (u a, ua b, u c, ua d)
  | Ltac_plugin.Tacexpr.TacFirst a               -> ITac.TacFirst (uu a)
  | Ltac_plugin.Tacexpr.TacSolve a               -> ITac.TacSolve (uu a)
  | Ltac_plugin.Tacexpr.TacTry a                 -> ITac.TacTry (u a)
  | Ltac_plugin.Tacexpr.TacOr (a,b)              -> ITac.TacOr (u a, u b)
  | Ltac_plugin.Tacexpr.TacOnce a                -> ITac.TacOnce (u a)
  | Ltac_plugin.Tacexpr.TacExactlyOnce a         -> ITac.TacExactlyOnce (u a)
  | Ltac_plugin.Tacexpr.TacIfThenCatch (a,b,c)   -> ITac.TacIfThenCatch (u a,u b,u c)
  | Ltac_plugin.Tacexpr.TacOrelse (a,b)          -> ITac.TacOrelse (u a,u b)
  | Ltac_plugin.Tacexpr.TacDo (a,b)              -> ITac.TacDo (a,u b)
  | Ltac_plugin.Tacexpr.TacTimeout (a,b)         -> ITac.TacTimeout (a,u b)
  | Ltac_plugin.Tacexpr.TacTime (a,b)            -> ITac.TacTime (a,u b)
  | Ltac_plugin.Tacexpr.TacRepeat a              -> ITac.TacRepeat (u a)
  | Ltac_plugin.Tacexpr.TacProgress a            -> ITac.TacProgress (u a)
  (* | Ltac_plugin.Tacexpr.TacShowHyps a            -> ITac.TacShowHyps (u a) *)
  | Ltac_plugin.Tacexpr.TacAbstract (a,b)        -> ITac.TacAbstract (u a,b)
  | Ltac_plugin.Tacexpr.TacId a                  -> ITac.TacId a
  | Ltac_plugin.Tacexpr.TacFail (a,b,c)          -> ITac.TacFail (a,b,c)
  (* | Ltac_plugin.Tacexpr.TacInfo a                -> ITac.TacInfo (u a) *)
  (* | TacLetIn of rec_flag * *)
  (*     (Names.Id.t located * 'a gen_tactic_arg) list * *)
  (*     'a gen_tactic_expr *)
  | Ltac_plugin.Tacexpr.TacLetIn (a, l, t) ->
    let _pt = List.map (fun (a,t) -> (a,_gen_tactic_arg_put t)) in
    ITac.TacLetIn (a, _pt l, _gen_tactic_expr_put t)
  (* | TacMatch of lazy_flag * *)
  (*     'a gen_tactic_expr * *)
  (*     ('p,'a gen_tactic_expr) match_rule list *)
  (* type ('a,'t) match_rule = *)
  (* | Pat of 'a match_context_hyps list * 'a match_pattern * 't *)
  (* | All of 't *)
  | Ltac_plugin.Tacexpr.TacMatch (a, e, mr)      ->
    let _pmr = List.map (function
        | Ltac_plugin.Tacexpr.Pat (a,b,t) -> Ltac_plugin.Tacexpr.Pat (a,b,_gen_tactic_expr_put t)
        | Ltac_plugin.Tacexpr.All e       -> Ltac_plugin.Tacexpr.All (_gen_tactic_expr_put e)
      ) in
    ITac.TacMatch(a, _gen_tactic_expr_put e, _pmr mr)
  | Ltac_plugin.Tacexpr.TacMatchGoal (e, d, t)  ->
    let _pmr = List.map (function
        | Ltac_plugin.Tacexpr.Pat (a,b,t) -> Ltac_plugin.Tacexpr.Pat (a,b,_gen_tactic_expr_put t)
        | Ltac_plugin.Tacexpr.All e       -> Ltac_plugin.Tacexpr.All (_gen_tactic_expr_put e)
      ) in
    ITac.TacMatchGoal(e, d, _pmr t)
  | Ltac_plugin.Tacexpr.TacFun a                 -> ITac.TacFun (_gen_tactic_fun_ast_put a)
  | Ltac_plugin.Tacexpr.TacArg l                 -> ITac.TacArg (_gen_tactic_arg_put l)
  | Ltac_plugin.Tacexpr.TacSelect(gs,te)         -> ITac.TacSelect(gs, _gen_tactic_expr_put te)
  | Ltac_plugin.Tacexpr.TacML (l,m)              -> ITac.TacML (l, List.map _gen_tactic_arg_put m)
  | Ltac_plugin.Tacexpr.TacAlias (l,m)           -> ITac.TacAlias (l, List.map _gen_tactic_arg_put m)
and _gen_tactic_expr_put (t : _ Ltac_plugin.Tacexpr.gen_tactic_expr) =
  C.map _gen_tactic_expr_r_put t

and _gen_tactic_fun_ast_put (t : 'a Ltac_plugin.Tacexpr.gen_tactic_fun_ast) :
  ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) ITac.gen_tactic_fun_ast =
  match t with
  | (a,b) -> (a, _gen_tactic_expr_put b)

let rec _gen_atom_tactic_expr_get (t : ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) ITac.gen_atomic_tactic_expr) :
  'a Ltac_plugin.Tacexpr.gen_atomic_tactic_expr = match t with
  | ITac.TacIntroPattern(a,b)         -> Ltac_plugin.Tacexpr.TacIntroPattern(a,b)
  | ITac.TacApply (a,b,c,d)           -> Ltac_plugin.Tacexpr.TacApply (a,b,c,d)
  | ITac.TacElim (a,b,c)              -> Ltac_plugin.Tacexpr.TacElim (a,b,c)
  | ITac.TacCase (a,b)                -> Ltac_plugin.Tacexpr.TacCase (a,b)
  | ITac.TacMutualFix (a,b,c)         -> Ltac_plugin.Tacexpr.TacMutualFix (a,b,c)
  | ITac.TacMutualCofix (a,b)         -> Ltac_plugin.Tacexpr.TacMutualCofix (a,b)
  | ITac.TacAssert (a,b,c,d,e)        -> Ltac_plugin.Tacexpr.TacAssert (a,b,c,d,e)
  | ITac.TacGeneralize a              -> Ltac_plugin.Tacexpr.TacGeneralize a
  | ITac.TacLetTac (a,b,c,d,e,f)      -> Ltac_plugin.Tacexpr.TacLetTac (a,b,c,d,e,f)
  | ITac.TacInductionDestruct (a,b,c) -> Ltac_plugin.Tacexpr.TacInductionDestruct (a,b,c)
  | ITac.TacReduce (a,b)              -> Ltac_plugin.Tacexpr.TacReduce (a,b)
  | ITac.TacChange (a,b,c,d)          -> Ltac_plugin.Tacexpr.TacChange (a,b,c,d)
  | ITac.TacRewrite (a,b,c,d)         -> Ltac_plugin.Tacexpr.TacRewrite (a,b,c,d)
  | ITac.TacInversion (a,b)           -> Ltac_plugin.Tacexpr.TacInversion (a,b)
and _gen_tactic_arg_get (t : ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) ITac.gen_tactic_arg) :
  'a Ltac_plugin.Tacexpr.gen_tactic_arg = match t with
  | ITac.TacGeneric(a,b)   -> Ltac_plugin.Tacexpr.TacGeneric (a,b)
  | ITac.ConstrMayEval a   -> Ltac_plugin.Tacexpr.ConstrMayEval a
  | ITac.Reference a       -> Ltac_plugin.Tacexpr.Reference a
  | ITac.TacCall l         -> Ltac_plugin.Tacexpr.TacCall C.(map (fun (b,c) -> (b, List.map _gen_tactic_arg_get c)) l)
  | ITac.TacFreshId a      -> Ltac_plugin.Tacexpr.TacFreshId a
  | ITac.Tacexp a          -> Ltac_plugin.Tacexpr.Tacexp a
  | ITac.TacPretype a      -> Ltac_plugin.Tacexpr.TacPretype a
  | ITac.TacNumgoals       -> Ltac_plugin.Tacexpr.TacNumgoals
and _gen_tactic_expr_r_get (t : ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) ITac.gen_tactic_expr_r) :
  'a Ltac_plugin.Tacexpr.gen_tactic_expr_r =
  let u  x = _gen_tactic_expr_get x in
  let uu x = List.map u x           in
  let ua x = Array.map u x          in
  match t with
  | ITac.TacAtom l                -> Ltac_plugin.Tacexpr.TacAtom (_gen_atom_tactic_expr_get l)
  | ITac.TacThen (a,b)            -> Ltac_plugin.Tacexpr.TacThen (u a, u b)
  | ITac.TacDispatch a            -> Ltac_plugin.Tacexpr.TacDispatch (uu a)
  | ITac.TacExtendTac (a,b,c)     -> Ltac_plugin.Tacexpr.TacExtendTac (ua a, u b, ua c)
  | ITac.TacThens (a,b)           -> Ltac_plugin.Tacexpr.TacThens (u a, uu b)
  | ITac.TacThens3parts (a,b,c,d) -> Ltac_plugin.Tacexpr.TacThens3parts (u a, ua b, u c, ua d)
  | ITac.TacFirst a               -> Ltac_plugin.Tacexpr.TacFirst (uu a)
  | ITac.TacSolve a               -> Ltac_plugin.Tacexpr.TacSolve (uu a)
  | ITac.TacTry a                 -> Ltac_plugin.Tacexpr.TacTry (u a)
  | ITac.TacOr (a,b)              -> Ltac_plugin.Tacexpr.TacOr (u a, u b)
  | ITac.TacOnce a                -> Ltac_plugin.Tacexpr.TacOnce (u a)
  | ITac.TacExactlyOnce a         -> Ltac_plugin.Tacexpr.TacExactlyOnce (u a)
  | ITac.TacIfThenCatch (a,b,c)   -> Ltac_plugin.Tacexpr.TacIfThenCatch (u a,u b,u c)
  | ITac.TacOrelse (a,b)          -> Ltac_plugin.Tacexpr.TacOrelse (u a,u b)
  | ITac.TacDo (a,b)              -> Ltac_plugin.Tacexpr.TacDo (a,u b)
  | ITac.TacTimeout (a,b)         -> Ltac_plugin.Tacexpr.TacTimeout (a,u b)
  | ITac.TacTime (a,b)            -> Ltac_plugin.Tacexpr.TacTime (a,u b)
  | ITac.TacRepeat a              -> Ltac_plugin.Tacexpr.TacRepeat (u a)
  | ITac.TacProgress a            -> Ltac_plugin.Tacexpr.TacProgress (u a)
  (* | ITac.TacShowHyps a            -> Ltac_plugin.Tacexpr.TacShowHyps (u a) *)
  | ITac.TacAbstract (a,b)        -> Ltac_plugin.Tacexpr.TacAbstract (u a,b)
  | ITac.TacId a                  -> Ltac_plugin.Tacexpr.TacId a
  | ITac.TacFail (a,b,c)          -> Ltac_plugin.Tacexpr.TacFail (a,b,c)
  (* | ITac.TacInfo a                -> Ltac_plugin.Tacexpr.TacInfo (u a) *)
  | ITac.TacLetIn (a, l, t)       ->
    let _pt = List.map (fun (a,t) -> (a,_gen_tactic_arg_get t)) in
    Ltac_plugin.Tacexpr.TacLetIn (a, _pt l, _gen_tactic_expr_get t)
  | ITac.TacMatch (a,e,mr)          ->
    let _gmr = List.map (function
        | Ltac_plugin.Tacexpr.Pat (a,b,t) -> Ltac_plugin.Tacexpr.Pat (a,b,_gen_tactic_expr_get t)
        | Ltac_plugin.Tacexpr.All e       -> Ltac_plugin.Tacexpr.All (_gen_tactic_expr_get e)
      ) in
    Ltac_plugin.Tacexpr.TacMatch(a, _gen_tactic_expr_get e, _gmr mr)
  | ITac.TacMatchGoal (a,d,t)     ->
    let _gmr = List.map (function
        | Ltac_plugin.Tacexpr.Pat (a,b,t) -> Ltac_plugin.Tacexpr.Pat (a,b,_gen_tactic_expr_get t)
        | Ltac_plugin.Tacexpr.All e       -> Ltac_plugin.Tacexpr.All (_gen_tactic_expr_get e)
      ) in
    Ltac_plugin.Tacexpr.TacMatchGoal(a,d, _gmr t)
  | ITac.TacFun a                 -> Ltac_plugin.Tacexpr.TacFun (_gen_tactic_fun_ast_get a)
  | ITac.TacArg l                 -> Ltac_plugin.Tacexpr.TacArg (_gen_tactic_arg_get l)
  | ITac.TacSelect(gs,te)         -> Ltac_plugin.Tacexpr.TacSelect(gs, _gen_tactic_expr_get te)
  | ITac.TacML (l,m)              -> Ltac_plugin.Tacexpr.TacML (l, List.map _gen_tactic_arg_get m)
  | ITac.TacAlias (l,m)           -> Ltac_plugin.Tacexpr.TacAlias (l, List.map _gen_tactic_arg_get m)

and _gen_tactic_expr_get (t : ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) ITac.gen_tactic_expr) :
  'a Ltac_plugin.Tacexpr.gen_tactic_expr =
  C.map _gen_tactic_expr_r_get t

and _gen_tactic_fun_ast_get (t : ('t, 'dtrm, 'p, 'rp, 'c, 'r, 'n, 'occvar, 'tacexpr, 'l) ITac.gen_tactic_fun_ast) :
  'a Ltac_plugin.Tacexpr.gen_tactic_fun_ast =
  match t with
  | (a,b) -> (a, _gen_tactic_expr_get b)

type 'd gen_atomic_tactic_expr = 'd Ltac_plugin.Tacexpr.gen_atomic_tactic_expr

(* Sexp part for generic functions *)

let sexp_of_gen_atomic_tactic_expr
  t d p rp c r n ov te l (tac : 'a Ltac_plugin.Tacexpr.gen_atomic_tactic_expr) : Sexp.t =
  ITac.sexp_of_gen_atomic_tactic_expr t d p rp c r n ov te l (_gen_atomic_tactic_expr_put tac)

let sexp_of_gen_tactic_expr
  t d p rp c r n ov te l (tac : 'a Ltac_plugin.Tacexpr.gen_tactic_expr) : Sexp.t =
  ITac.sexp_of_gen_tactic_expr t d p rp c r n ov te l (_gen_tactic_expr_put tac)

let sexp_of_gen_tactic_arg
  t d p rp c r n ov te l (tac : 'a Ltac_plugin.Tacexpr.gen_tactic_arg) : Sexp.t =
  ITac.sexp_of_gen_tactic_arg t d p rp c r n ov te l (_gen_tactic_arg_put tac)

let sexp_of_gen_fun_ast
  t d p rp c r n ov te l (tac : 'a Ltac_plugin.Tacexpr.gen_tactic_fun_ast) : Sexp.t =
  ITac.sexp_of_gen_tactic_fun_ast t d p rp c r n ov te l (_gen_tactic_fun_ast_put tac)

let gen_atomic_tactic_expr_of_sexp (tac : Sexp.t)
  t d p rp c r n ov te l : 'a Ltac_plugin.Tacexpr.gen_atomic_tactic_expr =
  _gen_atom_tactic_expr_get (ITac.gen_atomic_tactic_expr_of_sexp t d p rp c r n ov te l tac)

let gen_tactic_expr_of_sexp (tac : Sexp.t)
  t d p rp c r n ov te l : 'a Ltac_plugin.Tacexpr.gen_tactic_expr =
  _gen_tactic_expr_get (ITac.gen_tactic_expr_of_sexp t d p rp c r n ov te l tac)

let gen_tactic_arg_of_sexp (tac : Sexp.t)
  t d p rp c r n ov te l : 'a Ltac_plugin.Tacexpr.gen_tactic_arg =
  _gen_tactic_arg_get (ITac.gen_tactic_arg_of_sexp t d p rp c r n ov te l tac)

let gen_fun_ast_of_sexp (tac : Sexp.t)
  t d p rp c r n ov te l : 'a Ltac_plugin.Tacexpr.gen_tactic_fun_ast =
  _gen_tactic_fun_ast_get (ITac.gen_tactic_fun_ast_of_sexp t d p rp c r n ov te l tac)

(* Yojson part for generic functions *)

let gen_atomic_tactic_expr_to_yojson
  t d p rp c r n ov te l (tac : 'a Ltac_plugin.Tacexpr.gen_atomic_tactic_expr) : _ =
  ITac.gen_atomic_tactic_expr_to_yojson t d p rp c r n ov te l (_gen_atomic_tactic_expr_put tac)

let gen_tactic_expr_to_yojson
  t d p rp c r n ov te l (tac : 'a Ltac_plugin.Tacexpr.gen_tactic_expr) : Yojson.Safe.t =
  ITac.gen_tactic_expr_to_yojson t d p rp c r n ov te l (_gen_tactic_expr_put tac)

let gen_tactic_expr_of_yojson tac
  t d p rp c r n ov te l : ('a Ltac_plugin.Tacexpr.gen_tactic_expr, _) result =
  Result.map _gen_tactic_expr_get (ITac.gen_tactic_expr_of_yojson t d p rp c r n ov te l tac)

let gen_atomic_tactic_expr_of_yojson tac
  t d p rp c r n ov te l : ('a Ltac_plugin.Tacexpr.gen_atomic_tactic_expr, _) result =
  Result.map _gen_atom_tactic_expr_get (ITac.gen_atomic_tactic_expr_of_yojson t d p rp c r n ov te l tac)

(* Hash part for generic functions *)

let hash_fold_gen_tactic_expr t d p rp c r n ov te l st tac =
  ITac.hash_fold_gen_tactic_expr t d p rp c r n ov te l st (_gen_tactic_expr_put tac)

let hash_fold_gen_atomic_tactic_expr t d p rp c r n ov te l st tac =
  ITac.hash_fold_gen_atomic_tactic_expr t d p rp c r n ov te l st (_gen_atomic_tactic_expr_put tac)

(* Compare part for generic functions *)

let compare_gen_tactic_expr t d p rp c r n ov te l t1 t2 : int =
  ITac.compare_gen_tactic_expr t d p rp c r n ov te l (_gen_tactic_expr_put t1) (_gen_tactic_expr_put t2)

let compare_gen_atomic_tactic_expr t d p rp c r n ov te l t1 t2 =
  ITac.compare_gen_atomic_tactic_expr t d p rp c r n ov te l (_gen_atomic_tactic_expr_put t1) (_gen_atomic_tactic_expr_put t2)

(************************************************************************)
(* Main tactics types, we follow tacexpr and provide glob,raw, and      *)
(* atomic                                                               *)
(************************************************************************)

(* Glob *)
type glob_tactic_expr = Ltac_plugin.Tacexpr.glob_tactic_expr
type glob_atomic_tactic_expr = Ltac_plugin.Tacexpr.glob_atomic_tactic_expr

let rec glob_tactic_expr_of_sexp tac =
  gen_tactic_expr_of_sexp
    tac
    Genintern.glob_constr_and_expr_of_sexp
    Genintern.glob_constr_and_expr_of_sexp
    Genintern.glob_constr_pattern_and_expr_of_sexp
    Genintern.glob_constr_and_expr_of_sexp
    (Locus.or_var_of_sexp (Genredexpr.and_short_name_of_sexp Evaluable.t_of_sexp))
    (Locus.or_var_of_sexp (Loc.located_of_sexp ltac_constant_of_sexp))
    Names.lident_of_sexp
    (Locus.or_var_of_sexp int_of_sexp)
    glob_tactic_expr_of_sexp
    Genarg.glevel_of_sexp
and glob_atomic_tactic_expr_of_sexp tac =
  gen_atomic_tactic_expr_of_sexp
    tac
    Genintern.glob_constr_and_expr_of_sexp
    Genintern.glob_constr_and_expr_of_sexp
    Genintern.glob_constr_pattern_and_expr_of_sexp
    Genintern.glob_constr_and_expr_of_sexp
    (Locus.or_var_of_sexp (Genredexpr.and_short_name_of_sexp Evaluable.t_of_sexp))
    (Locus.or_var_of_sexp (Loc.located_of_sexp ltac_constant_of_sexp))
    Names.lident_of_sexp
    (Locus.or_var_of_sexp int_of_sexp)
    glob_tactic_expr_of_sexp
    Genarg.glevel_of_sexp

let rec sexp_of_glob_tactic_expr (tac : glob_tactic_expr) =
  sexp_of_gen_tactic_expr
    Genintern.sexp_of_glob_constr_and_expr
    Genintern.sexp_of_glob_constr_and_expr
    Genintern.sexp_of_glob_constr_pattern_and_expr
    Genintern.sexp_of_glob_constr_and_expr
    (Locus.sexp_of_or_var (Genredexpr.sexp_of_and_short_name Evaluable.sexp_of_t))
    (Locus.sexp_of_or_var (Loc.sexp_of_located sexp_of_ltac_constant))
    Names.sexp_of_lident
    (Locus.sexp_of_or_var sexp_of_int)
    sexp_of_glob_tactic_expr
    Genarg.sexp_of_glevel
    tac
and sexp_of_glob_atomic_tactic_expr (tac : glob_atomic_tactic_expr) =
  sexp_of_gen_atomic_tactic_expr
    Genintern.sexp_of_glob_constr_and_expr
    Genintern.sexp_of_glob_constr_and_expr
    Genintern.sexp_of_glob_constr_pattern_and_expr
    Genintern.sexp_of_glob_constr_and_expr
    (Locus.sexp_of_or_var (Genredexpr.sexp_of_and_short_name Evaluable.sexp_of_t))
    (Locus.sexp_of_or_var (Loc.sexp_of_located sexp_of_ltac_constant))
    Names.sexp_of_lident
    (Locus.sexp_of_or_var sexp_of_int)
    sexp_of_glob_tactic_expr
    Genarg.sexp_of_glevel
    tac

let rec glob_tactic_expr_of_yojson tac =
  gen_tactic_expr_of_yojson
    tac
    Genintern.glob_constr_and_expr_of_yojson
    Genintern.glob_constr_and_expr_of_yojson
    Genintern.glob_constr_pattern_and_expr_of_yojson
    Genintern.glob_constr_and_expr_of_yojson
    (Locus.or_var_of_yojson (Genredexpr.and_short_name_of_yojson Evaluable.of_yojson))
    (Locus.or_var_of_yojson (Loc.located_of_yojson ltac_constant_of_yojson))
    Names.lident_of_yojson
    (Locus.or_var_of_yojson Ser_int.of_yojson)
    glob_tactic_expr_of_yojson
    Genarg.glevel_of_yojson
and glob_atomic_tactic_expr_of_yojson tac =
  gen_atomic_tactic_expr_of_yojson
    tac
    Genintern.glob_constr_and_expr_of_yojson
    Genintern.glob_constr_and_expr_of_yojson
    Genintern.glob_constr_pattern_and_expr_of_yojson
    Genintern.glob_constr_and_expr_of_yojson
    (Locus.or_var_of_yojson (Genredexpr.and_short_name_of_yojson Evaluable.of_yojson))
    (Locus.or_var_of_yojson (Loc.located_of_yojson ltac_constant_of_yojson))
    Names.lident_of_yojson
    (Locus.or_var_of_yojson Ser_int.of_yojson)
    glob_tactic_expr_of_yojson
    Genarg.glevel_of_yojson

let rec glob_tactic_expr_to_yojson tac =
  gen_tactic_expr_to_yojson
    Genintern.glob_constr_and_expr_to_yojson
    Genintern.glob_constr_and_expr_to_yojson
    Genintern.glob_constr_pattern_and_expr_to_yojson
    Genintern.glob_constr_and_expr_to_yojson
    (Locus.or_var_to_yojson (Genredexpr.and_short_name_to_yojson Evaluable.to_yojson))
    (Locus.or_var_to_yojson (Loc.located_to_yojson ltac_constant_to_yojson))
    Names.lident_to_yojson
    (Locus.or_var_to_yojson Ser_int.to_yojson)
    glob_tactic_expr_to_yojson
    Genarg.glevel_to_yojson
    tac
and glob_atomic_tactic_expr_to_yojson tac =
  gen_atomic_tactic_expr_to_yojson
    Genintern.glob_constr_and_expr_to_yojson
    Genintern.glob_constr_and_expr_to_yojson
    Genintern.glob_constr_pattern_and_expr_to_yojson
    Genintern.glob_constr_and_expr_to_yojson
    (Locus.or_var_to_yojson (Genredexpr.and_short_name_to_yojson Evaluable.to_yojson))
    (Locus.or_var_to_yojson (Loc.located_to_yojson ltac_constant_to_yojson))
    Names.lident_to_yojson
    (Locus.or_var_to_yojson Ser_int.to_yojson)
    glob_tactic_expr_to_yojson
    Genarg.glevel_to_yojson
    tac

let rec hash_fold_glob_tactic_expr st tac =
  hash_fold_gen_tactic_expr
    Genintern.hash_fold_glob_constr_and_expr
    Genintern.hash_fold_glob_constr_and_expr
    Genintern.hash_fold_glob_constr_pattern_and_expr
    Genintern.hash_fold_glob_constr_and_expr
    (Locus.hash_fold_or_var (Genredexpr.hash_fold_and_short_name Evaluable.hash_fold_t))
    (Locus.hash_fold_or_var (Loc.hash_fold_located hash_fold_ltac_constant))
    Names.hash_fold_lident
    (Locus.hash_fold_or_var Ser_int.hash_fold_t)
    hash_fold_glob_tactic_expr
    Genarg.hash_fold_glevel
    st tac
and hash_fold_glob_atomic_tactic_expr st tac =
  hash_fold_gen_atomic_tactic_expr
    Genintern.hash_fold_glob_constr_and_expr
    Genintern.hash_fold_glob_constr_and_expr
    Genintern.hash_fold_glob_constr_pattern_and_expr
    Genintern.hash_fold_glob_constr_and_expr
    (Locus.hash_fold_or_var (Genredexpr.hash_fold_and_short_name Evaluable.hash_fold_t))
    (Locus.hash_fold_or_var (Loc.hash_fold_located hash_fold_ltac_constant))
    Names.hash_fold_lident
    (Locus.hash_fold_or_var Ser_int.hash_fold_t)
    hash_fold_glob_tactic_expr
    Genarg.hash_fold_glevel
    st tac

let hash_glob_tactic_expr = Ppx_hash_lib.Std.Hash.of_fold hash_fold_glob_tactic_expr
let hash_glob_atomic_tactic_expr = Ppx_hash_lib.Std.Hash.of_fold hash_fold_glob_atomic_tactic_expr

let rec compare_glob_tactic_expr tac =
  compare_gen_tactic_expr
    Genintern.compare_glob_constr_and_expr
    Genintern.compare_glob_constr_and_expr
    Genintern.compare_glob_constr_pattern_and_expr
    Genintern.compare_glob_constr_and_expr
    (Locus.compare_or_var (Genredexpr.compare_and_short_name Evaluable.compare))
    (Locus.compare_or_var (Loc.compare_located compare_ltac_constant))
    Names.compare_lident
    (Locus.compare_or_var Ser_int.compare)
    compare_glob_tactic_expr
    Genarg.compare_glevel
    tac
and compare_glob_atomic_tactic_expr tac =
  compare_gen_atomic_tactic_expr
    Genintern.compare_glob_constr_and_expr
    Genintern.compare_glob_constr_and_expr
    Genintern.compare_glob_constr_pattern_and_expr
    Genintern.compare_glob_constr_and_expr
    (Locus.compare_or_var (Genredexpr.compare_and_short_name Evaluable.compare))
    (Locus.compare_or_var (Loc.compare_located compare_ltac_constant))
    Names.compare_lident
    (Locus.compare_or_var Ser_int.compare)
    compare_glob_tactic_expr
    Genarg.compare_glevel
    tac

(* Raw *)
type raw_tactic_expr = Ltac_plugin.Tacexpr.raw_tactic_expr
type raw_atomic_tactic_expr = Ltac_plugin.Tacexpr.raw_atomic_tactic_expr

let rec raw_tactic_expr_of_sexp tac =
  gen_tactic_expr_of_sexp
    tac
    Constrexpr.constr_expr_of_sexp
    Constrexpr.constr_expr_of_sexp
    Constrexpr.constr_pattern_expr_of_sexp
    Constrexpr.constr_expr_of_sexp
    (Constrexpr.or_by_notation_of_sexp Libnames.qualid_of_sexp)
    Libnames.qualid_of_sexp
    Names.lident_of_sexp
    (Locus.or_var_of_sexp Ser_int.t_of_sexp)
    raw_tactic_expr_of_sexp
    Genarg.rlevel_of_sexp
and raw_atomic_tactic_expr_of_sexp tac =
  gen_atomic_tactic_expr_of_sexp
    tac
    Constrexpr.constr_expr_of_sexp
    Constrexpr.constr_expr_of_sexp
    Constrexpr.constr_pattern_expr_of_sexp
    Constrexpr.constr_expr_of_sexp
    (Constrexpr.or_by_notation_of_sexp Libnames.qualid_of_sexp)
    Libnames.qualid_of_sexp
    Names.lident_of_sexp
    (Locus.or_var_of_sexp Ser_int.t_of_sexp)
    raw_tactic_expr_of_sexp
    Genarg.rlevel_of_sexp

let rec sexp_of_raw_tactic_expr (tac : raw_tactic_expr) =
  sexp_of_gen_tactic_expr
    Constrexpr.sexp_of_constr_expr
    Constrexpr.sexp_of_constr_expr
    Constrexpr.sexp_of_constr_pattern_expr
    Constrexpr.sexp_of_constr_expr
    (Constrexpr.sexp_of_or_by_notation Libnames.sexp_of_qualid)
    Libnames.sexp_of_qualid
    Names.sexp_of_lident
    (Locus.sexp_of_or_var Ser_int.sexp_of_t)
    sexp_of_raw_tactic_expr
    Genarg.sexp_of_rlevel
    tac
and sexp_of_raw_atomic_tactic_expr tac =
  sexp_of_gen_atomic_tactic_expr
    Constrexpr.sexp_of_constr_expr
    Constrexpr.sexp_of_constr_expr
    Constrexpr.sexp_of_constr_pattern_expr
    Constrexpr.sexp_of_constr_expr
    (Constrexpr.sexp_of_or_by_notation Libnames.sexp_of_qualid)
    Libnames.sexp_of_qualid
    Names.sexp_of_lident
    (Locus.sexp_of_or_var Ser_int.sexp_of_t)
    sexp_of_raw_tactic_expr
    Genarg.sexp_of_rlevel
    tac

(* Yojson *)
let rec raw_tactic_expr_of_yojson tac =
  gen_tactic_expr_of_yojson
    tac
    Constrexpr.constr_expr_of_yojson
    Constrexpr.constr_expr_of_yojson
    Constrexpr.constr_pattern_expr_of_yojson
    Constrexpr.constr_expr_of_yojson
    (Constrexpr.or_by_notation_of_yojson Libnames.qualid_of_yojson)
    Libnames.qualid_of_yojson
    Names.lident_of_yojson
    (Locus.or_var_of_yojson Ser_int.of_yojson)
    raw_tactic_expr_of_yojson
    Genarg.rlevel_of_yojson
and raw_atomic_tactic_expr_of_yojson tac =
  gen_atomic_tactic_expr_of_yojson
    tac
    Constrexpr.constr_expr_of_yojson
    Constrexpr.constr_expr_of_yojson
    Constrexpr.constr_pattern_expr_of_yojson
    Constrexpr.constr_expr_of_yojson
    (Constrexpr.or_by_notation_of_yojson Libnames.qualid_of_yojson)
    Libnames.qualid_of_yojson
    Names.lident_of_yojson
    (Locus.or_var_of_yojson Ser_int.of_yojson)
    raw_tactic_expr_of_yojson
    Genarg.rlevel_of_yojson

let rec raw_tactic_expr_to_yojson (tac : raw_tactic_expr) =
  gen_tactic_expr_to_yojson
    Constrexpr.constr_expr_to_yojson
    Constrexpr.constr_expr_to_yojson
    Constrexpr.constr_pattern_expr_to_yojson
    Constrexpr.constr_expr_to_yojson
    (Constrexpr.or_by_notation_to_yojson Libnames.qualid_to_yojson)
    Libnames.qualid_to_yojson
    Names.lident_to_yojson
    (Locus.or_var_to_yojson Ser_int.to_yojson)
    raw_tactic_expr_to_yojson
    Genarg.rlevel_to_yojson
    tac
and raw_atomic_tactic_expr_to_yojson tac =
  gen_atomic_tactic_expr_to_yojson
    Constrexpr.constr_expr_to_yojson
    Constrexpr.constr_expr_to_yojson
    Constrexpr.constr_pattern_expr_to_yojson
    Constrexpr.constr_expr_to_yojson
    (Constrexpr.or_by_notation_to_yojson Libnames.qualid_to_yojson)
    Libnames.qualid_to_yojson
    Names.lident_to_yojson
    (Locus.or_var_to_yojson Ser_int.to_yojson)
    raw_tactic_expr_to_yojson
    Genarg.rlevel_to_yojson
    tac

let rec hash_fold_raw_tactic_expr st tac =
  hash_fold_gen_tactic_expr
    Constrexpr.hash_fold_constr_expr
    Constrexpr.hash_fold_constr_expr
    Constrexpr.hash_fold_constr_pattern_expr
    Constrexpr.hash_fold_constr_expr
    (Constrexpr.hash_fold_or_by_notation Libnames.hash_fold_qualid)
    Libnames.hash_fold_qualid
    Names.hash_fold_lident
    (Locus.hash_fold_or_var Ser_int.hash_fold_t)
    hash_fold_raw_tactic_expr
    Genarg.hash_fold_rlevel
    st tac
and hash_fold_raw_atomic_tactic_expr st tac =
  hash_fold_gen_atomic_tactic_expr
    Constrexpr.hash_fold_constr_expr
    Constrexpr.hash_fold_constr_expr
    Constrexpr.hash_fold_constr_pattern_expr
    Constrexpr.hash_fold_constr_expr
    (Constrexpr.hash_fold_or_by_notation Libnames.hash_fold_qualid)
    Libnames.hash_fold_qualid
    Names.hash_fold_lident
    (Locus.hash_fold_or_var Ser_int.hash_fold_t)
    hash_fold_raw_tactic_expr
    Genarg.hash_fold_rlevel
    st tac

let hash_raw_tactic_expr = Ppx_hash_lib.Std.Hash.of_fold hash_fold_raw_tactic_expr
let hash_raw_atomic_tactic_expr = Ppx_hash_lib.Std.Hash.of_fold hash_fold_raw_atomic_tactic_expr

let rec compare_raw_tactic_expr tac =
  compare_gen_tactic_expr
    Constrexpr.compare_constr_expr
    Constrexpr.compare_constr_expr
    Constrexpr.compare_constr_pattern_expr
    Constrexpr.compare_constr_expr
    (Constrexpr.compare_or_by_notation Libnames.compare_qualid)
    Libnames.compare_qualid
    Names.compare_lident
    (Locus.compare_or_var Ser_int.compare)
    compare_raw_tactic_expr
    Genarg.compare_rlevel
    tac
and compare_raw_atomic_tactic_expr tac =
  compare_gen_atomic_tactic_expr
    Constrexpr.compare_constr_expr
    Constrexpr.compare_constr_expr
    Constrexpr.compare_constr_pattern_expr
    Constrexpr.compare_constr_expr
    (Constrexpr.compare_or_by_notation Libnames.compare_qualid)
    Libnames.compare_qualid
    Names.compare_lident
    (Locus.compare_or_var Ser_int.compare)
    compare_raw_tactic_expr
    Genarg.compare_rlevel
    tac

(* Atomic *)
type atomic_tactic_expr = Ltac_plugin.Tacexpr.atomic_tactic_expr

let atomic_tactic_expr_of_sexp tac =
  gen_atomic_tactic_expr_of_sexp tac
    EConstr.t_of_sexp
    Genintern.glob_constr_and_expr_of_sexp
    Pattern.constr_pattern_of_sexp
    Pattern.constr_pattern_of_sexp
    Evaluable.t_of_sexp
    (Loc.located_of_sexp ltac_constant_of_sexp)
    Names.Id.t_of_sexp
    Ser_int.t_of_sexp
    unit_of_sexp
    Genarg.tlevel_of_sexp

let sexp_of_atomic_tactic_expr tac =
  sexp_of_gen_atomic_tactic_expr
    EConstr.sexp_of_t
    Genintern.sexp_of_glob_constr_and_expr
    Pattern.sexp_of_constr_pattern
    Pattern.sexp_of_constr_pattern
    Evaluable.sexp_of_t
    (Loc.sexp_of_located sexp_of_ltac_constant)
    Names.Id.sexp_of_t
    Ser_int.sexp_of_t
    sexp_of_unit
    Genarg.sexp_of_tlevel
    tac

(* Helpers for raw_red_expr *)
type tacdef_body =
  [%import: Ltac_plugin.Tacexpr.tacdef_body]
  [@@deriving sexp,yojson,hash,compare]

(* Unsupported serializers *)
type intro_pattern =
  [%import: Ltac_plugin.Tacexpr.intro_pattern]
  [@@deriving sexp,yojson,hash,compare]

type raw_strategy = [%import: Ltac_plugin.Tacexpr.raw_strategy]
  [@@deriving sexp,hash,compare]
type glob_strategy = [%import: Ltac_plugin.Tacexpr.glob_strategy]
  [@@deriving sexp,hash,compare]
