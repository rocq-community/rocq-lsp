(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(************************************************************************)
(* SerAPI: Coq interaction protocol with bidirectional serialization    *)
(************************************************************************)
(* Copyright 2016-2019 MINES ParisTech -- License LGPL 2.1+             *)
(* Copyright 2019-2023 Inria           -- License LGPL 2.1+             *)
(* Written by: Emilio J. Gallego Arias and others                       *)
(************************************************************************)

open Sexplib.Std
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

open Serlib

open Ltac_plugin

module TV = SerType.Opaque(struct type t = Tacarg.tacvalue let name = "tacvalue" end)

type tacvalue = TV.t
[@@deriving sexp,hash,compare]

module CAst         = Ser_cAst
module Names        = Ser_names
module Constrexpr   = Ser_constrexpr
module Tactypes     = Ser_tactypes
module Locus        = Ser_locus
module Libnames     = Ser_libnames
module Genintern    = Ser_genintern
module Geninterp    = Ser_geninterp
module EConstr      = Ser_eConstr
module Hints        = Ser_hints
module Ltac_pretype = Ser_ltac_pretype
module Genredexpr   = Ser_genredexpr
module Redexpr      = Ser_redexpr
module Gentactic    = Ser_gentactic

module Ltac_plugin = struct
  module G_rewrite    = G_rewrite
  module RewriteStratAst      = Ser_rewriteStratAst
  module Tacexpr      = Ser_tacexpr
  module Tacentries   = Ser_tacentries
end

(* Needed for compat with -pack build method used in Coq's make *)
open Ltac_plugin

(* Tacarg *)
module A1 = struct
  type raw = Constrexpr.constr_expr Tactypes.intro_pattern_expr CAst.t
  [@@deriving sexp,hash,compare]
  type glb = Genintern.glob_constr_and_expr Tactypes.intro_pattern_expr CAst.t
  [@@deriving sexp,hash,compare]
  type top = Ltac_plugin.Tacexpr.intro_pattern
  [@@deriving sexp,hash,compare]
end

let ser_wit_simple_intropattern =
  let module M = Ser_genarg.GS(A1) in M.genser

module A2 = struct
  type raw = Constrexpr.constr_pattern_expr Tactypes.with_bindings Ser_tactics.destruction_arg
  [@@deriving sexp,hash,compare]
  type glb = Genintern.glob_constr_and_expr Tactypes.with_bindings Ser_tactics.destruction_arg
  [@@deriving sexp,hash,compare]
  type top = Tactypes.delayed_open_constr_with_bindings Ser_tactics.destruction_arg
  [@@deriving sexp,hash,compare]
end

let ser_wit_destruction_arg =
  let module M = Ser_genarg.GS(A2) in M.genser

module A3 = struct
  type raw = Tacexpr.raw_tactic_expr
  [@@deriving sexp,hash,compare]
  type glb = Tacexpr.glob_tactic_expr
  [@@deriving sexp,hash,compare]
  type top = tacvalue
  [@@deriving sexp,hash,compare]
end

let ser_wit_tactic = let module M = Ser_genarg.GS(A3) in M.genser

module A4 = struct
  type raw = Tacexpr.raw_tactic_expr
  [@@deriving sexp,hash,yojson,compare]
  type glb = Tacexpr.glob_tactic_expr
  [@@deriving sexp,hash,yojson,compare]
end

let ser_wit_ltac = let module M = Ser_gentactic.GS(A4) in M.genser

module A5 = struct
  type t = Ser_tactypes.quantified_hypothesis [@@deriving sexp,hash,compare]
end

let ser_wit_quant_hyp = let module M = Ser_genarg.GS0(A5) in M.genser

module A6 = struct
  type raw = Constrexpr.constr_expr Tactypes.bindings
  [@@deriving sexp,hash,compare]
  type glb = Genintern.glob_constr_and_expr Tactypes.bindings
  [@@deriving sexp,hash,compare]
  type top = EConstr.constr Tactypes.bindings Tactypes.delayed_open
  [@@deriving sexp,hash,compare]
end

let ser_wit_bindings = let module M = Ser_genarg.GS(A6) in M.genser

module A7 = struct
  type raw = Constrexpr.constr_expr Tactypes.with_bindings
  [@@deriving sexp,hash,compare]
  type glb = Genintern.glob_constr_and_expr Tactypes.with_bindings
  [@@deriving sexp,hash,compare]
  type top = EConstr.constr Tactypes.with_bindings Tactypes.delayed_open
  [@@deriving sexp,hash,compare]
end

let ser_wit_constr_with_bindings = let module M = Ser_genarg.GS(A7) in M.genser

(* G_ltac *)
(* Defined in g_ltac but serialized here *)

module A8 = struct
  type t = int
  [@@deriving sexp,hash,compare]
end

let ser_wit_ltac_info = let module M = Ser_genarg.GSV(A8) in M.genser

module A9 = struct
  type t = Ltac_plugin.Tacentries.raw_argument Ser_tacentries.grammar_tactic_prod_item_expr
  [@@deriving sexp,hash,compare]
end

let ser_wit_production_item = let module M = Ser_genarg.GSV(A9) in M.genser

module A10 = struct
  type t = string
  [@@deriving sexp,hash,compare]
end
let ser_wit_ltac_production_sep = let module M = Ser_genarg.GSV(A10) in M.genser

module A11 = struct
  type t = Ser_goal_select.t
  [@@deriving sexp,hash,compare]
end
let ser_wit_ltac_selector = let module M = Ser_genarg.GSV(A11) in M.genser

module A12 = struct
  type t = Ser_tacexpr.tacdef_body
  [@@deriving sexp,hash,compare]
end
let ser_wit_ltac_tacdef_body = let module M = Ser_genarg.GSV(A12) in M.genser

module A13 = struct
  type t = int  [@@deriving sexp,hash,compare]
end
let ser_wit_ltac_tactic_level = let module M = Ser_genarg.GSV(A13) in M.genser

module A14 = struct
  type t = bool CAst.t
  [@@deriving sexp,hash,compare]
end
let ser_wit_ltac_use_default = let module M = Ser_genarg.GSV(A14) in M.genser

(* From G_auto *)
module A15 = struct
  type raw = Ser_constrexpr.constr_expr list
  [@@deriving sexp,hash,compare]
  type glb = Ser_genintern.glob_constr_and_expr list
  [@@deriving sexp,hash,compare]
  type top = Ser_ltac_pretype.closed_glob_constr list
  [@@deriving sexp,hash,compare]
end
let ser_wit_auto_using = let module M = Ser_genarg.GS(A15) in M.genser

module A16 = struct
  type raw = string list option
  [@@deriving sexp,hash,compare]
  type glb = string list option
  [@@deriving sexp,hash,compare]
  type top = string list option
  [@@deriving sexp,hash,compare]
end
let ser_wit_hintbases = let module M = Ser_genarg.GS(A16) in M.genser

module A17 = struct
  type t = Ser_libnames.qualid Ser_hints.hints_path_gen
  [@@deriving sexp,hash,compare]
end
let ser_wit_hintbases_path = let module M = Ser_genarg.GSV(A17) in M.genser

module A19 = struct
  type raw = string list option
  [@@deriving sexp,hash,compare]
  type glb = string list option
  [@@deriving sexp,hash,compare]
  type top = string list option
  [@@deriving sexp,hash,compare]
end

let ser_wit_opthints = let module M = Ser_genarg.GS(A19) in M.genser

(* G_rewrite *)

module G_rewrite = struct

let wit_binders = Ltac_plugin.G_rewrite.wit_binders

module GT0 = struct
  type t =
    [%import: Ltac_plugin.G_rewrite.binders_argtype]
  [@@deriving sexp,hash,compare]
end

let ser_wit_binders = let module M = Ser_genarg.GSV(GT0) in M.genser

let wit_rewstrategy = Ltac_plugin.G_rewrite.wit_rewstrategy

module GT2 = struct
  type raw = Ltac_plugin.Tacexpr.raw_strategy
  [@@deriving sexp,hash,compare]
  type glb = Ltac_plugin.Tacexpr.glob_strategy
  [@@deriving sexp,hash,compare]
  type top = Ltac_plugin.RewriteStratAst.strategy
  [@@deriving sexp,hash,compare]
end


(* (Tacexpr.raw_strategy, Tacexpr.glob_strategy, Rewrite.strategy) *)

let ser_wit_rewstrategy = let module M = Ser_genarg.GS(GT2) in M.genser

end

let ser_wit_debug =
  let open Sexplib.Conv in
  Ser_genarg.mk_uniform sexp_of_bool bool_of_sexp hash_fold_bool compare_bool

module SWESS = struct
  type t = Ser_class_tactics.search_strategy option
  [@@deriving sexp,hash,compare]
end
let ser_wit_eauto_search_strategy = let module M = Ser_genarg.GS0(SWESS) in M.genser

module SWWT = struct
  type t = Gentactic.raw_generic_tactic option
  [@@deriving sexp,hash,compare]
end
let ser_wit_withtac = let module M = Ser_genarg.GSV(SWWT) in M.genser

(* extraargs *)

type 'a gen_place =
  [%import: 'a Ltac_plugin.Extraargs.gen_place]
  [@@deriving sexp,hash,compare]

type loc_place =
  [%import: Ltac_plugin.Extraargs.loc_place]
  [@@deriving sexp,hash,compare]

type place =
  [%import: Ltac_plugin.Extraargs.place]
  [@@deriving sexp,hash,compare]

module GT3 = struct
  type raw = loc_place
  [@@deriving sexp,hash,compare]
  type glb = loc_place
  [@@deriving sexp,hash,compare]
  type top = place
  [@@deriving sexp,hash,compare]
end

let ser_wit_hloc = let module M = Ser_genarg.GS(GT3) in M.genser

module GT4 = struct
  type raw = Constrexpr.constr_expr
  [@@deriving sexp,hash,compare]
  type glb = Genintern.glob_constr_and_expr
  [@@deriving sexp,hash,compare]
  type top = Ltac_pretype.closed_glob_constr
  [@@deriving sexp,hash,compare]
end

let ser_wit_lglob = let module M = Ser_genarg.GS(GT4) in M.genser

let ser_wit_orient =
  let open Sexplib.Conv in
  Ser_genarg.mk_uniform sexp_of_bool bool_of_sexp hash_fold_bool compare_bool

module WRen = struct
  type t = Names.Id.t * Names.Id.t
  [@@deriving sexp,hash,compare]
end

let ser_wit_rename =
  let module M = Ser_genarg.GS0(WRen) in M.genser

let ser_wit_natural =
  let open Sexplib.Conv in
  Ser_genarg.mk_uniform sexp_of_int int_of_sexp hash_fold_int compare_int

module GT5 = struct
  type raw = Constrexpr.constr_expr
  [@@deriving sexp,hash,compare]
  type glb = Genintern.glob_constr_and_expr
  [@@deriving sexp,hash,compare]
  type top = EConstr.t
  [@@deriving sexp,hash,compare]
end

let ser_wit_lconstr : (Constrexpr.constr_expr, Ser_genintern.glob_constr_and_expr, EConstr.t) Ser_genarg.gen_ser =
  let module M = Ser_genarg.GS(GT5) in M.genser

(* let _ser_wit_casted_constr : (Constrexpr.constr_expr, Ser_genintern.glob_constr_and_expr, EConstr.t) Ser_genarg.gen_ser =
 *   Ser_genarg.{
 *     raw_ser = Ser_constrexpr.sexp_of_constr_expr
 *   ; glb_ser = Ser_genintern.sexp_of_glob_constr_and_expr
 *   ; top_ser = Ser_eConstr.sexp_of_t
 * 
 *   ; raw_des = Ser_constrexpr.constr_expr_of_sexp
 *   ; glb_des = Ser_genintern.glob_constr_and_expr_of_sexp
 *   ; top_des = Ser_eConstr.t_of_sexp
 *   } *)

module GT6 = struct
  type raw = Names.lident Locus.clause_expr
  [@@deriving sexp,hash,compare]
  type glb = Names.lident Locus.clause_expr
  [@@deriving sexp,hash,compare]
  type top = Names.Id.t Locus.clause_expr
  [@@deriving sexp,hash,compare]
end

let ser_wit_in_clause :
  (Names.lident Locus.clause_expr, Names.lident Locus.clause_expr, Names.Id.t Locus.clause_expr) Ser_genarg.gen_ser =
  let module M = Ser_genarg.GS(GT6) in M.genser

module GT7 = struct
  type raw = Tacexpr.raw_tactic_expr option
  [@@deriving sexp,hash,compare]
  type glb = Tacexpr.glob_tactic_expr option
  [@@deriving sexp,hash,compare]
  type top = tacvalue option
  [@@deriving sexp,hash,compare]
end

let ser_wit_by_arg_tac =
  let module M = Ser_genarg.GS(GT7) in M.genser

let ser_wit_lpar_id_colon =
  let open Sexplib.Conv in
  Ser_genarg.mk_uniform sexp_of_unit unit_of_sexp hash_fold_unit compare_unit

module GT8 = struct
  type raw = int list Locus.or_var
  [@@deriving sexp,hash,compare]
  type glb = int list Locus.or_var
  [@@deriving sexp,hash,compare]
  type top = int list
  [@@deriving sexp,hash,compare]
end

let ser_wit_occurences = let module M = Ser_genarg.GS(GT8) in M.genser

let register () =

  Ser_genarg.register_genser Tacarg.wit_bindings ser_wit_bindings;
  Ser_genarg.register_genser Tacarg.wit_constr_with_bindings      ser_wit_constr_with_bindings;
  Ser_genarg.register_genser Tacarg.wit_open_constr_with_bindings ser_wit_constr_with_bindings;
  Ser_genarg.register_genser Tacarg.wit_destruction_arg ser_wit_destruction_arg;
  Ser_genarg.register_genser Tacarg.wit_simple_intropattern  ser_wit_simple_intropattern;
  (* Ser_genarg.register_genser Tacarg.wit_intro_pattern ser_wit_intropattern; *)
  Ser_gentactic.register Tacarg.wit_ltac ser_wit_ltac;
  Ser_genarg.register_genser Tacarg.wit_quant_hyp ser_wit_quant_hyp;
  (* Ser_genarg.register_genser Tacarg.wit_quantified_hypothesis ser_wit_quant_hyp; *)
  Ser_genarg.register_genser Tacarg.wit_tactic ser_wit_tactic;

  Ser_genarg.register_genser G_ltac.wit_ltac_info ser_wit_ltac_info;
  Ser_genarg.register_genser G_ltac.wit_ltac_production_item ser_wit_production_item;
  Ser_genarg.register_genser G_ltac.wit_ltac_production_sep ser_wit_ltac_production_sep;
  Ser_genarg.register_genser G_ltac.wit_ltac_selector ser_wit_ltac_selector;
  Ser_genarg.register_genser G_ltac.wit_ltac_tacdef_body ser_wit_ltac_tacdef_body;
  Ser_genarg.register_genser G_ltac.wit_ltac_tactic_level ser_wit_ltac_tactic_level;
  Ser_genarg.register_genser G_ltac.wit_ltac_use_default ser_wit_ltac_use_default;

  Ser_genarg.register_genser G_auto.wit_auto_using ser_wit_auto_using;
  Ser_genarg.register_genser G_auto.wit_hintbases ser_wit_hintbases;
  Ser_genarg.register_genser G_auto.wit_hints_path_entry ser_wit_hintbases_path;
  Ser_genarg.register_genser G_auto.wit_opthints ser_wit_opthints;

  Ser_genarg.register_genser G_rewrite.wit_binders G_rewrite.ser_wit_binders;
  Ser_genarg.register_genser G_rewrite.wit_rewstrategy G_rewrite.ser_wit_rewstrategy;

  Ser_genarg.register_genser G_class.wit_debug ser_wit_debug;
  Ser_genarg.register_genser G_class.wit_eauto_search_strategy ser_wit_eauto_search_strategy;

  Ser_genarg.register_genser G_obligations.wit_withtac ser_wit_withtac;

  Ser_genarg.register_genser Extraargs.wit_by_arg_tac ser_wit_by_arg_tac;
  (* XXX: seems gone from Coq *)
  (* Ser_genarg.register_genser Extraargs.wit_casted_constr ser_wit_casted_constr; *)
  Ser_genarg.register_genser Extraargs.wit_glob ser_wit_lglob;
  Ser_genarg.register_genser Extraargs.wit_hloc ser_wit_hloc;
  Ser_genarg.register_genser Extraargs.wit_in_clause ser_wit_in_clause;
  Ser_genarg.register_genser Extraargs.wit_lconstr ser_wit_lconstr;
  Ser_genarg.register_genser Extraargs.wit_lglob ser_wit_lglob;
  Ser_genarg.register_genser Extraargs.wit_natural ser_wit_natural;
  Ser_genarg.register_genser Extraargs.wit_orient ser_wit_orient;
  Ser_genarg.register_genser Extraargs.wit_occurrences ser_wit_occurences;
  Ser_genarg.register_genser Extraargs.wit_rename ser_wit_rename;
  Ser_genarg.register_genser Extraargs.wit_test_lpar_id_colon ser_wit_lpar_id_colon;
  ()

let () = register ()
