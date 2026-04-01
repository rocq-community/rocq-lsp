(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   VV/  **************************************************************)
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

open Sexplib.Conv

module Names      = Ser_names
module Genredexpr = Ser_genredexpr

let ser_wit_unit   = Ser_genarg.mk_uniform sexp_of_unit unit_of_sexp Ppx_hash_lib.Std.Hash.Builtin.hash_fold_unit Ppx_compare_lib.Builtin.compare_unit
let ser_wit_bool   = Ser_genarg.mk_uniform sexp_of_bool bool_of_sexp Ppx_hash_lib.Std.Hash.Builtin.hash_fold_bool Ppx_compare_lib.Builtin.compare_bool
let ser_wit_int    = Ser_genarg.mk_uniform sexp_of_int int_of_sexp Ppx_hash_lib.Std.Hash.Builtin.hash_fold_int Ppx_compare_lib.Builtin.compare_int
let ser_wit_string = Ser_genarg.mk_uniform sexp_of_string string_of_sexp Ppx_hash_lib.Std.Hash.Builtin.hash_fold_string Ppx_compare_lib.Builtin.compare_string
let ser_wit_pre_ident = Ser_genarg.mk_uniform sexp_of_string string_of_sexp Ppx_hash_lib.Std.Hash.Builtin.hash_fold_string Ppx_compare_lib.Builtin.compare_string

let ser_wit_hyp : (Ser_names.lident, Ser_names.lident, Ser_names.Id.t) Ser_genarg.gen_ser = Ser_genarg.
  { raw_ser = Ser_names.sexp_of_lident
  ; glb_ser = Ser_names.sexp_of_lident
  ; top_ser = Ser_names.Id.sexp_of_t

  ; raw_des = Ser_names.lident_of_sexp
  ; glb_des = Ser_names.lident_of_sexp
  ; top_des = Ser_names.Id.t_of_sexp

  ; raw_hash = Ser_names.hash_fold_lident
  ; glb_hash = Ser_names.hash_fold_lident
  ; top_hash = Ser_names.Id.hash_fold_t

  ; raw_compare = Ser_names.compare_lident
  ; glb_compare = Ser_names.compare_lident
  ; top_compare = Ser_names.Id.compare
  }


module IDR = struct
  type t = Names.lident
  [@@deriving sexp,hash,compare]
end

let ser_wit_identref =
  let module M = Ser_genarg.GSV(IDR) in M.genser

let ser_wit_nat_or_var = Ser_genarg.
  { raw_ser = Ser_locus.sexp_of_or_var sexp_of_int
  ; glb_ser = Ser_locus.sexp_of_or_var sexp_of_int
  ; top_ser = sexp_of_int

  ; raw_des = Ser_locus.or_var_of_sexp int_of_sexp
  ; glb_des = Ser_locus.or_var_of_sexp int_of_sexp
  ; top_des = int_of_sexp

  ; raw_hash = Ser_locus.hash_fold_or_var Ppx_hash_lib.Std.Hash.Builtin.hash_fold_int
  ; glb_hash = Ser_locus.hash_fold_or_var Ppx_hash_lib.Std.Hash.Builtin.hash_fold_int
  ; top_hash = Ppx_hash_lib.Std.Hash.Builtin.hash_fold_int

  ; raw_compare = Ser_locus.compare_or_var Ppx_compare_lib.Builtin.compare_int
  ; glb_compare = Ser_locus.compare_or_var Ppx_compare_lib.Builtin.compare_int
  ; top_compare = Ppx_compare_lib.Builtin.compare_int
  }

let ser_wit_int_or_var = Ser_genarg.
  { raw_ser = Ser_locus.sexp_of_or_var sexp_of_int
  ; glb_ser = Ser_locus.sexp_of_or_var sexp_of_int
  ; top_ser = sexp_of_int

  ; raw_des = Ser_locus.or_var_of_sexp int_of_sexp
  ; glb_des = Ser_locus.or_var_of_sexp int_of_sexp
  ; top_des = int_of_sexp

  ; raw_hash = Ser_locus.hash_fold_or_var Ppx_hash_lib.Std.Hash.Builtin.hash_fold_int
  ; glb_hash = Ser_locus.hash_fold_or_var Ppx_hash_lib.Std.Hash.Builtin.hash_fold_int
  ; top_hash = Ppx_hash_lib.Std.Hash.Builtin.hash_fold_int

  ; raw_compare = Ser_locus.compare_or_var Ppx_compare_lib.Builtin.compare_int
  ; glb_compare = Ser_locus.compare_or_var Ppx_compare_lib.Builtin.compare_int
  ; top_compare = Ppx_compare_lib.Builtin.compare_int
  }

let ser_wit_ident  = Ser_genarg.mk_uniform Ser_names.Id.sexp_of_t Ser_names.Id.t_of_sexp Ser_names.Id.hash_fold_t Ser_names.Id.compare

let ser_wit_ref = Ser_genarg.{
    raw_ser = Ser_libnames.sexp_of_qualid
  ; glb_ser = Ser_locus.sexp_of_or_var Ser_loc.(sexp_of_located Ser_names.GlobRef.sexp_of_t)
  ; top_ser = Ser_names.GlobRef.sexp_of_t

  ; raw_des = Ser_libnames.qualid_of_sexp
  ; glb_des = Ser_locus.or_var_of_sexp Ser_loc.(located_of_sexp Ser_names.GlobRef.t_of_sexp)
  ; top_des = Ser_names.GlobRef.t_of_sexp

  ; raw_hash = Ser_libnames.hash_fold_qualid
  ; glb_hash = Ser_locus.hash_fold_or_var Ser_loc.(hash_fold_located Ser_names.GlobRef.hash_fold_t)
  ; top_hash = Ser_names.GlobRef.hash_fold_t

  ; raw_compare = Ser_libnames.compare_qualid
  ; glb_compare = Ser_locus.compare_or_var Ser_loc.(compare_located Ser_names.GlobRef.compare)
  ; top_compare = Ser_names.GlobRef.compare

  }

let ser_wit_sort_quality_or_set = Ser_genarg.{
    raw_ser = Ser_univGen.QualityOrSet.sexp_of_t
  ; glb_ser = sexp_of_unit
  ; top_ser = sexp_of_unit

  ; raw_des = Ser_univGen.QualityOrSet.t_of_sexp
  ; glb_des = unit_of_sexp
  ; top_des = unit_of_sexp

  ; raw_hash = Ser_univGen.QualityOrSet.hash_fold_t
  ; glb_hash = Ppx_hash_lib.Std.Hash.Builtin.hash_fold_unit
  ; top_hash = Ppx_hash_lib.Std.Hash.Builtin.hash_fold_unit

  ; raw_compare = Ser_univGen.QualityOrSet.compare
  ; glb_compare = Ppx_compare_lib.Builtin.compare_unit
  ; top_compare = Ppx_compare_lib.Builtin.compare_unit
  }

(* let ser_ref  *)

let ser_wit_constr = Ser_genarg.{
    raw_ser = Ser_constrexpr.sexp_of_constr_expr
  ; glb_ser = Ser_genintern.sexp_of_glob_constr_and_expr
  ; top_ser = Ser_eConstr.sexp_of_t

  ; raw_des = Ser_constrexpr.constr_expr_of_sexp
  ; glb_des = Ser_genintern.glob_constr_and_expr_of_sexp
  ; top_des = Ser_eConstr.t_of_sexp

  ; raw_hash = Ser_constrexpr.hash_fold_constr_expr
  ; glb_hash = Ser_genintern.hash_fold_glob_constr_and_expr
  ; top_hash = Ser_eConstr.hash_fold_t

  ; raw_compare = Ser_constrexpr.compare_constr_expr
  ; glb_compare = Ser_genintern.compare_glob_constr_and_expr
  ; top_compare = Ser_eConstr.compare
  }

let ser_wit_uconstr = Ser_genarg.{
    raw_ser = Ser_constrexpr.sexp_of_constr_expr
  ; glb_ser = Ser_genintern.sexp_of_glob_constr_and_expr
  ; top_ser = Ser_ltac_pretype.sexp_of_closed_glob_constr

  ; raw_des = Ser_constrexpr.constr_expr_of_sexp
  ; glb_des = Ser_genintern.glob_constr_and_expr_of_sexp
  ; top_des = Ser_ltac_pretype.closed_glob_constr_of_sexp

  ; raw_hash = Ser_constrexpr.hash_fold_constr_expr
  ; glb_hash = Ser_genintern.hash_fold_glob_constr_and_expr
  ; top_hash = Ser_ltac_pretype.hash_fold_closed_glob_constr

  ; raw_compare = Ser_constrexpr.compare_constr_expr
  ; glb_compare = Ser_genintern.compare_glob_constr_and_expr
  ; top_compare = Ser_ltac_pretype.compare_closed_glob_constr
  }

let ser_wit_clause_dft_concl = Ser_genarg.{
    raw_ser = Ser_locus.sexp_of_clause_expr Ser_names.sexp_of_lident
  ; glb_ser = Ser_locus.sexp_of_clause_expr Ser_names.sexp_of_lident
  ; top_ser = Ser_locus.sexp_of_clause_expr Ser_names.Id.sexp_of_t

  ; raw_des = Ser_locus.clause_expr_of_sexp Ser_names.lident_of_sexp
  ; glb_des = Ser_locus.clause_expr_of_sexp Ser_names.lident_of_sexp
  ; top_des = Ser_locus.clause_expr_of_sexp Ser_names.Id.t_of_sexp

  ; raw_hash = Ser_locus.hash_fold_clause_expr Ser_names.hash_fold_lident
  ; glb_hash = Ser_locus.hash_fold_clause_expr Ser_names.hash_fold_lident
  ; top_hash = Ser_locus.hash_fold_clause_expr Ser_names.Id.hash_fold_t

  ; raw_compare = Ser_locus.compare_clause_expr Ser_names.compare_lident
  ; glb_compare = Ser_locus.compare_clause_expr Ser_names.compare_lident
  ; top_compare = Ser_locus.compare_clause_expr Ser_names.Id.compare

  }

let register () =

  Ser_genarg.register_genser Stdarg.wit_bool ser_wit_bool;
  Ser_genarg.register_genser Stdarg.wit_clause_dft_concl ser_wit_clause_dft_concl;
  Ser_genarg.register_genser Stdarg.wit_constr ser_wit_constr;
  Ser_genarg.register_genser Stdarg.wit_ident ser_wit_ident;
  Ser_genarg.register_genser Stdarg.wit_identref ser_wit_identref;
  Ser_genarg.register_genser Stdarg.wit_hyp ser_wit_hyp;
  Ser_genarg.register_genser Stdarg.wit_int ser_wit_int;
  Ser_genarg.register_genser Stdarg.wit_int_or_var ser_wit_int_or_var;
  Ser_genarg.register_genser Stdarg.wit_nat_or_var ser_wit_nat_or_var;
  Ser_genarg.register_genser Stdarg.wit_open_constr ser_wit_constr;
  Ser_genarg.register_genser Stdarg.wit_pre_ident ser_wit_pre_ident;
  Ser_genarg.register_genser Stdarg.wit_ref ser_wit_ref;
  Ser_genarg.register_genser Stdarg.wit_sort_quality_or_set ser_wit_sort_quality_or_set;
  Ser_genarg.register_genser Stdarg.wit_string ser_wit_string;
  Ser_genarg.register_genser Stdarg.wit_uconstr ser_wit_uconstr;
  Ser_genarg.register_genser Stdarg.wit_unit ser_wit_unit;

  ()

let _ = register ()
