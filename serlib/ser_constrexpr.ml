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

open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

let hash_fold_array = Ppx_hash_lib.Std.Hash.Builtin.hash_fold_array_frozen

open Sexplib.Std

module Loc        = Ser_loc
module CAst       = Ser_cAst
module Names      = Ser_names
module Constr     = Ser_constr
module UState     = Ser_uState
module Namegen    = Ser_namegen
module Pattern    = Ser_pattern
module Evar_kinds = Ser_evar_kinds
module Genarg     = Ser_genarg
module Libnames   = Ser_libnames
module Glob_term  = Ser_glob_term
module NumTok     = Ser_numTok
module Univ       = Ser_univ
module UVars      = Ser_uvars
module Sorts      = Ser_sorts
module Globnames  = Ser_globnames

type sort_name_expr =
  [%import: Constrexpr.sort_name_expr]
  [@@deriving sexp,yojson,hash,compare]

type univ_level_expr =
  [%import: Constrexpr.univ_level_expr]
  [@@deriving sexp,yojson,hash,compare]

type qvar_expr =
  [%import: Constrexpr.qvar_expr]
  [@@deriving sexp,yojson,hash,compare]

type quality_expr =
  [%import: Constrexpr.quality_expr]
  [@@deriving sexp,yojson,hash,compare]

type relevance_expr =
  [%import: Constrexpr.relevance_expr]
  [@@deriving sexp,yojson,hash,compare]

type relevance_info_expr =
  [%import: Constrexpr.relevance_info_expr]
  [@@deriving sexp,yojson,hash,compare]

type sort_expr =
  [%import: Constrexpr.sort_expr]
  [@@deriving sexp,yojson,hash,compare
]
type univ_constraint_expr =
  [%import: Constrexpr.univ_constraint_expr]
  [@@deriving sexp,yojson,hash,compare]

type elim_constraint_expr =
  [%import: Constrexpr.elim_constraint_expr]
  [@@deriving sexp,yojson,hash,compare]

type sort_constraint_expr =
  [%import: Constrexpr.sort_constraint_expr]
  [@@deriving sexp,yojson,hash,compare]

type instance_expr =
   [%import: Constrexpr.instance_expr]
  [@@deriving sexp,yojson,hash,compare]

type 'a or_by_notation_r =
  [%import: 'a Constrexpr.or_by_notation_r]
  [@@deriving sexp,yojson,hash,compare]

type 'a or_by_notation =
  [%import: 'a Constrexpr.or_by_notation]
  [@@deriving sexp,yojson,hash,compare]

type universe_decl_expr =
  [%import: Constrexpr.universe_decl_expr]
  [@@deriving sexp,yojson,hash,compare]

type ident_decl =
  [%import: Constrexpr.ident_decl]
  [@@deriving sexp,yojson,hash,compare]

type cumul_univ_decl_expr =
  [%import: Constrexpr.cumul_univ_decl_expr]
  [@@deriving sexp,yojson,hash,compare]

type cumul_ident_decl =
  [%import: Constrexpr.cumul_ident_decl]
  [@@deriving sexp,yojson,hash,compare]

type name_decl =
  [%import: Constrexpr.name_decl]
  [@@deriving sexp,yojson,hash,compare]

type notation_with_optional_scope =
  [%import: Constrexpr.notation_with_optional_scope]
  [@@deriving sexp,yojson,hash,compare]

type side =
  [%import: Constrexpr.side]
  [@@deriving sexp,yojson,hash,compare]

type 'name notation_entry_gen =
  [%import: 'name Constrexpr.notation_entry_gen]
  [@@deriving sexp,yojson,hash,compare]

type notation_entry =
  [%import: Constrexpr.notation_entry]
  [@@deriving sexp,yojson,hash,compare]

type entry_level =
  [%import: Constrexpr.entry_level]
  [@@deriving sexp,yojson,hash,compare]

type entry_relative_level =
  [%import: Constrexpr.entry_relative_level]
  [@@deriving sexp,yojson,hash,compare]

type notation_entry_level =
  [%import: Constrexpr.notation_entry_level]
  [@@deriving sexp,yojson,hash,compare]

type notation_entry_relative_level =
  [%import: Constrexpr.notation_entry_relative_level]
  [@@deriving sexp,yojson,hash,compare]

type notation_key =
  [%import: Constrexpr.notation_key]
  [@@deriving sexp,yojson,hash,compare]

type notation =  [%import: Constrexpr.notation]
  [@@deriving sexp,yojson,hash,compare]

type explicitation = [%import: Constrexpr.explicitation]
  [@@deriving sexp,yojson,hash,compare]

type binder_kind = [%import: Constrexpr.binder_kind]
  [@@deriving sexp,yojson,hash,compare]

type explicit_flag = [%import: Constrexpr.explicit_flag]
  [@@deriving sexp,yojson,hash,compare]

(* type abstraction_kind = [%import: Constrexpr.abstraction_kind]
 *   [@@deriving sexp,yojson] *)

(* type proj_flag = [%import: Constrexpr.proj_flag]
 *   [@@deriving sexp,yojson] *)

(* type raw_numeral = [%import: Constrexpr.raw_numeral]
 *   [@@deriving sexp,yojson] *)

(* type sign = [%import: Constrexpr.sign]
 *   [@@deriving sexp,yojson] *)

type delimiter_depth = [%import: Constrexpr.delimiter_depth]
  [@@deriving sexp,yojson,hash,compare]

type prim_token = [%import: Constrexpr.prim_token]
  [@@deriving sexp,yojson,hash,compare]

type cases_pattern_expr_r = [%import: Constrexpr.cases_pattern_expr_r]
and cases_pattern_expr = [%import: Constrexpr.cases_pattern_expr]
and kinded_cases_pattern_expr = [%import: Constrexpr.kinded_cases_pattern_expr]
and cases_pattern_notation_substitution = [%import: Constrexpr.cases_pattern_notation_substitution]
and constr_expr_r = [%import: Constrexpr.constr_expr_r]
and constr_expr = [%import: Constrexpr.constr_expr]
and case_expr   = [%import: Constrexpr.case_expr]
and branch_expr = [%import: Constrexpr.branch_expr]
and fix_expr    = [%import: Constrexpr.fix_expr]
and cofix_expr  = [%import: Constrexpr.cofix_expr]
and fixpoint_order_expr_r = [%import: Constrexpr.fixpoint_order_expr_r]
and fixpoint_order_expr = [%import: Constrexpr.fixpoint_order_expr]
and local_binder_expr    = [%import: Constrexpr.local_binder_expr]
and constr_notation_substitution = [%import: Constrexpr.constr_notation_substitution]
  [@@deriving sexp,yojson,hash,compare]

type constr_pattern_expr = [%import: Constrexpr.constr_pattern_expr]
  [@@deriving sexp,yojson,hash,compare]

type with_declaration_ast =
  [%import: Constrexpr.with_declaration_ast]
  [@@deriving sexp,yojson,hash,compare]

type module_ast_r = [%import: Constrexpr.module_ast_r]
and module_ast =
  [%import: Constrexpr.module_ast]
  [@@deriving sexp,yojson,hash,compare]
