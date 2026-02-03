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

open Sexplib

type 'a or_by_notation = 'a Constrexpr.or_by_notation [@@deriving sexp, yojson, hash,compare]

type 'custom notation_entry_gen = 'custom Constrexpr.notation_entry_gen [@@deriving sexp, yojson, hash,compare]

type notation_entry = Constrexpr.notation_entry [@@deriving sexp, yojson, hash,compare]

type entry_level = Constrexpr.entry_level [@@deriving sexp, yojson, hash,compare]

type notation_entry_level = Constrexpr.notation_entry_level [@@deriving sexp, yojson, hash,compare]

type entry_relative_level = Constrexpr.entry_relative_level [@@deriving sexp, yojson, hash,compare]

type notation_entry_relative_level = Constrexpr.notation_entry_relative_level [@@deriving sexp, yojson, hash,compare]

type universe_decl_expr = Constrexpr.universe_decl_expr [@@deriving sexp, yojson, hash,compare]
type ident_decl = Constrexpr.ident_decl [@@deriving sexp, yojson, hash,compare]
type cumul_ident_decl = Constrexpr.cumul_ident_decl [@@deriving sexp, yojson, hash,compare]
type univ_constraint_expr = Constrexpr.univ_constraint_expr [@@deriving sexp, yojson, hash,compare]
type sort_constraint_expr = Constrexpr.sort_constraint_expr [@@deriving sexp, yojson, hash,compare]
type name_decl = Constrexpr.name_decl [@@deriving sexp, yojson, hash,compare]

type notation_with_optional_scope = Constrexpr.notation_with_optional_scope [@@deriving sexp, yojson, hash,compare]

type side = Constrexpr.side [@@deriving sexp, yojson, hash,compare]

type notation = Constrexpr.notation

val notation_of_sexp : Sexp.t -> notation
val sexp_of_notation : notation -> Sexp.t

type explicitation = Constrexpr.explicitation

val explicitation_of_sexp : Sexp.t -> explicitation
val sexp_of_explicitation : explicitation -> Sexp.t

type binder_kind = Constrexpr.binder_kind

val binder_kind_of_sexp : Sexp.t -> binder_kind
val sexp_of_binder_kind : binder_kind -> Sexp.t

(* type abstraction_kind = Constrexpr.abstraction_kind
 * 
 * val abstraction_kind_of_sexp : Sexp.t -> abstraction_kind
 * val sexp_of_abstraction_kind : abstraction_kind -> Sexp.t *)

(* type proj_flag = Constrexpr.proj_flag
 * 
 * val proj_flag_of_sexp : Sexp.t -> proj_flag
 * val sexp_of_proj_flag : proj_flag -> Sexp.t *)

type delimiter_depth = [%import: Constrexpr.delimiter_depth]
  [@@deriving sexp,yojson,hash,compare]

type prim_token = Constrexpr.prim_token

val prim_token_of_sexp : Sexp.t -> prim_token
val sexp_of_prim_token : prim_token -> Sexp.t

type cases_pattern_expr = Constrexpr.cases_pattern_expr
and cases_pattern_notation_substitution = Constrexpr.cases_pattern_notation_substitution

val cases_pattern_expr_of_sexp : Sexp.t -> cases_pattern_expr
val cases_pattern_notation_substitution_of_sexp : Sexp.t -> cases_pattern_notation_substitution

val sexp_of_cases_pattern_expr : cases_pattern_expr -> Sexp.t
val sexp_of_cases_pattern_notation_substitution : cases_pattern_notation_substitution -> Sexp.t

type instance_expr = Constrexpr.instance_expr

val instance_expr_of_sexp : Sexp.t -> instance_expr
val sexp_of_instance_expr : instance_expr -> Sexp.t

type constr_expr         = Constrexpr.constr_expr
and case_expr            = Constrexpr.case_expr
and branch_expr          = Constrexpr.branch_expr
and fix_expr             = Constrexpr.fix_expr
and cofix_expr           = Constrexpr.cofix_expr
and fixpoint_order_expr = Constrexpr.fixpoint_order_expr
and local_binder_expr    = Constrexpr.local_binder_expr
and constr_notation_substitution = Constrexpr.constr_notation_substitution
[@@deriving sexp, yojson, hash,compare]

type constr_pattern_expr = Constrexpr.constr_pattern_expr [@@deriving sexp,yojson,hash,compare]

type with_declaration_ast = Constrexpr.with_declaration_ast [@@deriving sexp,yojson,hash,compare]

type module_ast = Constrexpr.module_ast [@@deriving sexp,yojson,hash,compare]
