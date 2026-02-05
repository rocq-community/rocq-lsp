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

type infix_flag =
  [%import: Vernacexpr.infix_flag]
  [@@deriving sexp,yojson,hash,compare]

type opacity_flag =
  [%import: Vernacexpr.opacity_flag]
  [@@deriving sexp,yojson,hash,compare]

type scope_name =
  [%import: Vernacexpr.scope_name]
  [@@deriving sexp,yojson,hash,compare]

type scope_delimiter =
  [%import: Vernacexpr.scope_delimiter]
  [@@deriving sexp,yojson,hash,compare]

type notation_format =
  [%import: Vernacexpr.notation_format]
  [@@deriving sexp,yojson,hash,compare]

type syntax_modifier =
  [%import: Vernacexpr.syntax_modifier]
  [@@deriving sexp,yojson,hash,compare]

type coercion_class =
  [%import: Vernacexpr.coercion_class]
  [@@deriving sexp,yojson,hash,compare]

type goal_reference =
  [%import: Vernacexpr.goal_reference]
  [@@deriving sexp,yojson,hash,compare]

type debug_univ_name =
  [%import: Vernacexpr.debug_univ_name]
  [@@deriving sexp,yojson,hash,compare]

type print_universes =
  [%import: Vernacexpr.print_universes]
  [@@deriving sexp,yojson,hash,compare]

type printable =
  [%import: Vernacexpr.printable]
  [@@deriving sexp,yojson,hash,compare]

type glob_search_where =
  [%import: Vernacexpr.glob_search_where]
  [@@deriving sexp,yojson,hash,compare]

type search_item =
  [%import: Vernacexpr.search_item]
  [@@deriving sexp,yojson,hash,compare]

type search_request =
  [%import: Vernacexpr.search_request]
  [@@deriving sexp,yojson,hash,compare]

type searchable =
  [%import: Vernacexpr.searchable]
  [@@deriving sexp,yojson,hash,compare]

type locatable =
  [%import: Vernacexpr.locatable]
  [@@deriving sexp,yojson,hash,compare]

type showable =
  [%import: Vernacexpr.showable]
  [@@deriving sexp,yojson,hash,compare]

type comment =
  [%import: Vernacexpr.comment]
  [@@deriving sexp,yojson,hash,compare]

type 'a search_restriction =
  [%import: 'a Vernacexpr.search_restriction]
  [@@deriving sexp,yojson,hash,compare]

type verbose_flag =
  [%import: Vernacexpr.verbose_flag]
  [@@deriving sexp,yojson,hash,compare]

type coercion_flag =
  [%import: Vernacexpr.coercion_flag]
  [@@deriving sexp,yojson,hash,compare]

type instance_flag =
  [%import: Vernacexpr.instance_flag]
  [@@deriving sexp,yojson,hash,compare]

type export_flag =
  [%import: Vernacexpr.export_flag]
  [@@deriving sexp,yojson,hash,compare]

type locality_flag =
  [%import: Vernacexpr.locality_flag]
  [@@deriving sexp,yojson,hash,compare]

type definition_expr =
  [%import: Vernacexpr.definition_expr]
  [@@deriving sexp,yojson,hash,compare]

type notation_declaration =
  [%import: Vernacexpr.notation_declaration]
  [@@deriving sexp,yojson,hash,compare]

type recursion_order_expr =
  [%import: Vernacexpr.recursion_order_expr]
  [@@deriving sexp,yojson,hash,compare]

type recursive_expr_gen =
  [%import: Vernacexpr.recursive_expr_gen]
  [@@deriving sexp,yojson,hash,compare]

type fixpoint_expr =
  [%import: Vernacexpr.fixpoint_expr]
  [@@deriving sexp,yojson,hash,compare]

type fixpoints_expr =
  [%import: Vernacexpr.fixpoints_expr]
  [@@deriving sexp,yojson,hash,compare]

type cofixpoints_expr =
  [%import: Vernacexpr.cofixpoints_expr]
  [@@deriving sexp,yojson,hash,compare]

type recursives_expr =
  [%import: Vernacexpr.recursives_expr]
  [@@deriving sexp,yojson,hash,compare]

type local_decl_expr =
  [%import: Vernacexpr.local_decl_expr]
  [@@deriving sexp,yojson,hash,compare]

type inductive_kind =
  [%import: Vernacexpr.inductive_kind]
  [@@deriving sexp,yojson,hash,compare]

type simple_binder =
  [%import: Vernacexpr.simple_binder]
  [@@deriving sexp,yojson,hash,compare]

type class_binder =
  [%import: Vernacexpr.class_binder]
  [@@deriving sexp,yojson,hash,compare]

type 'a with_coercion =
  [%import: 'a Vernacexpr.with_coercion]
  [@@deriving sexp,yojson,hash,compare]

type 'a with_coercion_instance =
  [%import: 'a Vernacexpr.with_coercion_instance]
  [@@deriving sexp,yojson,hash,compare]

type constructor_expr =
  [%import: Vernacexpr.constructor_expr]
  [@@deriving sexp,yojson,hash,compare]

type record_field_attr_unparsed =
  [%import: Vernacexpr.record_field_attr_unparsed]
  [@@deriving sexp,yojson,hash,compare]

type constructor_list_or_record_decl_expr =
  [%import: Vernacexpr.constructor_list_or_record_decl_expr]
  [@@deriving sexp,yojson,hash,compare]

type inductive_params_expr =
  [%import: Vernacexpr.inductive_params_expr]
  [@@deriving sexp,yojson,hash,compare]

type inductive_expr =
  [%import: Vernacexpr.inductive_expr]
  [@@deriving sexp,yojson,hash,compare]

type one_inductive_expr =
  [%import: Vernacexpr.one_inductive_expr]
  [@@deriving sexp,yojson,hash,compare]

type proof_expr =
  [%import: Vernacexpr.proof_expr]
  [@@deriving sexp,yojson,hash,compare]

type proof_end =
  [%import: Vernacexpr.proof_end]
  [@@deriving sexp,yojson,hash,compare]

type scheme_type =
  [%import: Vernacexpr.scheme_type]
  [@@deriving sexp,yojson,hash,compare]

type scheme =
  [%import: Vernacexpr.scheme]
  [@@deriving sexp,yojson,hash,compare]

type subproof_kind =
  [%import: Vernacexpr.subproof_kind]
  [@@deriving sexp,yojson,hash,compare]

type section_subset_expr =
  [%import: Vernacexpr.section_subset_expr]
  [@@deriving sexp,yojson,hash,compare]

type extend_name =
  [%import: Vernacexpr.extend_name]
  [@@deriving sexp,yojson,hash,compare]

type register_kind =
  [%import: Vernacexpr.register_kind]
  [@@deriving sexp,yojson,hash,compare]

type module_ast_inl =
  [%import: Vernacexpr.module_ast_inl]
  [@@deriving sexp,yojson,hash,compare]

type discharge =
  [%import: Vernacexpr.discharge]
  [@@deriving sexp,yojson,hash,compare]

type equality_scheme_type =
  [%import: Vernacexpr.equality_scheme_type]
  [@@deriving sexp,yojson,hash,compare]

type import_categories =
  [%import: Vernacexpr.import_categories]
  [@@deriving sexp,yojson,hash,compare]

type export_with_cats =
  [%import: Vernacexpr.export_with_cats]
  [@@deriving sexp,yojson,hash,compare]

type module_binder =
  [%import: Vernacexpr.module_binder]
  [@@deriving sexp,yojson,hash,compare]

type one_import_filter_name =
  [%import: Vernacexpr.one_import_filter_name]
  [@@deriving sexp,yojson,hash,compare]

type import_filter_expr =
  [%import: Vernacexpr.import_filter_expr]
  [@@deriving sexp,yojson,hash,compare]

type hint_info_expr =
  [%import: Vernacexpr.hint_info_expr]
  [@@deriving sexp,yojson,hash,compare]

type reference_or_constr =
  [%import: Vernacexpr.reference_or_constr]
  [@@deriving sexp,yojson,hash,compare]

type hints_expr =
  [%import: Vernacexpr.hints_expr]
  [@@deriving sexp,yojson,hash,compare]

type vernac_one_argument_status =
  [%import: Vernacexpr.vernac_one_argument_status]
  [@@deriving sexp,yojson,hash,compare]

type vernac_argument_status =
  [%import: Vernacexpr.vernac_argument_status]
  [@@deriving sexp,yojson,hash,compare]

type arguments_modifier =
  [%import: Vernacexpr.arguments_modifier]
  [@@deriving sexp,yojson,hash,compare]

type option_setting =
  [%import: Vernacexpr.option_setting]
  [@@deriving sexp,yojson,hash,compare]

type notation_enable_modifier =
  [%import: Vernacexpr.notation_enable_modifier]
  [@@deriving sexp, yojson,hash,compare]

type synterp_vernac_expr =
  [%import: Vernacexpr.synterp_vernac_expr]
  [@@deriving sexp,yojson,hash,compare]

type synpure_vernac_expr =
  [%import: Vernacexpr.synpure_vernac_expr]
  [@@deriving sexp,yojson,hash,compare]

type 'a vernac_expr_gen =
  [%import: 'a Vernacexpr.vernac_expr_gen]
  [@@deriving sexp,yojson,hash,compare]

type control_flag_r =
  [%import: Vernacexpr.control_flag_r]
  [@@deriving sexp,yojson,hash,compare]

type control_flag =
  [%import: Vernacexpr.control_flag]
  [@@deriving sexp,yojson,hash,compare]

type ('a, 'b) vernac_control_gen_r =
  [%import: ('a, 'b) Vernacexpr.vernac_control_gen_r]
  [@@deriving sexp,yojson,hash,compare]

type 'a vernac_control_gen =
  [%import: 'a Vernacexpr.vernac_control_gen]
  [@@deriving sexp,yojson,hash,compare]

type vernac_control =
  [%import: Vernacexpr.vernac_control]
  [@@deriving sexp,yojson,hash,compare]
