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

open Sexplib
open Ltac_plugin

type direction_flag = bool
val direction_flag_of_sexp : Sexp.t -> direction_flag
val sexp_of_direction_flag : direction_flag -> Sexp.t

type lazy_flag = Tacexpr.lazy_flag = General | Select | Once
val lazy_flag_of_sexp : Sexp.t -> lazy_flag
val sexp_of_lazy_flag : lazy_flag -> Sexp.t

type global_flag = Tacexpr.global_flag = TacGlobal | TacLocal
val global_flag_of_sexp : Sexp.t -> global_flag
val sexp_of_global_flag : global_flag -> Sexp.t

type evars_flag = bool
val evars_flag_of_sexp : Sexp.t -> evars_flag
val sexp_of_evars_flag : evars_flag -> Sexp.t

type rec_flag = bool
val rec_flag_of_sexp : Sexp.t -> rec_flag
val sexp_of_rec_flag : rec_flag -> Sexp.t

type advanced_flag = bool
val advanced_flag_of_sexp : Sexp.t -> advanced_flag
val sexp_of_advanced_flag : advanced_flag -> Sexp.t

type letin_flag = bool
val letin_flag_of_sexp : Sexp.t -> letin_flag
val sexp_of_letin_flag : letin_flag -> Sexp.t

type clear_flag = bool option
val clear_flag_of_sexp : Sexp.t -> clear_flag
val sexp_of_clear_flag : clear_flag -> Sexp.t

(* type debug = Tacexpr.debug = Debug | Info | Off *)
(* val debug_of_sexp : Sexp.t -> debug *)
(* val sexp_of_debug : debug -> Sexp.t *)

(* type goal_selector = Tacexpr.goal_selector *)
(* val goal_selector_of_sexp : Sexp.t -> goal_selector *)
(* val sexp_of_goal_selector : goal_selector -> Sexp.t *)

type ('c, 'd, 'id) inversion_strength = ('c, 'd, 'id) Tacexpr.inversion_strength

val inversion_strength_of_sexp :
  (Sexp.t -> 'c) ->
  (Sexp.t -> 'd) ->
  (Sexp.t -> 'id) ->
  Sexp.t -> ('c, 'd, 'id) inversion_strength

val sexp_of_inversion_strength :
  ('c -> Sexp.t) ->
  ('d -> Sexp.t) ->
  ('id -> Sexp.t) ->
  ('c, 'd, 'id) inversion_strength -> Sexp.t

type 'id message_token = 'id Tacexpr.message_token

val message_token_of_sexp :
  (Sexp.t -> 'id) -> Sexp.t -> 'id message_token

val sexp_of_message_token :
  ('id -> Sexp.t) -> 'id message_token -> Sexp.t

type ('dconstr, 'id) induction_clause = ('dconstr, 'id) Tacexpr.induction_clause

val induction_clause_of_sexp :
  (Sexp.t -> 'dconstr) ->
  (Sexp.t -> 'id) ->
  Sexp.t -> ('dconstr, 'id) induction_clause

val sexp_of_induction_clause :
  ('dconstr -> Sexp.t) ->
  ('id -> Sexp.t) ->
  ('dconstr, 'id) induction_clause -> Sexp.t


type ('constr, 'dconstr, 'id) induction_clause_list =
  ('constr, 'dconstr, 'id) Tacexpr.induction_clause_list

val induction_clause_list_of_sexp :
  (Sexp.t -> 'constr) ->
  (Sexp.t -> 'dconstr) ->
  (Sexp.t -> 'id) ->
  Sexp.t -> ('constr, 'dconstr, 'id) induction_clause_list

val sexp_of_induction_clause_list :
  ('constr -> Sexp.t) ->
  ('dconstr -> Sexp.t) ->
  ('id -> Sexp.t) ->
  ('constr, 'dconstr, 'id) induction_clause_list -> Sexp.t

type 'a with_bindings_arg = 'a Tacexpr.with_bindings_arg

val with_bindings_arg_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a with_bindings_arg
val sexp_of_with_bindings_arg : ('a -> Sexp.t) -> 'a with_bindings_arg -> Sexp.t

(* type multi = Tacexpr.multi *)
(* val multi_of_sexp : Sexp.t -> multi *)
(* val sexp_of_multi : multi -> Sexp.t *)

type 'a match_pattern = 'a Tacexpr.match_pattern

val match_pattern_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a match_pattern
val sexp_of_match_pattern : ('a -> Sexp.t) -> 'a match_pattern -> Sexp.t

type 'a match_context_hyps = 'a Tacexpr.match_context_hyps

val match_context_hyps_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a match_context_hyps
val sexp_of_match_context_hyps : ('a -> Sexp.t) -> 'a match_context_hyps -> Sexp.t

type ('a, 't) match_rule = ('a, 't) Tacexpr.match_rule

val match_rule_of_sexp :
  (Sexp.t -> 'a) ->
  (Sexp.t -> 't) -> Sexp.t -> ('a, 't) match_rule
val sexp_of_match_rule :
  ('a -> Sexp.t) ->
  ('t -> Sexp.t) -> ('a, 't) match_rule -> Sexp.t

type ml_tactic_name = Tacexpr.ml_tactic_name

val ml_tactic_name_of_sexp : Sexp.t -> ml_tactic_name
val sexp_of_ml_tactic_name : ml_tactic_name -> Sexp.t

type 'd gen_atomic_tactic_expr = 'd Tacexpr.gen_atomic_tactic_expr

val sexp_of_gen_atomic_tactic_expr :
  ('a -> Sexplib.Sexp.t) ->
  ('c -> Sexplib.Sexp.t) ->
  ('d -> Sexplib.Sexp.t) ->
  ('rp -> Sexplib.Sexp.t) ->
  ('e -> Sexplib.Sexp.t) ->
  ('f -> Sexplib.Sexp.t) ->
  ('g -> Sexplib.Sexp.t) ->
  ('occvar -> Sexplib.Sexp.t) ->
  ('h -> Sexplib.Sexp.t) ->
  ('i -> Sexplib.Sexp.t) ->
  < occvar : 'occvar; red_pattern : 'rp; constant : 'e; dterm : 'c; level : 'i; name : 'g; pattern : 'd;
    reference : 'f; tacexpr : 'h; term : 'a; >
  Tacexpr.gen_atomic_tactic_expr -> Sexplib.Sexp.t
val sexp_of_gen_tactic_expr :
  ('a -> Sexplib.Sexp.t) ->
  ('c -> Sexplib.Sexp.t) ->
  ('d -> Sexplib.Sexp.t) ->
  ('rp -> Sexplib.Sexp.t) ->
  ('e -> Sexplib.Sexp.t) ->
  ('f -> Sexplib.Sexp.t) ->
  ('g -> Sexplib.Sexp.t) ->
  ('occvar -> Sexplib.Sexp.t) ->
  ('h -> Sexplib.Sexp.t) ->
  ('i -> Sexplib.Sexp.t) ->
  < occvar : 'occvar; red_pattern : 'rp; constant : 'e; dterm : 'c; level : 'i; name : 'g; pattern : 'd;
    reference : 'f; tacexpr : 'h; term : 'a; >
  Tacexpr.gen_tactic_expr -> Sexplib.Sexp.t
val sexp_of_gen_tactic_arg :
  ('a -> Sexplib.Sexp.t) ->
  ('c -> Sexplib.Sexp.t) ->
  ('d -> Sexplib.Sexp.t) ->
  ('rp -> Sexplib.Sexp.t) ->
  ('e -> Sexplib.Sexp.t) ->
  ('f -> Sexplib.Sexp.t) ->
  ('g -> Sexplib.Sexp.t) ->
  ('occvar -> Sexplib.Sexp.t) ->
  ('h -> Sexplib.Sexp.t) ->
  ('i -> Sexplib.Sexp.t) ->
  < occvar : 'occvar; red_pattern : 'rp; constant : 'e; dterm : 'c; level : 'i; name : 'g; pattern : 'd;
    reference : 'f; tacexpr : 'h; term : 'a; >
  Tacexpr.gen_tactic_arg -> Sexplib.Sexp.t
val sexp_of_gen_fun_ast :
  ('a -> Sexplib.Sexp.t) ->
  ('c -> Sexplib.Sexp.t) ->
  ('d -> Sexplib.Sexp.t) ->
  ('rp -> Sexplib.Sexp.t) ->
  ('e -> Sexplib.Sexp.t) ->
  ('f -> Sexplib.Sexp.t) ->
  ('g -> Sexplib.Sexp.t) ->
  ('occvar -> Sexplib.Sexp.t) ->
  ('h -> Sexplib.Sexp.t) ->
  ('i -> Sexplib.Sexp.t) ->
  < occvar : 'occvar; red_pattern : 'rp; constant : 'e; dterm : 'c; level : 'i; name : 'g; pattern : 'd;
    reference : 'f; tacexpr : 'h; term : 'a; >
  Tacexpr.gen_tactic_fun_ast -> Sexplib.Sexp.t

val gen_atomic_tactic_expr_of_sexp :
  Sexplib.Sexp.t ->
  (Sexplib.Sexp.t -> 'a) ->
  (Sexplib.Sexp.t -> 'c) ->
  (Sexplib.Sexp.t -> 'd) ->
  (Sexplib.Sexp.t -> 'rp) ->
  (Sexplib.Sexp.t -> 'e) ->
  (Sexplib.Sexp.t -> 'f) ->
  (Sexplib.Sexp.t -> 'g) ->
  (Sexplib.Sexp.t -> 'occvar) ->
  (Sexplib.Sexp.t -> 'h) ->
  (Sexplib.Sexp.t -> 'i) ->
  < occvar : 'occvar; red_pattern : 'rp; constant : 'e; dterm : 'c; level : 'i; name : 'g; pattern : 'd;
    reference : 'f; tacexpr : 'h; term : 'a; >
  Tacexpr.gen_atomic_tactic_expr

val gen_tactic_expr_of_sexp :
  Sexplib.Sexp.t ->
  (Sexplib.Sexp.t -> 'a) ->
  (Sexplib.Sexp.t -> 'c) ->
  (Sexplib.Sexp.t -> 'd) ->
  (Sexplib.Sexp.t -> 'rp) ->
  (Sexplib.Sexp.t -> 'e) ->
  (Sexplib.Sexp.t -> 'f) ->
  (Sexplib.Sexp.t -> 'g) ->
  (Sexplib.Sexp.t -> 'occvar) ->
  (Sexplib.Sexp.t -> 'h) ->
  (Sexplib.Sexp.t -> 'i) ->
  < occvar : 'occvar; red_pattern : 'rp; constant : 'e; dterm : 'c; level : 'i; name : 'g; pattern : 'd;
    reference : 'f; tacexpr : 'h; term : 'a; >
  Tacexpr.gen_tactic_expr

val gen_tactic_arg_of_sexp :
  Sexplib.Sexp.t ->
  (Sexplib.Sexp.t -> 'a) ->
  (Sexplib.Sexp.t -> 'c) ->
  (Sexplib.Sexp.t -> 'd) ->
  (Sexplib.Sexp.t -> 'rp) ->
  (Sexplib.Sexp.t -> 'e) ->
  (Sexplib.Sexp.t -> 'f) ->
  (Sexplib.Sexp.t -> 'g) ->
  (Sexplib.Sexp.t -> 'occvar) ->
  (Sexplib.Sexp.t -> 'h) ->
  (Sexplib.Sexp.t -> 'i) ->
  < occvar : 'occvar; red_pattern : 'rp; constant : 'e; dterm : 'c; level : 'i; name : 'g; pattern : 'd;
    reference : 'f; tacexpr : 'h; term : 'a; >
  Tacexpr.gen_tactic_arg

val gen_fun_ast_of_sexp :
  Sexplib.Sexp.t ->
  (Sexplib.Sexp.t -> 'a) ->
  (Sexplib.Sexp.t -> 'c) ->
  (Sexplib.Sexp.t -> 'd) ->
  (Sexplib.Sexp.t -> 'rp) ->
  (Sexplib.Sexp.t -> 'e) ->
  (Sexplib.Sexp.t -> 'f) ->
  (Sexplib.Sexp.t -> 'g) ->
  (Sexplib.Sexp.t -> 'occvar) ->
  (Sexplib.Sexp.t -> 'h) ->
  (Sexplib.Sexp.t -> 'i) ->
  < occvar : 'occvar; red_pattern : 'rp; constant : 'e; dterm : 'c; level : 'i; name : 'g; pattern : 'd;
    reference : 'f; tacexpr : 'h; term : 'a; >
  Tacexpr.gen_tactic_fun_ast

type glob_tactic_expr = Tacexpr.glob_tactic_expr
  [@@deriving sexp,yojson,hash,compare]

type glob_atomic_tactic_expr = Tacexpr.glob_atomic_tactic_expr
  [@@deriving sexp,yojson,hash,compare]

type raw_tactic_expr = Tacexpr.raw_tactic_expr
  [@@deriving sexp,yojson,hash,compare]

type raw_atomic_tactic_expr = Tacexpr.raw_atomic_tactic_expr
  [@@deriving sexp,yojson,hash,compare]

type atomic_tactic_expr = Tacexpr.atomic_tactic_expr
val atomic_tactic_expr_of_sexp : Sexp.t -> atomic_tactic_expr
val sexp_of_atomic_tactic_expr : atomic_tactic_expr -> Sexp.t

type tacdef_body = Tacexpr.tacdef_body
  [@@deriving sexp,hash,compare]

type intro_pattern = Tacexpr.intro_pattern
  [@@deriving sexp,hash,compare]

type raw_strategy = Tacexpr.raw_strategy
  [@@deriving sexp,hash,compare]

type glob_strategy = Tacexpr.glob_strategy
  [@@deriving sexp,hash,compare]
