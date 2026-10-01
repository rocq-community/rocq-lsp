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

module Loc        = Ser_loc
module Names      = Ser_names
module Tok        = Ser_tok

open Sexplib.Std
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

module Constrexpr = Ser_constrexpr

type scope_name =
  [%import: Notation_term.scope_name]
  [@@deriving sexp,yojson,hash,compare]

type tmp_scope_name =
  [%import: Notation_term.tmp_scope_name]
  [@@deriving sexp,yojson,hash,compare]

type subscopes =
  [%import: Notation_term.subscopes]
  [@@deriving sexp,yojson,hash,compare]

(* type notation_spec = *)
(*   [%import: Notation_term.notation_spec] *)
(*   [@@deriving sexp] *)

(* type syntax_modifier = *)
(*   [%import: Notation_term.syntax_modifier] *)
(*   [@@deriving sexp] *)

type notation_binder_kind =
  [%import: Notation_term.notation_binder_kind]
  [@@deriving sexp,yojson,hash,compare]

type notation_var_internalization_type =
  [%import: Notation_term.notation_var_internalization_type]
  [@@deriving sexp,yojson,hash,compare]

type notation_var_binders =
  [%import: Notation_term.notation_var_binders]
  [@@deriving sexp,yojson,hash,compare]
