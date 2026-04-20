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
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

module Stdlib     = Ser_stdlib
module Names      = Ser_names
module Environ    = Ser_environ
module Glob_term  = Ser_glob_term
module Constrexpr = Ser_constrexpr
module Pattern    = Ser_pattern
module Notation_term = Ser_notation_term
module UnivNames = Ser_univNames

module Store = struct

  module StoreOpaque = struct type t = Genintern.Store.t let name = "Genintern.Store.t" end

  include SerType.Opaque(StoreOpaque)

end

type ntnvar_status = Genintern.ntnvar_status = {
  mutable ntnvar_used : bool list [@hash.ignore];
  mutable ntnvar_used_as_binder : bool [@hash.ignore];
  mutable ntnvar_scopes : Notation_term.subscopes option [@hash.ignore];
  mutable ntnvar_binding_ids : Notation_term.notation_var_binders option [@hash.ignore];
  ntnvar_typ : Notation_term.notation_var_internalization_type;
}
[@@deriving sexp,yojson,hash,compare]

type intern_variable_status =
  [%import: Genintern.intern_variable_status]
  [@@deriving sexp,yojson,hash,compare]

type glob_sign =
  [%import: Genintern.glob_sign]
  [@@deriving sexp]

type glob_constr_and_expr =
  [%import: Genintern.glob_constr_and_expr]
  [@@deriving sexp,yojson,hash,compare]

type glob_constr_pattern_and_expr =
  [%import: Genintern.glob_constr_pattern_and_expr]
  [@@deriving sexp,yojson,hash,compare]
