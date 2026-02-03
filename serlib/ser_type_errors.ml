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

open Sexplib.Std

module Stdlib  = Ser_stdlib
module Loc     = Ser_loc
module Names   = Ser_names
module Constr  = Ser_constr
module Environ = Ser_environ
module Sorts   = Ser_sorts
module Univ    = Ser_univ
module UVars   = Ser_uvars
module Context = Ser_context
module CPrimitives = Ser_cPrimitives
module PConstraints = Ser_pConstraints

type 'constr pfix_guard_error =
  [%import: 'constr Type_errors.pfix_guard_error]
  [@@deriving sexp]

type 'constr pcofix_guard_error =
  [%import: 'constr Type_errors.pcofix_guard_error]
  [@@deriving sexp]

type 'constr pguard_error =
  [%import: 'constr Type_errors.pguard_error]
  [@@deriving sexp]

type guard_error =
  [%import: Type_errors.guard_error]
  [@@deriving sexp]

type ('constr, 'types) pcant_apply_bad_type =
  [%import: ('constr, 'types) Type_errors.pcant_apply_bad_type]
  [@@deriving sexp]

type ('constr, 'types, 'r) ptype_error =
  [%import: ('constr, 'types, 'r) Type_errors.ptype_error]
  [@@deriving sexp]

type type_error =
  [%import: Type_errors.type_error]
  [@@deriving sexp]

