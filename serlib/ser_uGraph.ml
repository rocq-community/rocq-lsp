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

module Stdlib = Ser_stdlib
module Sorts = Ser_sorts
module Univ = Ser_univ
module Pp = Ser_pp

type t = [%import: UGraph.t]

let sexp_of_t = Serlib_base.sexp_of_opaque ~typ:"UGraph.t"
let t_of_sexp = Serlib_base.opaque_of_sexp ~typ:"UGraph.t"

type path_explanation = [%import: UGraph.path_explanation]

let sexp_of_path_explanation = Serlib_base.sexp_of_opaque ~typ:"UGraph.path_explanation"
let path_explanation_of_sexp = Serlib_base.opaque_of_sexp ~typ:"UGraph.path_explanation"

type explanation = [%import: UGraph.explanation]
  [@@deriving sexp]

type univ_inconsistency =
  [%import: UGraph.univ_inconsistency]
  [@@deriving sexp]
