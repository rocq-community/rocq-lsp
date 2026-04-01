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

module Names         = Ser_names
module Constrexpr    = Ser_constrexpr
module Tok           = Ser_tok
module Extend        = Ser_extend
module Gramlib       = Ser_gramlib
module PrimNotations = Ser_primNotations
module Notationextern = Ser_notationextern
module Procq = Ser_procq

(* type precedence =
 *   [%import: Notation_gram.precedence]
 *   [@@deriving sexp] *)

(* type parenRelation =
 *   [%import: Notation_gram.parenRelation]
 *   [@@deriving sexp] *)

(* type tolerability =
 *   [%import: Notation_gram.tolerability]
 *   [@@deriving sexp] *)

type grammar_constr_prod_item =
  [%import: Notation_gram.grammar_constr_prod_item]
  [@@deriving sexp]

type one_notation_grammar =
  [%import: Notation_gram.one_notation_grammar]
  [@@deriving sexp]

type notation_grammar =
  [%import: Notation_gram.notation_grammar]
  [@@deriving sexp]
