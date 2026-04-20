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
open Sexplib.Std

module Tok           = Ser_tok
module Notation_term = Ser_notation_term
module Constrexpr    = Ser_constrexpr
module Gramlib       = Ser_gramlib
module Globnames     = Ser_globnames
module Procq = Ser_procq

type production_position =
  [%import: Extend.production_position]
  [@@deriving sexp,yojson,hash,compare]

type production_level =
  [%import: Extend.production_level]
  [@@deriving sexp,yojson,hash,compare]

type binder_entry_kind =
  [%import: Extend.binder_entry_kind]
  [@@deriving sexp]

and constr_prod_entry_key =
  [%import: Extend.constr_prod_entry_key]
  [@@deriving sexp]

type ('custom,'lev) constr_entry_key_gen =
  [%import: ('custom,'lev) Extend.constr_entry_key_gen]
  [@@deriving sexp,yojson,hash,compare]

type constr_entry_key =
  [%import: Extend.constr_entry_key]
  [@@deriving sexp,yojson,hash,compare]

type 'custom simple_constr_prod_entry_key =
  [%import: 'custom Extend.simple_constr_prod_entry_key]
  [@@deriving sexp,yojson,hash,compare]

