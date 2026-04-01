(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(************************************************************************)
(* SerAPI: Coq interaction protocol with bidirectional serialization    *)
(************************************************************************)
(* Copyright 2016-2019 MINES ParisTech -- License LGPL 2.1+             *)
(* Copyright 2019-2023 Inria           -- License LGPL 2.1+             *)
(* Written by: Emilio J. Gallego Arias and others                       *)
(************************************************************************)

open Ltac_plugin

type unary_strategy = RewriteStratAst.unary_strategy
  [@@deriving sexp, hash, compare]

type binary_strategy = RewriteStratAst.binary_strategy
  [@@deriving sexp, hash, compare]

type ('a,'b,'c,'d,'e) strategy_ast = ('a,'b,'c,'d,'e) RewriteStratAst.strategy_ast
  [@@deriving sexp, hash, compare]

type strategy = Rewrite.strategy
  [@@deriving sexp, hash, compare]
