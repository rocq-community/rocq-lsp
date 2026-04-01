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

open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin
open Sexplib.Conv

type unary_strategy =
  [%import: Ltac_plugin.RewriteStratAst.unary_strategy]
  [@@deriving sexp,hash,compare]

type binary_strategy =
  [%import: Ltac_plugin.RewriteStratAst.binary_strategy]
  [@@deriving sexp,hash,compare]

type nary_strategy =
  [%import: Ltac_plugin.RewriteStratAst.nary_strategy]
  [@@deriving sexp,hash,compare]

type ('a,'b,'c,'d,'e) strategy_ast =
  [%import: ('a,'b,'c,'d,'e) Ltac_plugin.RewriteStratAst.strategy_ast]
  [@@deriving sexp,hash,compare]

type strategy = Rewrite.strategy

let strategy_of_sexp = Serlib.Serlib_base.opaque_of_sexp ~typ:"rewrite/strategy"
let sexp_of_strategy = Serlib.Serlib_base.sexp_of_opaque ~typ:"rewrite/strategy"
let hash_strategy = Hashtbl.hash
let hash_fold_strategy st d = Ppx_hash_lib.Std.Hash.Builtin.hash_fold_int st (Hashtbl.hash d)
let compare_strategy = Stdlib.compare
