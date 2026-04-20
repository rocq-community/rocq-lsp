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
open Ppx_compare_lib.Builtin
open Ppx_hash_lib.Std.Hash.Builtin

let hash_fold_array = hash_fold_array_frozen

module RTreePierce = struct

  type 'a t = 'a Rtree.t
  type 'a _t =
    | Param of int * int
    | Node of 'a * 'a _t array
    | Rec of int * 'a _t array
  [@@deriving sexp,yojson,hash,compare]
end

include SerType.Pierce1(RTreePierce)

module Automaton = SerType.Opaque1(struct type 'a t = 'a Rtree.Automaton.t let name = "Rtree.Automaton" end)
