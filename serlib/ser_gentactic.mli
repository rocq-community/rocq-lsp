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

open Sexplib

type raw_generic_tactic = Gentactic.raw_generic_tactic
[@@deriving sexp,hash,compare,yojson]

type ('raw, 'glb) gen_ser =
  { raw_ser : 'raw -> Sexp.t
  ; raw_des : Sexp.t -> 'raw
  ; raw_hash : 'raw Ppx_hash_lib.Std.Hash.folder
  ; raw_compare : 'raw -> 'raw -> int
  }

module type GenSer = sig
  type raw [@@deriving sexp,hash,compare,yojson]
end

module GS (M : GenSer) : sig val genser : (M.raw,'glb) gen_ser end

val register : ('raw,'glb) Gentactic.tag -> ('raw,'glb) gen_ser -> unit
val register_mod : ('raw,'glb) Gentactic.tag -> (module GenSer with type raw = 'raw) -> unit
