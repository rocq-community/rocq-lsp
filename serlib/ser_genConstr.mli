(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Sexplib

type raw = GenConstr.raw
[@@deriving sexp,yojson,hash,compare]

type glb = GenConstr.glb
[@@deriving sexp,yojson,hash,compare]

type ('raw, 'glb) gen_ser =
  { raw_ser : 'raw -> Sexp.t
  ; raw_des : Sexp.t -> 'raw
  ; raw_hash : 'raw Ppx_hash_lib.Std.Hash.folder
  ; raw_compare : 'raw -> 'raw -> int

  ; glb_ser : 'glb -> Sexp.t
  ; glb_des : Sexp.t -> 'glb
  ; glb_hash : 'glb Ppx_hash_lib.Std.Hash.folder
  ; glb_compare : 'glb -> 'glb -> int
  }

module type GenSer = sig
  type raw [@@deriving sexp,hash,compare,yojson]
  type glb [@@deriving sexp,hash,compare,yojson]
end

module GS (M : GenSer) : sig val genser : (M.raw,M.glb) gen_ser end

val register : ('raw,'glb) GenConstr.tag -> ('raw,'glb) gen_ser -> unit
val register_mod : ('raw,'glb) GenConstr.tag -> (module GenSer with type raw = 'raw and type glb = 'glb) -> unit
