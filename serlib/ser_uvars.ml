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
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

let hash_fold_array = hash_fold_array_frozen

module Names = Ser_names

module Univ = Ser_univ
open Univ

module Sorts = Ser_sorts
open Sorts

module PConstraints = Ser_pConstraints

module Variance = struct

  type t =
    [%import: UVars.Variance.t]
  [@@deriving sexp,yojson,hash,compare]

end

module Instance = struct

type t =
  [%import: UVars.Instance.t]

type _t = Instance of (Quality.t array * Level.t array)
  [@@deriving sexp, yojson, hash, compare]

let _instance_put instance            = Instance (UVars.Instance.to_array instance)
let _instance_get (Instance instance) = UVars.Instance.of_array instance

let t_of_sexp sexp     = _instance_get (_t_of_sexp sexp)
let sexp_of_t instance = sexp_of__t (_instance_put instance)

let of_yojson json  =
  Ppx_deriving_yojson_runtime.(_t_of_yojson json >|= _instance_get)
let to_yojson level = _t_to_yojson (_instance_put level)

let hash i = hash__t (Instance (UVars.Instance.to_array i))
let hash_fold_t st i = hash_fold__t st (Instance (UVars.Instance.to_array i))
let compare i1 i2 = compare__t (Instance (UVars.Instance.to_array i1)) (Instance (UVars.Instance.to_array i2))

end

type bound_names = [%import: UVars.bound_names]
  [@@deriving sexp,yojson,hash,compare]

module UContext = struct

  module I = struct
    type t = UVars.UContext.t
    type _t = bound_names * (Instance.t * PConstraints.t)
    [@@deriving sexp,yojson,hash,compare]

    let to_t (un, cs) = UVars.UContext.make un cs
    let of_t uc = UVars.UContext.(names uc, (instance uc, constraints uc))
  end

  include SerType.Biject(I)

end

module AbstractContext = struct

  module ACPierceDef = struct

    type t = UVars.AbstractContext.t
    type _t = bound_names * UnivConstraints.t
    [@@deriving sexp,yojson,hash,compare]
  end

  include SerType.Pierce(ACPierceDef)

end

type 'a in_universe_context =
  [%import: 'a UVars.in_universe_context]
  [@@deriving sexp]

type 'a puniverses =
  [%import: 'a UVars.puniverses]
  [@@deriving sexp, yojson, hash, compare]
