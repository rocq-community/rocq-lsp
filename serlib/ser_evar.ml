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

(* Private *)
module Self = struct
type t = [%import: Evar.t]

type _t                    = Ser_Evar of int [@@deriving sexp,yojson,hash,compare]
let _t_put  evar           = Ser_Evar (Evar.repr evar)
let _t_get (Ser_Evar evar) = Evar.unsafe_of_int evar

let t_of_sexp sexp = _t_get (_t_of_sexp sexp)
let sexp_of_t evar = sexp_of__t (_t_put evar)

let of_yojson json = Ppx_deriving_yojson_runtime.(_t_of_yojson json >|= _t_get)
let to_yojson level = _t_to_yojson (_t_put level)

let hash x = hash__t (_t_put x)
let hash_fold_t st id = hash_fold__t st (_t_put id)

let compare x y = compare__t (_t_put x) (_t_put y)

end

include Self

module Set = Ser_cSet.Make(Evar.Set)(Self)
module Map = Ser_cMap.Make(Evar.Map)(Self)
