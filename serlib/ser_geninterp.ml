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

module Names      = Ser_names
module PolyFlags = Ser_polyFlags

module Val = struct

  type 'a typ =
    [%import: 'a Geninterp.Val.typ]

  (* let typ_of_sexp _ _ = Serlib_base.opaque_of_sexp "Geninterp.Val.typ" *)
  let sexp_of_typ _ x = Serlib_base.sexp_of_opaque ~typ:"Geninterp.Val.typ" x

  type t =
    [%import: Geninterp.Val.t]
    [@@deriving sexp_of]

  let t_of_sexp x = Serlib_base.opaque_of_sexp ~typ:"Geninterp.Val.t" x
  let of_yojson = Serlib_base.opaque_of_yojson ~typ:"Geninterp.Val.t"
  let to_yojson x = Serlib_base.opaque_to_yojson ~typ:"Geninterp.Val.t" x

  let hash = Hashtbl.hash
  let hash_fold_t st d = Ppx_hash_lib.Std.Hash.Builtin.hash_fold_int st (Hashtbl.hash d)
  let compare = Stdlib.compare

end

module TacStore = struct
  type t = Geninterp.TacStore.t
  let t_of_sexp = Serlib_base.opaque_of_sexp ~typ:"Geninterp.TacStore.t"
  let sexp_of_t = Serlib_base.sexp_of_opaque ~typ:"Geninterp.TacStore.t"
  let to_yojson = Serlib_base.opaque_to_yojson ~typ:"Geninterp.TacStore.t"
  let of_yojson = Serlib_base.opaque_of_yojson ~typ:"Geninterp.TacStore.t"
  let _hash = Hashtbl.hash
  let hash_fold_t st d = Ppx_hash_lib.Std.Hash.Builtin.hash_fold_int st (Hashtbl.hash d)
  let compare = Stdlib.compare
end

type interp_sign =
  [%import: Geninterp.interp_sign]
  [@@deriving sexp,yojson,hash,compare]
