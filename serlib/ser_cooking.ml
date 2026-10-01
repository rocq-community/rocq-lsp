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

module Names = Ser_names
module Univ = Ser_univ
module UVars = Ser_uvars
module Constr = Ser_constr

type abstr_info = {
  abstr_ctx : Constr.named_context;
  abstr_auctx : UVars.AbstractContext.t;
  abstr_ausubst : UVars.Instance.t;
} [@@deriving sexp,yojson,hash,compare]

type abstr_inst_info = {
  abstr_rev_inst : Names.Id.t list;
  abstr_uinst : UVars.Instance.t;
} [@@deriving sexp,yojson,hash,compare]

type 'a entry_map = 'a Names.Cmap.t * 'a Names.Mindmap.t [@@deriving sexp,yojson,hash,compare]
type expand_info = abstr_inst_info entry_map [@@deriving sexp,yojson,hash,compare]

module CIP = struct
type _t = {
  expand_info : expand_info;
  abstr_info : abstr_info;
} [@@deriving sexp,yojson,hash,compare]

type t =
  [%import: Cooking.cooking_info]
end

module B_ = SerType.Pierce(CIP)
type cooking_info = B_.t
 [@@deriving sexp,yojson,hash,compare]
