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

open Sexplib.Std
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

open Serlib

module Genintern = Ser_genintern
module Geninterp = Ser_geninterp
module Ltac_plugin = struct
  module Tacinterp = Serlib_ltac.Ser_tacinterp
end

type ssrtermkind =
  [%import: Ssrmatching_plugin.Ssrmatching.ssrtermkind]
  [@@deriving sexp,yojson,hash,compare]

type cpattern =
  [%import: Ssrmatching_plugin.Ssrmatching.cpattern]
  [@@deriving sexp,yojson,hash,compare]

type ('a, 'b) ssrpattern =
  [%import: ('a, 'b) Ssrmatching_plugin.Ssrmatching.ssrpattern]
  [@@deriving sexp,yojson,hash,compare]

type rpattern =
  [%import: Ssrmatching_plugin.Ssrmatching.rpattern]
  [@@deriving sexp,yojson,hash,compare]

type ssrdir =
  [%import: Ssrmatching_plugin.Ssrmatching.ssrdir]
  [@@deriving sexp]
