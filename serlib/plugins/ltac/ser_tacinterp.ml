(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Serlib

module Names = Ser_names
module Geninterp = Ser_geninterp
module PolyFlags = Ser_polyFlags
module Constrexpr   = Ser_constrexpr
module Tactypes     = Ser_tactypes
module Genintern    = Ser_genintern


module TacStore = struct
  type t = Ltac_plugin.Tacinterp.TacStore.t
  let t_of_sexp = Serlib_base.opaque_of_sexp ~typ:"Geninterp.TacStore.t"
  let sexp_of_t = Serlib_base.sexp_of_opaque ~typ:"Geninterp.TacStore.t"
  let to_yojson = Serlib_base.opaque_to_yojson ~typ:"Geninterp.TacStore.t"
  let of_yojson = Serlib_base.opaque_of_yojson ~typ:"Geninterp.TacStore.t"
  let _hash = Hashtbl.hash
  let hash_fold_t st d = Ppx_hash_lib.Std.Hash.Builtin.hash_fold_int st (Hashtbl.hash d)
  let compare = Stdlib.compare
end

type interp_sign = [%import: Ltac_plugin.Tacinterp.interp_sign]
[@@deriving sexp,yojson,hash,compare]

let wit_glob_constr_with_bindings = Ltac_plugin.G_rewrite.wit_glob_constr_with_bindings

module GT1 = struct
  type raw = Constrexpr.constr_expr Tactypes.with_bindings
  [@@deriving sexp,hash,compare]
  type glb = Genintern.glob_constr_and_expr Tactypes.with_bindings
  [@@deriving sexp,hash,compare]
  type top = interp_sign * Genintern.glob_constr_and_expr Tactypes.with_bindings
  [@@deriving sexp,hash,compare]
end

let ser_wit_glob_constr_with_bindings = let module M = Ser_genarg.GS(GT1) in M.genser

let () =
  Ser_genarg.register_genser wit_glob_constr_with_bindings ser_wit_glob_constr_with_bindings;
