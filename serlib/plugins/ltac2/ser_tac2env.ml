(************************************************************************)
(* SerAPI: Coq interaction protocol with bidirectional serialization    *)
(************************************************************************)
(* Copyright 2016-2019 MINES ParisTech -- License LGPL 2.1+             *)
(* Copyright 2019-2023 Inria           -- License LGPL 2.1+             *)
(* Written by: Emilio J. Gallego Arias and others                       *)
(************************************************************************)

open Serlib
open Ltac2_plugin

open Sexplib.Std
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

module Util = Ser_util
module Loc = Ser_loc
module CAst = Ser_cAst
module Names = Ser_names
module Tac2expr = Ser_tac2expr

module WLC2 = struct
  type raw = Tac2expr.raw_tacexpr
  [@@deriving sexp,yojson,hash,compare]
  type glb = Names.Id.Set.t * Tac2expr.glb_tacexpr
  [@@deriving sexp,yojson,hash,compare]
end

let ser_wit_ltac2_constr = let module M = Ser_genConstr.GS(WLC2) in M.genser

type var_quotation_kind =
  [%import: Ltac2_plugin.Tac2env.var_quotation_kind]
  [@@deriving sexp,yojson,hash,compare]

module WLQ2 = struct
  type raw = Names.lident option * Names.lident
  [@@deriving sexp,yojson,hash,compare]
  type glb = var_quotation_kind * Names.Id.t
  [@@deriving sexp,yojson,hash,compare]
end

let ser_wit_ltac2_var_quotation = let module M = Ser_genConstr.GS(WLQ2) in M.genser

let register () =
  Ser_genConstr.register Tac2env.wit_ltac2_constr ser_wit_ltac2_constr;
  Ser_genConstr.register Tac2env.wit_ltac2_var_quotation ser_wit_ltac2_var_quotation;
  ()

let () = register ()
