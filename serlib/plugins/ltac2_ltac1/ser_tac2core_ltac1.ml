(************************************************************************)
(* SerAPI: Coq interaction protocol with bidirectional serialization    *)
(************************************************************************)
(* Copyright 2016-2019 MINES ParisTech -- License LGPL 2.1+             *)
(* Copyright 2019-2023 Inria           -- License LGPL 2.1+             *)
(* Written by: Emilio J. Gallego Arias and others                       *)
(************************************************************************)

open Serlib
open Ltac2_ltac1_plugin

open Sexplib.Std
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

module Util = Ser_util
module Loc = Ser_loc
module CAst = Ser_cAst
module Names = Ser_names
module Tac2expr = Serlib_ltac2.Ser_tac2expr
module Geninterp = Ser_geninterp

module WL2in1 = struct
  type raw = Tac2expr.uid CAst.t list * Tac2expr.raw_tacexpr
  [@@deriving sexp,hash,compare]
  type glb = Tac2expr.uid list * Tac2expr.glb_tacexpr
  [@@deriving sexp,hash,compare]
  type top = Geninterp.Val.t
  [@@deriving sexp,hash,compare]
end

let ser_wit_ltac2in1 = let module M = Ser_genarg.GS(WL2in1) in M.genser

module WL2in1V = struct
  type raw = Tac2expr.uid CAst.t list * Tac2expr.raw_tacexpr
  [@@deriving sexp,hash,compare]
  type glb = Tac2expr.glb_tacexpr
  [@@deriving sexp,hash,compare]
  type top = Geninterp.Val.t
  [@@deriving sexp,hash,compare]
end

let ser_wit_ltac2in1_val = let module M = Ser_genarg.GS(WL2in1V) in M.genser

module WLV2 = struct
  type raw = Util.Empty.t
  [@@deriving sexp,hash,compare]
  type glb = unit
  [@@deriving sexp,hash,compare]
  type top = Geninterp.Val.t
  [@@deriving sexp,hash,compare]
end

let ser_wit_ltac2_val = let module M = Ser_genarg.GS(WLV2) in M.genser

let register () =
  Ser_genarg.register_genser Tac2core_ltac1.wit_ltac2in1 ser_wit_ltac2in1;
  Ser_genarg.register_genser Tac2core_ltac1.wit_ltac2in1_val ser_wit_ltac2in1_val;

  (* XXX not sure this one really needs registration, it should only
     appear temporarily at runtime in ltac execution *)
  Ser_genarg.register_genser Tac2core_ltac1.wit_ltac2_val ser_wit_ltac2_val;
  ()

let () = register ()
