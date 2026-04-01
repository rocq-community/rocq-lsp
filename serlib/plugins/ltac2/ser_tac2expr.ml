(************************************************************************)
(* SerAPI: Coq interaction protocol with bidirectional serialization    *)
(************************************************************************)
(* Copyright 2016-2019 MINES ParisTech -- License LGPL 2.1+             *)
(* Copyright 2019-2023 Inria           -- License LGPL 2.1+             *)
(* Written by: Emilio J. Gallego Arias and others                       *)
(************************************************************************)

module KN = Names.KerName
module NID = Names.Name

(* Color info analyzer *)
module CI = Serlib.Analysis_semtokens.Color_info
module CIA = Serlib.Analysis_semtokens

let drange = Loc.initial ToplevelInput

let m range msg = CI.make range (Format.asprintf "ekind: %s" msg)

let rec ltac2_expr_ci (e : Ltac2_plugin.Tac2expr.raw_tacexpr) : CI.t list =
  let { CAst.v; loc = range } = e in
  let range = Option.default drange range in
  match v with
  | CTacAtm _ -> [m range "atm"]
  | CTacRef _ -> [m range "ref"]
  | CTacCst _ -> [m range "cst"]
  | CTacFun (_args, body) -> [m range "fun"] @ ltac2_expr_ci body
  | CTacApp (fn, args) ->
    [m range "app"] @ ltac2_expr_ci fn @ List.concat_map ltac2_expr_ci args
  | CTacSyn _ -> [m range "nota"]
  | CTacLet (_, cases, expr) -> [m range "let"]
                                @ List.concat_map (fun (_, er) -> ltac2_expr_ci er) cases
                                @ ltac2_expr_ci expr
  | CTacCnv (_, _) -> [m range "cnv"]
  | CTacSeq (_, _) -> [m range "seq"]
  | CTacIft (_, _, _) -> [m range "ift"]
  | CTacCse (_, _) -> [m range "cse "]
  | CTacRec (_, _) -> [m range "rec"]
  | CTacPrj (_, _) -> [m range "prj"]
  | CTacSet (_, _, _) -> [m range "set"]
  | CTacExt (tag, _tag_val) ->
    let tag_name = Ltac2_plugin.Tac2dyn.Arg.repr tag in
    [m range ("ext: " ^ tag_name)]
  | CTacGlb (_, _, _, _) -> [m range "glb"]

let () =
  let raw = ltac2_expr_ci in
  let glb _ = [] in
  let top _ = [] in
  CIA.register Ltac2_plugin.G_ltac2.wit_ltac2_expr { raw; glb; top }

(* assumptions: parsing was OK *)
(* let orig_range = range_of_the_whole_sentence in *)
(* let sub_ranges = get_color_info ast in *)
(* let tac_ranges = orig_range - sub_ranges in *)
(* (tac_ranges, "english-word") @ sub_ranges *)

open Serlib

module Loc = Ser_loc
module CAst = Ser_cAst
module Names = Ser_names
module Libnames = Ser_libnames
module Attributes = Ser_attributes

open Sexplib.Std
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

let hash_fold_array = hash_fold_array_frozen

type mutable_flag =
  [%import: Ltac2_plugin.Tac2expr.mutable_flag]
  [@@deriving sexp,yojson,hash,compare]

type uid =
  [%import: Ltac2_plugin.Tac2expr.uid]
  [@@deriving sexp,yojson,hash,compare]

type lid =
  [%import: Ltac2_plugin.Tac2expr.lid]
  [@@deriving sexp,yojson,hash,compare]

type rec_flag =
  [%import: Ltac2_plugin.Tac2expr.rec_flag]
  [@@deriving sexp,yojson,hash,compare]

type redef_flag =
  [%import: Ltac2_plugin.Tac2expr.redef_flag]
  [@@deriving sexp,yojson,hash,compare]

type 'a or_relid =
  [%import: 'a Ltac2_plugin.Tac2expr.or_relid]
  [@@deriving sexp,yojson,hash,compare]

type 'a or_tuple =
  [%import: 'a Ltac2_plugin.Tac2expr.or_tuple]
  [@@deriving sexp,yojson,hash,compare]

type type_constant =
  [%import: Ltac2_plugin.Tac2expr.type_constant]
  [@@deriving sexp,yojson,hash,compare]

type raw_typexpr_r =
  [%import: Ltac2_plugin.Tac2expr.raw_typexpr_r]
  [@@deriving sexp,yojson,hash,compare]
and raw_typexpr =
  [%import: Ltac2_plugin.Tac2expr.raw_typexpr]
  [@@deriving sexp,yojson,hash,compare]

type raw_typedef =
  [%import: Ltac2_plugin.Tac2expr.raw_typedef]
  [@@deriving sexp,yojson,hash,compare]

type raw_quant_typedef =
  [%import: Ltac2_plugin.Tac2expr.raw_quant_typedef]
  [@@deriving sexp,yojson,hash,compare]

type 'a glb_typexpr =
  [%import: 'a Ltac2_plugin.Tac2expr.glb_typexpr]
  [@@deriving sexp,yojson,hash,compare]

type atom =
  [%import: Ltac2_plugin.Tac2expr.atom]
  [@@deriving sexp,yojson,hash,compare]

type ltac_constant =
  [%import: Ltac2_plugin.Tac2expr.ltac_constant]
  [@@deriving sexp,yojson,hash,compare]

type ltac_abbrev =
  [%import: Ltac2_plugin.Tac2expr.ltac_abbrev]
  [@@deriving sexp,yojson,hash,compare]

type ltac_constructor =
  [%import: Ltac2_plugin.Tac2expr.ltac_constructor]
  [@@deriving sexp,yojson,hash,compare]

type ltac_projection =
  [%import: Ltac2_plugin.Tac2expr.ltac_projection]
  [@@deriving sexp,yojson,hash,compare]

type raw_patexpr =
  [%import: Ltac2_plugin.Tac2expr.raw_patexpr]
  [@@deriving sexp,yojson,hash,compare]
and raw_patexpr_r =
  [%import: Ltac2_plugin.Tac2expr.raw_patexpr_r]
  [@@deriving sexp,yojson,hash,compare]

type tacref =
  [%import: Ltac2_plugin.Tac2expr.tacref]
  [@@deriving sexp,yojson,hash,compare]

type ml_tactic_name =
  [%import: Ltac2_plugin.Tac2expr.ml_tactic_name]
  [@@deriving sexp,yojson,hash,compare]

type sexpr =
  [%import: Ltac2_plugin.Tac2expr.sexpr]
  [@@deriving sexp,yojson,hash,compare]

type ctor_indx =
  [%import: Ltac2_plugin.Tac2expr.ctor_indx]
  [@@deriving sexp,yojson,hash,compare]

type ctor_data_for_patterns =
  [%import: Ltac2_plugin.Tac2expr.ctor_data_for_patterns]
  [@@deriving sexp,yojson,hash,compare]

type glb_pat =
  [%import: Ltac2_plugin.Tac2expr.glb_pat]
  [@@deriving sexp,yojson,hash,compare]

type case_info =
  [%import: Ltac2_plugin.Tac2expr.case_info]
  [@@deriving sexp,yojson,hash,compare]

type 'a open_match =
  [%import: 'a Ltac2_plugin.Tac2expr.open_match]
  [@@deriving sexp,yojson,hash,compare]

module ObjS = struct type t = Obj.t let name = "Obj.t" end
module Obj = SerType.Opaque(ObjS)

module GT2ESpec = struct
  type t = Ltac2_plugin.Tac2expr.glb_tacexpr
  open Ltac2_plugin.Tac2expr
  type _t =
    | GTacAtm of atom
    | GTacVar of Names.Id.t
    | GTacRef of ltac_constant
    | GTacFun of Names.Name.t list * _t
    | GTacApp of _t * _t list
    | GTacLet of rec_flag * (Names.Name.t * _t) list * _t
    | GTacCst of case_info * int * _t list
    | GTacCse of _t * case_info * _t array * (Names.Name.t array * _t) array
    | GTacPrj of type_constant * _t * int
    | GTacSet of type_constant * _t * int * _t
    | GTacOpn of ltac_constructor * _t list
    | GTacWth of _t open_match
    | GTacFullMatch of _t * (glb_pat * _t) list
    | GTacExt of int * Obj.t
    | GTacPrm of ml_tactic_name
  [@@deriving sexp,yojson,hash,compare]

end

module GT2E = Serlib.SerType.Pierce(GT2ESpec)
type glb_tacexpr = GT2E.t
  [@@deriving sexp,yojson,hash,compare]

(* TODO implement some fancy stuff? *)
module TacSyn = SerType.Opaque(struct type t let name = "tac2syn" end)

module T2ESpec = struct
  type t = Ltac2_plugin.Tac2expr.raw_tacexpr_r
  open Ltac2_plugin.Tac2expr
  type _t =
    | CTacAtm of atom
    | CTacRef of tacref or_relid
    | CTacCst of ltac_constructor or_tuple or_relid
    | CTacFun of raw_patexpr list * raw_tacexpr
    | CTacApp of raw_tacexpr * raw_tacexpr list
    | CTacSyn of TacSyn.t
    | CTacLet of rec_flag * (raw_patexpr * raw_tacexpr) list * raw_tacexpr
    | CTacCnv of raw_tacexpr * raw_typexpr
    | CTacSeq of raw_tacexpr * raw_tacexpr
    | CTacIft of raw_tacexpr * raw_tacexpr * raw_tacexpr
    | CTacCse of raw_tacexpr * raw_taccase list
    | CTacRec of raw_tacexpr option * raw_recexpr
    | CTacPrj of raw_tacexpr * ltac_projection or_relid
    | CTacSet of raw_tacexpr * ltac_projection or_relid * raw_tacexpr
    | CTacExt of int * Obj.t
    | CTacGlb of int * (Names.lname * raw_tacexpr * int glb_typexpr option) list * glb_tacexpr * int glb_typexpr

  and raw_tacexpr = _t CAst.t
  and raw_taccase =
  [%import: Ltac2_plugin.Tac2expr.raw_taccase]
  and raw_recexpr =
  [%import: Ltac2_plugin.Tac2expr.raw_recexpr]
  [@@deriving sexp,yojson,hash,compare]

end

module T2E = Serlib.SerType.Pierce(T2ESpec)
type raw_tacexpr_r = T2E.t
  [@@deriving sexp,yojson,hash,compare]

type raw_tacexpr =
  [%import: Ltac2_plugin.Tac2expr.raw_tacexpr]
  [@@deriving sexp,yojson,hash,compare]

type strexpr =
  [%import: Ltac2_plugin.Tac2expr.strexpr]
  [@@deriving sexp,yojson,hash,compare]
