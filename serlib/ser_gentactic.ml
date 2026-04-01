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
open Sexplib
open Ppx_hash_lib.Std

type ('raw, 'glb) gen_ser =
  { raw_ser : 'raw -> Sexp.t
  ; raw_des : Sexp.t -> 'raw
  ; raw_hash : 'raw Ppx_hash_lib.Std.Hash.folder
  ; raw_compare : 'raw -> 'raw -> int
  }

module type GenSer = sig
  type raw [@@deriving sexp,hash,compare,yojson]
end

module GS (M:GenSer) = struct
  let genser =
    { raw_ser = M.sexp_of_raw
    ; raw_des = M.raw_of_sexp
    ; raw_hash = M.hash_fold_raw
    ; raw_compare = M.compare_raw
    }
end

module Obj = struct
  type ('raw,'glb) t = ('raw,'glb) gen_ser
end

module Map = Gentactic.Map(Obj)

let registered = ref Map.empty

let register tag d = registered := Map.add tag d !registered

let register_mod (type raw) tag (d:(module GenSer with type raw = raw)) =
  let module D = (val d) in
  let module M = GS(D) in
  register tag M.genser

let sexp_of_gen tag =
  let typ = "raw_gentactic: " ^ Sexp.to_string (Atom (Gentactic.repr tag)) in
  Serlib_base.sexp_of_opaque ~typ

let default tag = {
  raw_ser = sexp_of_gen tag;
  raw_des = (Sexplib.Conv_error.no_matching_variant_found "raw_gentactic");
  raw_hash = (fun st a -> Hash.fold_int st (Hashtbl.hash a));
  raw_compare = Stdlib.compare;
}

let find_reg tag =
  match Map.find tag !registered with
  | v -> v
  | exception Not_found -> default tag

(* little trick to avoid needing to define non-fold hash *)
type _ aux = Gentactic.raw_generic_tactic

let sexp_of_aux _ (Gentactic.Raw (tag, d)) : Sexp.t =
  let r = find_reg tag in
  List [Atom (Gentactic.repr tag); r.raw_ser d]

let aux_of_sexp _ (s:Sexp.t) =
  let tag, d = match s with
    | List [Atom tag; d] -> tag, d
    | _ -> raise (Failure "SEXP Exception")
  in
  match Gentactic.name tag with
  | Some (Any tag) ->
    let r = find_reg tag in
    Gentactic.Raw (tag, r.raw_des d)
  | None ->
    let msg = Format.asprintf "gentactic type %s nt registered, missing plugin?" tag in
    raise (Failure msg)

let hash_fold_aux _ st (Gentactic.Raw (tag,d)) =
  let r = find_reg tag in
  r.raw_hash st d

let compare_aux _ (Gentactic.Raw (tag1,d1)) (Gentactic.Raw (tag2,d2)) =
  match Gentactic.equal tag1 tag2 with
  | None -> Stdlib.compare tag1 (Stdlib.Obj.magic tag2)
  | Some Refl ->
    let r = find_reg tag1 in
    r.raw_compare d1 d2

let rec yojson_to_sexp json = match json with
  | `String s -> Sexp.Atom s
  | `List s -> Sexp.List (List.map yojson_to_sexp s)
  | _ -> raise (Failure "ser_genarg: yojson_to_sexp")

let rec sexp_to_yojson sexp : Yojson.Safe.t =
  match sexp with
  | Sexp.Atom s -> `String s
  | List l -> `List (List.map sexp_to_yojson l)

let aux_of_yojson f x = Ok (aux_of_sexp f @@ yojson_to_sexp x)

let aux_to_yojson f x = sexp_to_yojson @@ sexp_of_aux f x

type aux0 = Aux0
[@@deriving sexp,hash,compare,yojson]

type raw_generic_tactic = aux0 aux
[@@deriving sexp,hash,compare,yojson]
