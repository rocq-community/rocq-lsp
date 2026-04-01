(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)
open Sexplib
open Ppx_hash_lib.Std

type ('raw, 'glb) gen_ser =
  { raw_ser : 'raw -> Sexp.t
  ; raw_des : Sexp.t -> 'raw
  ; raw_hash : 'raw Ppx_hash_lib.Std.Hash.folder
  ; raw_compare : 'raw -> 'raw -> int

  ; glb_ser : 'glb -> Sexp.t
  ; glb_des : Sexp.t -> 'glb
  ; glb_hash : 'glb Ppx_hash_lib.Std.Hash.folder
  ; glb_compare : 'glb -> 'glb -> int
  }


module type GenSer = sig
  type raw [@@deriving sexp,hash,compare,yojson]
  type glb [@@deriving sexp,hash,compare,yojson]
end

module GS (M:GenSer) = struct
  let genser =
    { raw_ser = M.sexp_of_raw
    ; raw_des = M.raw_of_sexp
    ; raw_hash = M.hash_fold_raw
    ; raw_compare = M.compare_raw

    ; glb_ser = M.sexp_of_glb
    ; glb_des = M.glb_of_sexp
    ; glb_hash = M.hash_fold_glb
    ; glb_compare = M.compare_glb
    }
end

module Obj = struct
  type ('raw,'glb) t = ('raw,'glb) gen_ser
end

module R = GenConstr.Register(Obj)

let register = R.register

let register_mod (type raw glb) tag (d:(module GenSer with type raw = raw and type glb = glb)) =
  let module D = (val d) in
  let module M = GS(D) in
  register tag M.genser

let sexp_of_gen t tag =
  let typ = t^"_genconstr: " ^ Sexp.to_string (Atom (GenConstr.repr tag)) in
  Serlib_base.sexp_of_opaque ~typ

let default tag = {
  raw_ser = sexp_of_gen "raw" tag;
  raw_des = (Sexplib.Conv_error.no_matching_variant_found "raw_genconstr");
  raw_hash = (fun st a -> Hash.fold_int st (Hashtbl.hash a));
  raw_compare = Stdlib.compare;
  glb_ser = sexp_of_gen "glb" tag;
  glb_des = (Sexplib.Conv_error.no_matching_variant_found "glb_genconstr");
  glb_hash = (fun st a -> Hash.fold_int st (Hashtbl.hash a));
  glb_compare = Stdlib.compare;
}

let find_reg tag =
  match R.find_opt tag with
  | Some v -> v
  | None -> default tag

let rec yojson_to_sexp json = match json with
  | `String s -> Sexp.Atom s
  | `List s -> Sexp.List (List.map yojson_to_sexp s)
  | _ -> raise (Failure "ser_genarg: yojson_to_sexp")

let rec sexp_to_yojson sexp : Yojson.Safe.t =
  match sexp with
  | Sexp.Atom s -> `String s
  | List l -> `List (List.map sexp_to_yojson l)

(* little trick to avoid needing to define non-fold hash *)
type _ raw0 = GenConstr.raw
type _ glb0 = GenConstr.glb

let sexp_of_raw0 _ (GenConstr.Raw (tag, d)) : Sexp.t =
  let r = find_reg tag in
  List [Atom (GenConstr.repr tag); r.raw_ser d]

let raw0_of_sexp _ (s:Sexp.t) =
  let tag, d = match s with
    | List [Atom tag; d] -> tag, d
    | _ -> raise (Failure "SEXP Exception")
  in
  match GenConstr.name tag with
  | Some (Any tag) ->
    let r = find_reg tag in
    GenConstr.Raw (tag, r.raw_des d)
  | None ->
    let msg = Format.asprintf "gentactic type %s nt registered, missing plugin?" tag in
    raise (Failure msg)

let hash_fold_raw0 _ st (GenConstr.Raw (tag,d)) =
  let r = find_reg tag in
  r.raw_hash st d

let compare_raw0 _ (GenConstr.Raw (tag1,d1)) (GenConstr.Raw (tag2,d2)) =
  match GenConstr.eq tag1 tag2 with
  | None -> Stdlib.compare tag1 (Stdlib.Obj.magic tag2)
  | Some Refl ->
    let r = find_reg tag1 in
    r.raw_compare d1 d2

let raw0_of_yojson f x = Ok (raw0_of_sexp f @@ yojson_to_sexp x)

let raw0_to_yojson f x = sexp_to_yojson @@ sexp_of_raw0 f x

let sexp_of_glb0 _ (GenConstr.Glb (tag, d)) : Sexp.t =
  let r = find_reg tag in
  List [Atom (GenConstr.repr tag); r.glb_ser d]

let glb0_of_sexp _ (s:Sexp.t) =
  let tag, d = match s with
    | List [Atom tag; d] -> tag, d
    | _ -> raise (Failure "SEXP Exception")
  in
  match GenConstr.name tag with
  | Some (Any tag) ->
    let r = find_reg tag in
    GenConstr.Glb (tag, r.glb_des d)
  | None ->
    let msg = Format.asprintf "gentactic type %s nt registered, missing plugin?" tag in
    raise (Failure msg)

let hash_fold_glb0 _ st (GenConstr.Glb (tag,d)) =
  let r = find_reg tag in
  r.glb_hash st d

let compare_glb0 _ (GenConstr.Glb (tag1,d1)) (GenConstr.Glb (tag2,d2)) =
  match GenConstr.eq tag1 tag2 with
  | None -> Stdlib.compare tag1 (Stdlib.Obj.magic tag2)
  | Some Refl ->
    let r = find_reg tag1 in
    r.glb_compare d1 d2

let glb0_of_yojson f x = Ok (glb0_of_sexp f @@ yojson_to_sexp x)

let glb0_to_yojson f x = sexp_to_yojson @@ sexp_of_glb0 f x

type aux0 = Aux0
[@@deriving sexp,hash,compare,yojson]

type raw = aux0 raw0
[@@deriving sexp,hash,compare,yojson]

type glb = aux0 glb0
[@@deriving sexp,hash,compare,yojson]
