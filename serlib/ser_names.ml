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

open Names

module Int  = Ser_int
module CAst = Ser_cAst

(************************************************************************)
(* Serialization of Names.mli                                           *)
(************************************************************************)

module Id = struct

  (* Id.t: private *)
  module Id_ = struct
    type t = Names.Id.t
    type _t = Id of string [@@deriving sexp, yojson, hash, compare]
    let of_t id = Id (Id.to_string id)
    let to_t (Id id) = Id.of_string_soft id
  end

  module Self = SerType.Biject(Id_)

  include Self

  module Set = Ser_cSet.Make(Names.Id.Set)(Self)
  module Map = Ser_cMap.Make(Names.Id.Map)(Self)

end

module Name = struct

(* Name.t: public *)
type t =
  [%import: Names.Name.t]
  [@@deriving sexp,yojson,hash,compare]

end

module DirPath = struct

  (* DirPath.t: private *)
  module DirPath_ = struct
    type t = Names.DirPath.t
    type _t = DirPath of Id.t list
    [@@deriving sexp,yojson,hash,compare]

    let of_t dp = DirPath (DirPath.repr dp)
    let to_t (DirPath dpl) = DirPath.make dpl
  end

  module Self = SerType.Biject(DirPath_)

  include Self

  module Set = Ser_cSet.Make(Names.DirPath.Set)(Self)
  module Map = Ser_cMap.Make(Names.DirPath.Map)(Self)

end

module DPmap = DirPath.Map

module MBId = struct

  (* MBId.t: private *)
  module MBIdBij = struct
    type t = [%import: Names.MBId.t]

    type _t = Mbid of int * Id.t * DirPath.t
    [@@deriving sexp,yojson,hash,compare]

  end

include SerType.Pierce(MBIdBij)

end

module ModPath = struct

  (* ModPath.t: public *)
  module Self = struct
    type t = [%import: Names.ModPath.t]
    [@@deriving sexp,yojson,hash,compare]
  end

  include Self

  module Set = Ser_cSet.Make(Names.ModPath.Set)(Self)
  module Map = Ser_cMap.Make(Names.ModPath.Map)(Self)

end

module MPmap = ModPath.Map

(* KerName: private *)
module KerName = struct

  module Self = struct
    type t = [%import: Names.KerName.t]

    type _t = KerName of ModPath.t * Id.t
    [@@deriving sexp,yojson,hash,compare]

    let _t_put kn              =
      let mp, l = KerName.repr kn in KerName (mp,l)
    let _kername_get (KerName (mp,l)) = KerName.make mp l

    let t_of_sexp sexp = _kername_get (_t_of_sexp sexp)
    let sexp_of_t kn   = sexp_of__t (_t_put kn)

    let of_yojson json = Ppx_deriving_yojson_runtime.(_t_of_yojson json >|= _kername_get)
    let to_yojson kn   = _t_to_yojson (_t_put kn)

    let hash x = hash__t (_t_put x)
    let hash_fold_t st id = hash_fold__t st (_t_put id)

    let compare x y = compare__t (_t_put x) (_t_put y)

    let equal = KerName.equal

  end
  include Self

  module Set = Ser_cSet.Make(Names.KerName.Set)(Self)
  module Map = Ser_cMap.Make(Names.KerName.Map)(Self)

end

module KNmap = KerName.Map

module Constant = struct

(* Constant.t: private *)
type t = [%import: Names.Constant.t]

type _t = Constant of KerName.t * KerName.t option
      [@@deriving sexp,yojson,hash,compare]

let _t_put cs =
  let cu, cc = Constant.(user cs, canonical cs) in
  if KerName.equal cu cc then Constant (cu, None) else Constant (cu, Some cc)
let _t_get = function
  | Constant (cu, None) -> Constant.make1 cu
  | Constant (cu, Some cc) -> Constant.make cu cc

let t_of_sexp sexp = _t_get (_t_of_sexp sexp)
let sexp_of_t dp   = sexp_of__t (_t_put dp)

let of_yojson json = Ppx_deriving_yojson_runtime.(_t_of_yojson json >|= _t_get)
let to_yojson level = _t_to_yojson (_t_put level)

let hash x = hash__t (_t_put x)
let hash_fold_t st id = hash_fold__t st (_t_put id)

let compare x y = compare__t (_t_put x) (_t_put y)

end

module Cset_env = Ser_cSet.Make(Cset_env)(Constant)

module Cmap = Ser_cMap.Make(Cmap)(Constant) [@@warning "-3"]
module Cmap_env = Ser_cMap.Make(Cmap_env)(Constant)

module MutInd = struct

(* MutInd.t: private *)
  module BijectSpec = struct
    type t = [%import: Names.MutInd.t]
    type _t = MutInd of KerName.t * KerName.t option
    [@@deriving sexp,yojson,hash,compare]

    let of_t cs =
      let cu, cc = MutInd.(user cs, canonical cs) in
      if KerName.equal cu cc then MutInd (cu, None) else MutInd (cu, Some cc)

    let to_t = function
      | MutInd (cu, None) -> MutInd.make1 cu
      | MutInd (cu, Some cc) -> MutInd.make cu cc
  end

  include SerType.Biject(BijectSpec)
end

module Mindmap_env = Ser_cMap.Make(Mindmap_env)(MutInd)

type 'a tableKey =
  [%import: 'a Names.tableKey]
  [@@deriving sexp]

type variable =
  [%import: Names.variable]
  [@@deriving sexp,yojson,hash,compare]

(* Inductive and constructor = public *)
module Ind = struct
  type t =
  [%import: Names.Ind.t]
  [@@deriving sexp,yojson,hash,compare]
end

module Indset_env = Ser_cSet.Make(Indset_env)(Ind)
module Indmap_env = Ser_cMap.Make(Indmap_env)(Ind)

type inductive =
  [%import: Names.inductive]
  [@@deriving sexp,yojson,hash,compare]

module Construct = struct
  type t =
  [%import: Names.Construct.t]
  [@@deriving sexp,yojson,hash,compare]

end
type constructor =
  [%import: Names.constructor]
  [@@deriving sexp,yojson,hash,compare]

(* Projection: private *)
module Projection = struct

  module Repr = struct
    module PierceSpec = struct
      type t = Names.Projection.Repr.t
      type _t =
        { proj_ind : inductive
        ; proj_relevant : bool
        ; proj_npars : int
        ; proj_arg : int
        ; proj_name : Id.t
        } [@@deriving sexp,yojson,hash,compare]
    end
    include SerType.Pierce(PierceSpec)
  end

  module PierceSpec = struct
    type t = [%import: Names.Projection.t]
    type _t = Repr.t * bool
    [@@deriving sexp,yojson,hash,compare]
  end
  include SerType.Pierce(PierceSpec)
end

module GlobRef = struct

type t = [%import: Names.GlobRef.t]
  [@@deriving sexp,yojson,hash,compare]

end

type lident =
  [%import: Names.lident]
  [@@deriving sexp,yojson,hash,compare]

type lname =
  [%import: Names.lname]
  [@@deriving sexp,yojson,hash,compare]

type lstring =
  [%import: Names.lstring]
  [@@deriving sexp,yojson,hash,compare]
