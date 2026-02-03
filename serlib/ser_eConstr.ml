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

module Sorts   = Ser_sorts
module Constr  = Ser_constr
module Environ = Ser_environ
module UVars   = Ser_uvars

module ERtoR = struct

  type t = EConstr.ERelevance.t
  type _t = Sorts.relevance
  [@@deriving sexp,yojson,hash,compare]

  let to_t = EConstr.ERelevance.make
  let of_t = EConstr.Unsafe.to_relevance
end 

module ERelevance = SerType.Biject(ERtoR)

module EItoI = struct

  type t = EConstr.EInstance.t
  type _t = UVars.Instance.t
  [@@deriving sexp,yojson,hash,compare]

  let to_t = EConstr.EInstance.make
  let of_t = EConstr.Unsafe.to_instance
end

module EInstance = SerType.Biject(EItoI)

module EStoS = struct

  type t = EConstr.ESorts.t
  type _t = Sorts.t
  [@@deriving sexp,yojson,hash,compare]

  let to_t = EConstr.ESorts.make
  let of_t = EConstr.Unsafe.to_sorts
end

module ESorts = SerType.Biject(EStoS)

module ECtoC = struct

  type t = EConstr.t
  type _t = Constr.t
  [@@deriving sexp,yojson,hash,compare]

  let to_t = EConstr.of_constr
  let of_t = EConstr.Unsafe.to_constr
end

include SerType.Biject(ECtoC)

type existential =
  [%import: EConstr.existential]
  [@@deriving sexp]

type constr =
  [%import: EConstr.constr]
  [@@deriving sexp,yojson,hash,compare]

type types =
  [%import: EConstr.types]
  [@@deriving sexp]

type unsafe_judgment =
  [%import: EConstr.unsafe_judgment]
  [@@deriving sexp]

type 'a puniverses = [%import: 'a EConstr.puniverses]
[@@deriving sexp]
