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

include SerType.SJHC with type t = Sorts.t

type relevance = Sorts.relevance [@@deriving sexp,yojson,hash,compare]
type ('q, 'u) pattern = ('q, 'u) Sorts.pattern [@@deriving sexp,yojson,hash,compare]

module QVar : sig
  include SerType.SJHC with type t = Sorts.QVar.t
  module Set : SerType.SJHC with type t = Sorts.QVar.Set.t
end

module Quality : sig
  type constant = Sorts.Quality.constant [@@deriving sexp,yojson,hash,compare]

  include SerType.SJHC with type t = Sorts.Quality.t
  module Set : SerType.SJHC with type t = Sorts.Quality.Set.t

  type 'q pattern = 'q Sorts.Quality.pattern [@@deriving sexp,yojson,hash,compare]
end

module ElimConstraint : sig
  type kind = Sorts.ElimConstraint.kind
  [@@deriving sexp,yojson,hash,compare]

  include SerType.SJHC with type t = Sorts.ElimConstraint.t
end

module ElimConstraints : SerType.SJHC with type t = Sorts.ElimConstraints.t
