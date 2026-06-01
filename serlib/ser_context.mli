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

type ('a,'r) pbinder_annot = ('a,'r) Context.pbinder_annot
  [@@deriving sexp,yojson,hash,compare]

module Rel : sig
  module Declaration : sig

    type ('c,'t,'r) pt = ('c,'t,'r) Context.Rel.Declaration.pt
     [@@deriving sexp,yojson,hash,compare]

  end

  type ('c, 't,'r) pt = ('c,'t,'r) Context.Rel.pt
   [@@deriving sexp,yojson,hash,compare]

end

module Named : sig

  module Declaration : sig

    type ('c, 't, 'r) pt = ('c, 't, 'r) Context.Named.Declaration.pt
     [@@deriving sexp,yojson,hash,compare]

  end

  type ('c, 't, 'r) pt = ('c, 't, 'r) Context.Named.pt
   [@@deriving sexp,yojson,hash,compare]

end
