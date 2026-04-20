(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

type ty_pattern = [%import: Procq.ty_pattern]

let sexp_of_ty_pattern (Procq.TPattern t) =
  Ser_tok.sexp_of_p (fun _ -> assert false) t

let ty_pattern_of_sexp s =
  Procq.TPattern (Ser_tok.p_of_sexp (fun _ -> assert false) s)
