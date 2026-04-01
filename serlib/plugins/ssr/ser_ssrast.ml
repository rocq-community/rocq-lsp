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

open Serlib

module Loc        = Ser_loc
module Names      = Ser_names
module Locus      = Ser_locus
module Constrexpr = Ser_constrexpr
module Genintern  = Ser_genintern
module Geninterp  = Ser_geninterp

module Ssrmatching_plugin = struct
  module Ssrmatching = Serlib_ssrmatching.Ser_ssrmatching
end

module Ltac_plugin = struct
  module Tacexpr = Serlib_ltac.Ser_tacexpr
  module Tacinterp = Serlib_ltac.Ser_tacinterp
  module Tacarg = Serlib_ltac.Ser_tacarg
end

type ssrtermkind = Ssrmatching_plugin.Ssrmatching.ssrtermkind
  [@@deriving sexp,yojson,hash,compare]

(* What a hack is ssreflect using... *)
module Proofview = struct
  type 'a tactic = 'a Proofview.tactic
  let tactic_of_sexp _ = Serlib_base.opaque_of_sexp ~typ:"Ssrast.tactic"
  let sexp_of_tactic _ = Serlib_base.sexp_of_opaque ~typ:"Ssrast.tactic"
end

type ssrhyp =
  [%import: Wrap_ssrast.ssrhyp]
  [@@deriving sexp,yojson,hash,compare]

type ssrhyp_or_id =
  [%import: Wrap_ssrast.ssrhyp_or_id]
  [@@deriving sexp,yojson,hash,compare]

type ssrhyps =
  [%import: Wrap_ssrast.ssrhyps]
  [@@deriving sexp,yojson,hash,compare]

type ssrdir =
  [%import: Wrap_ssrast.ssrdir]
  [@@deriving sexp,yojson,hash,compare]

type ssrsimpl =
  [%import: Wrap_ssrast.ssrsimpl]
  [@@deriving sexp,yojson,hash,compare]

type ssrmmod =
  [%import: Wrap_ssrast.ssrmmod]
  [@@deriving sexp,yojson,hash,compare]

type ssrmult =
  [%import: Wrap_ssrast.ssrmult]
  [@@deriving sexp,yojson,hash,compare]

type ssrocc =
  [%import: Wrap_ssrast.ssrocc]
  [@@deriving sexp,yojson,hash,compare]

type ssrindex =
  [%import: Wrap_ssrast.ssrindex]
  [@@deriving sexp,yojson,hash,compare]

type ssrclear =
  [%import: Wrap_ssrast.ssrclear]
  [@@deriving sexp,yojson,hash,compare]

type ssrdocc =
  [%import: Wrap_ssrast.ssrdocc]
  [@@deriving sexp,yojson,hash,compare]

type ssrterm =
  [%import: Wrap_ssrast.ssrterm]
  [@@deriving sexp,yojson,hash,compare]

type ast_glob_env =
  [%import: Wrap_ssrast.ast_glob_env]
  [@@deriving sexp,yojson,hash,compare]

type ast_closure_term =
  [%import: Wrap_ssrast.ast_closure_term]
  [@@deriving sexp,yojson,hash,compare]

type ssrview =
  [%import: Wrap_ssrast.ssrview]
  [@@deriving sexp,yojson,hash,compare]

type anon_kind =
  [%import: Wrap_ssrast.anon_kind]
  [@@deriving sexp,yojson,hash,compare]

(* type anon_iter =
 *   [%import: Wrap_ssrast.anon_iter]
 *   [@@deriving sexp] *)

type id_block =
  [%import: Wrap_ssrast.id_block]
  [@@deriving sexp,yojson,hash,compare]

type ssripat =
  [%import: Wrap_ssrast.ssripat]
  [@@deriving sexp,yojson,hash,compare]
and ssripats =
  [%import: Wrap_ssrast.ssripats]
  [@@deriving sexp,yojson,hash,compare]
and ssripatss =
  [%import: Wrap_ssrast.ssripatss]
  [@@deriving sexp,yojson,hash,compare]
and ssripatss_or_block =
  [%import: Wrap_ssrast.ssripatss_or_block]
  [@@deriving sexp,yojson,hash,compare]

type ssrhpats =
  [%import: Wrap_ssrast.ssrhpats]
  [@@deriving sexp,yojson,hash,compare]

type ssrhpats_wtransp =
  [%import: Wrap_ssrast.ssrhpats_wtransp]
  [@@deriving sexp,yojson,hash,compare]

type ssrintrosarg =
  [%import: Wrap_ssrast.ssrintrosarg]
  [@@deriving sexp,yojson,hash,compare]

type ssrfwdid =
  [%import: Wrap_ssrast.ssrfwdid]
  [@@deriving sexp,yojson,hash,compare]

type 'term ssrbind =
  [%import: 'term Wrap_ssrast.ssrbind]
  [@@deriving sexp,yojson,hash,compare]

type ssrbindfmt =
  [%import: Wrap_ssrast.ssrbindfmt]
  [@@deriving sexp,yojson,hash,compare]

type 'term ssrbindval =
  [%import: 'term Wrap_ssrast.ssrbindval]
  [@@deriving sexp,yojson,hash,compare]

type ssrfwdkind =
  [%import: Wrap_ssrast.ssrfwdkind]
  [@@deriving sexp,yojson,hash,compare]

type ssrfwdfmt =
  [%import: Wrap_ssrast.ssrfwdfmt]
  [@@deriving sexp,yojson,hash,compare]

type ssrclseq =
  [%import: Wrap_ssrast.ssrclseq]
  [@@deriving sexp,yojson,hash,compare]

type 'tac ssrhint =
  [%import: 'tac Wrap_ssrast.ssrhint]
  [@@deriving sexp,yojson,hash,compare]

type 'tac fwdbinders =
  [%import: 'tac Wrap_ssrast.fwdbinders]
  [@@deriving sexp,yojson,hash,compare]

type 'tac ffwbinders =
  [%import: 'tac Wrap_ssrast.ffwbinders]
  [@@deriving sexp,yojson,hash,compare]

type clause =
  [%import: Wrap_ssrast.clause]
  [@@deriving sexp,yojson,hash,compare]

type clauses =
  [%import: Wrap_ssrast.clauses]
  [@@deriving sexp,yojson,hash,compare]

type wgen =
  [%import: Wrap_ssrast.wgen]
  [@@deriving sexp,yojson,hash,compare]

type 'a ssrdoarg =
  [%import: 'a Wrap_ssrast.ssrdoarg]
  [@@deriving sexp,yojson,hash,compare]

type 'a ssrseqarg =
  [%import: 'a Wrap_ssrast.ssrseqarg]
  [@@deriving sexp,yojson,hash,compare]

type 'a ssragens =
  [%import: 'a Wrap_ssrast.ssragens]
  [@@deriving sexp,yojson,hash,compare]

type ssrapplyarg =
  [%import: Wrap_ssrast.ssrapplyarg]
  [@@deriving sexp,yojson,hash,compare]

type 'a ssrcasearg =
  [%import: 'a Wrap_ssrast.ssrcasearg]
  [@@deriving sexp,yojson,hash,compare]

type 'a ssrmovearg =
  [%import: 'a Wrap_ssrast.ssrmovearg]
  [@@deriving sexp,yojson,hash,compare]
