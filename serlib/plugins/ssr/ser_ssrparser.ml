(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
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

module Ssrmatching = Serlib_ssrmatching.Ser_ssrmatching
open Ssrmatching

module Ltac_plugin = struct
  module Tacexpr = Serlib_ltac.Ser_tacexpr
end

module Ssrast = Ser_ssrast

module Ssreflect_plugin = struct
  module Ssrast = Ser_ssrast
  module Ssrequality = Ser_ssrequality
  module Ssrparser = Ssreflect_plugin.Ssrparser
  module Ssrtacs = Ssreflect_plugin.Ssrtacs
end

open! Ssrast

type t_movearg = (cpattern ssragens) ssrmovearg
[@@deriving sexp,yojson,hash,compare]

let ser_wit_ssrmovearg  =
  Ser_genarg.mk_uniform sexp_of_t_movearg t_movearg_of_sexp hash_fold_t_movearg compare_t_movearg

let ser_wit_ssrapplyarg =
  Ser_genarg.mk_uniform sexp_of_ssrapplyarg ssrapplyarg_of_sexp hash_fold_ssrapplyarg compare_ssrapplyarg

let ser_wit_clauses =
  Ser_genarg.mk_uniform sexp_of_clauses clauses_of_sexp hash_fold_clauses compare_clauses

type t_rwarg = Ssreflect_plugin.Ssrequality.ssrrwarg list [@@deriving sexp,hash,compare]

let ser_wit_ssrrwargs =
  Ser_genarg.mk_uniform sexp_of_t_rwarg t_rwarg_of_sexp hash_fold_t_rwarg compare_t_rwarg

module A0 = struct
  type raw = Ltac_plugin.Tacexpr.raw_tactic_expr fwdbinders
  [@@deriving sexp,yojson,hash,compare]

  type glb = Ltac_plugin.Tacexpr.glob_tactic_expr fwdbinders
  [@@deriving sexp,yojson,hash,compare]

  type top = Ltac_plugin.Tacarg.tacvalue fwdbinders
  [@@deriving sexp,hash,compare]
end

let ser_wit_ssrhavefwdwbinders = let module M = Ser_genarg.GS(A0) in M.genser

type ssrfwdview =
  [%import: Ssreflect_plugin.Ssrparser.ssrfwdview]
  [@@deriving sexp,yojson,hash,compare]

type ssreqid =
  [%import: Ssreflect_plugin.Ssrparser.ssreqid]
  [@@deriving sexp,yojson,hash,compare]

type ssrarg =
  [%import: Ssreflect_plugin.Ssrparser.ssrarg]
  [@@deriving sexp,yojson,hash,compare]

let ser_wit_ssrarg =
  Ser_genarg.mk_uniform sexp_of_ssrarg ssrarg_of_sexp hash_fold_ssrarg compare_ssrarg

module A1 = struct
  type raw = Ltac_plugin.Tacexpr.raw_tactic_expr ssrhint
  [@@deriving sexp,yojson,hash,compare]
  type glb = Ltac_plugin.Tacexpr.glob_tactic_expr ssrhint
  [@@deriving sexp,yojson,hash,compare]
  type top = Ltac_plugin.Tacarg.tacvalue ssrhint
  [@@deriving sexp,hash,compare]
end

let ser_wit_ssrhintarg = let module M = Ser_genarg.GS(A1) in M.genser

module A2 = struct
  type raw = Serlib_ltac.Ser_tacexpr.raw_tactic_expr ssrseqarg
  [@@deriving sexp,yojson,hash,compare]

  type glb = Serlib_ltac.Ser_tacexpr.glob_tactic_expr ssrseqarg
  [@@deriving sexp,yojson,hash,compare]

  type top = Ltac_plugin.Tacarg.tacvalue ssrseqarg
  [@@deriving sexp,hash,compare]
end

let ser_wit_ssrseqarg = let module M = Ser_genarg.GS(A2) in M.genser

module A3 = struct
  type raw = Serlib_ltac.Ser_tacexpr.raw_tactic_expr * ssripats
  [@@deriving sexp,yojson,hash,compare]
  type glb = Serlib_ltac.Ser_tacexpr.glob_tactic_expr * ssripats
  [@@deriving sexp,yojson,hash,compare]
  type top = Ltac_plugin.Tacarg.tacvalue * ssripats
  [@@deriving sexp,hash,compare]
end

let ser_wit_ssrintrosarg = let module M = Ser_genarg.GS(A3) in M.genser

module A4 = struct
  type raw = Serlib_ltac.Ser_tacexpr.raw_tactic_expr ffwbinders
  [@@deriving sexp,yojson,hash,compare]
  type glb = Serlib_ltac.Ser_tacexpr.glob_tactic_expr ffwbinders
  [@@deriving sexp,yojson,hash,compare]
  type top = Ltac_plugin.Tacarg.tacvalue ffwbinders
  [@@deriving sexp,hash,compare]
end

let ser_wit_ssrsufffwd = let module M = Ser_genarg.GS(A4) in M.genser

module A5 = struct
  type t = ((int * Ssreflect_plugin.Ssrast.ssrterm) * Ssrmatching_plugin.Ssrmatching.cpattern ssragens)
  [@@deriving sexp,yojson,hash,compare]
end

let ser_wit_ssrcongrarg = let module M = Ser_genarg.GS0(A5) in M.genser

module A6 = struct
  type raw = Serlib_ltac.Ser_tacexpr.raw_tactic_expr ssrdoarg
  [@@deriving sexp,yojson,hash,compare]
  type glb = Serlib_ltac.Ser_tacexpr.glob_tactic_expr ssrdoarg
  [@@deriving sexp,yojson,hash,compare]
  type top = Ltac_plugin.Tacarg.tacvalue ssrdoarg
  [@@deriving sexp,hash,compare]
end

let ser_wit_ssrdoarg = let module M = Ser_genarg.GS(A6) in M.genser

module A7 = struct
  type t = ((ssrfwdfmt * (cpattern * ast_closure_term option)) * ssrdocc)
  [@@deriving sexp,yojson,hash,compare]
end

let ser_wit_ssrsetfwd = let module M = Ser_genarg.GS0(A7) in M.genser

module A8 = struct
  type raw = Serlib_ltac.Ser_tacexpr.raw_tactic_expr ssrhint
  [@@deriving sexp,yojson,hash,compare]
  type glb = Serlib_ltac.Ser_tacexpr.glob_tactic_expr ssrhint
  [@@deriving sexp,yojson,hash,compare]
  type top = Ltac_plugin.Tacarg.tacvalue ssrhint
  [@@deriving sexp,hash,compare]
end

let ser_wit_ssrhint = let module M = Ser_genarg.GS(A8) in M.genser

module A9 = struct
  type t = ssrfwdfmt * ast_closure_term
  [@@deriving sexp,yojson,hash,compare]
end

let ser_wit_ssrposefwd = let module M = Ser_genarg.GS0(A9) in M.genser

module A10 = struct
  type t = ssrocc * ssrterm
  [@@deriving sexp,yojson,hash,compare]
end

let ser_wit_ssrunlockarg = let module M = Ser_genarg.GS0(A10) in M.genser

module A11 = struct
  type t = clause list * (ssrfwdfmt * ast_closure_term)
  [@@deriving sexp,yojson,hash,compare]
end

let ser_wit_ssrwlogfwd = let module M = Ser_genarg.GS0(A11) in M.genser

module A12 = struct
  type t = Names.Id.t * (ssrfwdfmt * ast_closure_term)
  [@@deriving sexp,yojson,hash,compare]
end

let ser_wit_ssrfixfwd = let module M = Ser_genarg.GS0(A12) in M.genser

module A13 = struct
  type t = ssrfwdfmt * ast_closure_term
  [@@deriving sexp,yojson,hash,compare]
end

let ser_wit_ssrfwd = let module M = Ser_genarg.GS0(A13) in M.genser

module A14 = struct
  type t = cpattern ssragens
  [@@deriving sexp,yojson,hash,compare]
end

let ser_wit_ssrdgens = let module M = Ser_genarg.GS0(A14) in M.genser

let register () =
  let open Ser_genarg in
  let open Ssreflect_plugin.Ssrparser in
  let open Ssreflect_plugin.Ssrtacs in

  register_genser wit_ssrapplyarg        ser_wit_ssrapplyarg;
  register_genser wit_ssrarg             ser_wit_ssrarg;
  register_genser wit_ssrcasearg         ser_wit_ssrmovearg;
  register_genser wit_ssrclauses         ser_wit_clauses;
  register_genser wit_ssrmovearg         ser_wit_ssrmovearg;
  register_genser wit_ssrhavefwdwbinders ser_wit_ssrhavefwdwbinders;
  register_genser wit_ssrhintarg         ser_wit_ssrhintarg;
  register_genser wit_ssrrwargs          ser_wit_ssrrwargs;

  (* register_genser wit_ast_closure_lterm ser_wit_ast_closure_lterm
     register_genser wit_ast_closure_term  ser_wit_ast_closure_term
     register_genser wit_ssr_idcomma       ser_wit_ssr_idcomma
     register_genser wit_ssragen           ser_wit_ssragen
     register_genser wit_ssragens          ser_wit_ssragens
     register_genser wit_ssrbinder         ser_wit_ssrbinder
     register_genser wit_ssrbvar           ser_wit_ssrbvar
     register_genser wit_ssrbwdview        ser_wit_ssrbwdview
     register_genser wit_ssrclausehyps     ser_wit_ssrclausehyps
     register_genser wit_ssrclear          ser_wit_ssrclear
     register_genser wit_ssrclear_ne       ser_wit_ssrclear_ne
     register_genser wit_ssrclseq          ser_wit_ssrclseq
     register_genser wit_ssrcofixfwd       ser_wit_ssrcofixfwd *)

  register_genser wit_ssrcongrarg ser_wit_ssrcongrarg;
  register_genser wit_ssrcpat     (mk_uniform sexp_of_ssripat ssripat_of_sexp hash_fold_ssripat compare_ssripat);
  register_genser wit_ssrdgens    ser_wit_ssrdgens;
  register_genser wit_ssrdgens_tl ser_wit_ssrdgens;
  register_genser wit_ssrdir      (mk_uniform sexp_of_ssrdir ssrdir_of_sexp hash_fold_ssrdir compare_ssrdir);
  register_genser wit_ssrdoarg    ser_wit_ssrdoarg;
(*
  Ssreflect_plugin.Ssrparser.wit_ssrdocc
  Ssreflect_plugin.Ssrparser.wit_ssreqid
*)
  register_genser wit_ssrexactarg ser_wit_ssrapplyarg;
  register_genser wit_ssrfixfwd   ser_wit_ssrfixfwd;
  register_genser wit_ssrfwd      ser_wit_ssrfwd;
  register_genser wit_ssrfwdfmt   (mk_uniform sexp_of_ssrfwdfmt ssrfwdfmt_of_sexp hash_fold_ssrfwdfmt compare_ssrfwdfmt);
  register_genser wit_ssrfwdid    (let module M = Ser_genarg.GS0(Ser_names.Id) in M.genser);

(*
  Ssreflect_plugin.Ssrparser.wit_ssrfwdview
  Ssreflect_plugin.Ssrparser.wit_ssrgen
  Ssreflect_plugin.Ssrparser.wit_ssrhavefwd
*)
  register_genser wit_ssrhint ser_wit_ssrhint;
(*
  Ssreflect_plugin.Ssrparser.wit_ssrhoi_hyp
  Ssreflect_plugin.Ssrparser.wit_ssrhoi_id
*)
  register_genser wit_ssrhpats         (mk_uniform sexp_of_ssrhpats ssrhpats_of_sexp hash_fold_ssrhpats compare_ssrhpats);
  register_genser wit_ssrhpats_nobs    (mk_uniform sexp_of_ssrhpats ssrhpats_of_sexp hash_fold_ssrhpats compare_ssrhpats);
  register_genser wit_ssrhpats_wtransp (mk_uniform sexp_of_ssrhpats_wtransp ssrhpats_wtransp_of_sexp hash_fold_ssrhpats_wtransp compare_ssrhpats_wtransp);

(*
  Ssreflect_plugin.Ssrparser.wit_ssrhyp
  Ssreflect_plugin.Ssrparser.wit_ssrhoi_hyp
  Ssreflect_plugin.Ssrparser.wit_ssrhoi_id
  Ssreflect_plugin.Ssrparser.wit_ssrhoirep
  Ssreflect_plugin.Ssrparser.wit_ssrindex
  Ssreflect_plugin.Ssrparser.wit_ssrintros
  Ssreflect_plugin.Ssrparser.wit_ssrintros_ne
*)
  register_genser wit_ssrintrosarg ser_wit_ssrintrosarg;
(*
  Ssreflect_plugin.Ssrparser.wit_ssriorpat
  Ssreflect_plugin.Ssrparser.wit_ssripat
  Ssreflect_plugin.Ssrparser.wit_ssripatrep
  Ssreflect_plugin.Ssrparser.wit_ssripats_ne
  Ssreflect_plugin.Ssrparser.wit_ssripats

  Ssreflect_plugin.Ssrparser.wit_ssrmmod
  Ssreflect_plugin.Ssrparser.wit_ssrmult
  Ssreflect_plugin.Ssrparser.wit_ssrmult_ne

  Ssreflect_plugin.Ssrparser.wit_ssrocc
  Ssreflect_plugin.Ssrparser.wit_ssrortacarg
  Ssreflect_plugin.Ssrparser.wit_ssrortacs
  Ssreflect_plugin.Ssrparser.wit_ssrpattern_squarep
  Ssreflect_plugin.Ssrparser.wit_ssrpattern_ne_squarep
*)
  register_genser wit_ssrposefwd ser_wit_ssrposefwd;
  register_genser wit_ssrrpat    (mk_uniform sexp_of_ssripat ssripat_of_sexp hash_fold_ssripat compare_ssripat);

(*
  Ssreflect_plugin.Ssrparser.wit_ssrrule
  Ssreflect_plugin.Ssrparser.wit_ssrrule_ne
  Ssreflect_plugin.Ssrparser.wit_ssrrwarg

  Ssreflect_plugin.Ssrparser.wit_ssrrwkind
  Ssreflect_plugin.Ssrparser.wit_ssrrwocc
  *)

  register_genser wit_ssrseqarg ser_wit_ssrseqarg;
  register_genser wit_ssrseqdir (mk_uniform sexp_of_ssrdir ssrdir_of_sexp hash_fold_ssrdir compare_ssrdir);
  register_genser wit_ssrsetfwd ser_wit_ssrsetfwd;
(*
  Ssreflect_plugin.Ssrparser.wit_ssrsimpl
  Ssreflect_plugin.Ssrparser.wit_ssrsimpl_ne
  Ssreflect_plugin.Ssrparser.wit_ssrsimplrep
  Ssreflect_plugin.Ssrparser.wit_ssrstruct
*)
  register_genser wit_ssrsufffwd ser_wit_ssrsufffwd;
  register_genser wit_ssrtacarg Serlib_ltac.Ser_tacarg.ser_wit_tactic;
  register_genser wit_ssrtclarg Serlib_ltac.Ser_tacarg.ser_wit_tactic;

  register_genser wit_ssrterm       (mk_uniform sexp_of_ssrterm ssrterm_of_sexp hash_fold_ssrterm compare_ssrterm);
  register_genser wit_ssrunlockarg  ser_wit_ssrunlockarg;
  register_genser wit_ssrunlockargs (gen_ser_list ser_wit_ssrunlockarg);
  register_genser wit_ssrwgen       (mk_uniform sexp_of_clause clause_of_sexp hash_fold_clause compare_clause);
  register_genser wit_ssrwlogfwd    ser_wit_ssrwlogfwd;
  ()

let _ = register ()
