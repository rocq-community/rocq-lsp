(*************************************************************************)
(* Copyright 2015-2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2019-2024 Inria           -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2024-2025 Emilio J. Gallego Arias  -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                     -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors            *)
(*************************************************************************)
(* Rocq Language Server Protocol: Rocq Interp API                        *)
(*************************************************************************)

let coq_interp ~intern ~st cmd =
  let st = State.to_coq st in
  let cmd = Ast.to_coq cmd in
  Vernacinterp.interp ~intern ~st cmd |> State.of_coq

let interp ~token ~intern ~st cmd =
  Protect.eval ~token cmd ~f:(coq_interp ~intern ~st)

module Require = struct
  (* We could improve this Coq upstream by making the API a bit more
     orthogonal *)
  let interp ~intern ~st _files
      { Ast.Require.from; export; mods; loc = _; attrs; control } =
    Vernacinterp.interp ~intern ~st:(State.to_coq st) @@
    CAst.make Vernacexpr.{ control; attrs; expr = VernacSynterp (VernacRequire (from,export,mods)) }
    |> State.of_coq

  let interp ~token ~intern ~st files cmd =
    Protect.eval ~token ~f:(interp ~intern ~st files) cmd
end
