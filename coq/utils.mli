(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors           *)
(************************************************************************)
(* Rocq Language Server Protocol: Misc Utils                            *)
(************************************************************************)

(** Specific to Coq *)
val to_range : lines:string array -> Loc.t -> Lang.Range.t

val to_orange : lines:string array -> Loc.t option -> Lang.Range.t option
