(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors           *)
(************************************************************************)
(* Rocq Language Server Protocol: Misc Utils                            *)
(************************************************************************)

(* We convert in case of failure to some default values *)

let utf16_offset_of_utf8_offset ~lines ~line ~byte =
  if line < Array.length lines then
    let line = Array.get lines line in
    Lang.Utf.utf16_offset_of_utf8_offset ~line ~offset:byte
  else 0

let to_range ~lines (p : Loc.t) : Lang.Range.t =
  let Loc.{ line_nb; line_nb_last; bol_pos; bol_pos_last; bp; ep; _ } = p in

  let start_line = line_nb - 1 in
  let end_line = line_nb_last - 1 in

  (* cols *)
  let start_col = bp - bol_pos in
  let end_col = ep - bol_pos_last in

  let start_col =
    utf16_offset_of_utf8_offset ~lines ~line:start_line ~byte:start_col
  in
  let end_col =
    utf16_offset_of_utf8_offset ~lines ~line:end_line ~byte:end_col
  in
  Lang.Range.
    { start = { line = start_line; character = start_col; offset = bp }
    ; end_ = { line = end_line; character = end_col; offset = ep }
    }

let to_orange ~lines = Option.map (to_range ~lines)

let with_control ~fn ~control ~st =
  let open VernacControl in
  let control = from_syntax control in
  let control, () =
    under_control ~loc:None ~with_local_state:trivial_state control ~noop:() fn
  in
  let noop = after_last_phase ~loc:None control in
  let () =
    if noop then (
      Vernacstate.unfreeze_full_state (State.to_coq st))
  in
  ()
