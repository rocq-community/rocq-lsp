(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors          *)
(************************************************************************)
(* Flèche => document manager: what states a document keeps alive       *)
(************************************************************************)

(** Ownership of the states a checked document is made of.

    A document holds its own states, but {!States} needs to know about them too,
    or the store's cache bound would drop their ids and with them the memo
    entries that make re-checking the file cheap. This is the policy side of
    that: which documents hold a claim, and when they give it up.

    Only the most recently used documents hold claims; sending a whole
    development through does not pin every file seen. Released states are not
    dropped, they age out of the store's bounded cache, so closing and reopening
    a file stays warm under normal memory pressure. *)

(** [set uri states] makes [uri] claim exactly [states], releasing whatever it
    claimed before, and marks it most recently used. The shared prefix with the
    previous version is recognised physically, so a check that moved nothing
    costs no hashing. *)
val set : Lang.LUri.File.t -> Coq.State.t list -> unit

(** [remove uri] releases everything [uri] claimed, e.g. on close. *)
val remove : Lang.LUri.File.t -> unit
