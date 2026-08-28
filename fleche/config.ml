(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors           *)
(************************************************************************)
(* Rocq Language Server Protocol: Core Configuration                    *)
(************************************************************************)

module Completion = struct
  (** Module that enables LaTeX-like completion for unicode symbols *)
  module Unicode = struct
    module Mode = struct
      type t =
        | Off
        | Internal_small
        | Normal
        | Extended
    end

    let default_commit_chars =
      [ " "
      ; "("
      ; ")"
      ; ","
      ; "."
      ; "-"
      ; "'"
      ; "0"
      ; "1"
      ; "2"
      ; "3"
      ; "4"
      ; "5"
      ; "6"
      ; "7"
      ; "8"
      ; "9"
      ]

    type t =
      { enabled : Mode.t
      ; commit_chars : string list [@default default_commit_chars]
            (** Characters to use for accepting/commiting a completion during
                unicode completion *)
      }

    let default =
      let enabled = Mode.Normal in
      let commit_chars = default_commit_chars in
      { enabled; commit_chars }
  end

  type t = { unicode : Unicode.t }

  let default =
    let unicode = Unicode.default in
    { unicode }
end

type t =
  { mem_stats : bool [@default false]
        (** [mem_stats] Call [Obj.reachable_words] for every sentence. This is
            very slow and not very useful, so disabled by default *)
  ; gc_quick_stats : bool [@default true]
        (** [gc_quick_stats] Show the diff of [Gc.quick_stats] data for each
            sentence *)
  ; client_version : string [@default "any"]
  ; eager_diagnostics : bool [@default true]
        (** [eager_diagnostics] Send (full) diagnostics after processing each *)
  ; goal_after_tactic : bool [@default true]
        (** [proof/goals] setting: if [false], show goals before executing the
            tactic, if [true], show goals after. If there is no sentence at
            [position], show the goals of the closest sentence in the document
            backwards. *)
  ; messages_follow_goal : bool [@default false]
        (** [proof/goals] setting: if [true], messages and errors will be
            displayed following the [goal_after_tactic] setting. When [false],
            the default, we will always shows messages and errors corresponding
            to the sentence at [position]. If there is no sentence at
            [position], we show nothing. *)
  ; show_coq_info_messages : bool [@default false]
        (** Show `msg_info` messages in diagnostics *)
  ; show_notices_as_diagnostics : bool [@default false]
        (** Show `msg_notice` messages in diagnostics *)
  ; admit_on_bad_qed : bool [@default true]
        (** [admit_on_bad_qed] There are two possible error recovery strategies
            when a [Qed] fails: one is not to add the constant to the state, the
            other one is admit it. We find the second behavior more useful, but
            YMMV. *)
  ; debug : bool [@default false]
        (** Enable debug on Coq side, including backtraces *)
  ; unicode_completion : Completion.Unicode.Mode.t option [@default None]
        (** deprecated, use [completion.unicode.enabled] *)
  ; max_errors : int [@default 150]
  ; state_cache_size : int [@default 10_000]
        (** How many Rocq states no one holds a claim on the state store may
            keep as a cache; past this the least recently used are dropped.
            Client handles that were not freed, the most recently used open
            documents, and root states are claims, so they are never dropped and
            do not count. *)
  ; pp_type : int [@default 0]
        (** Pretty-printing type in Info Panel Request, 0 = string; 1 = Pp.t; 2
            = Coq Layout Engine *)
  ; show_stats_on_hover : bool [@default false]  (** Show stats on hover *)
  ; show_loc_info_on_hover : bool [@default false]
        (** Show loc info on hover *)
  ; show_universes_on_hover : bool [@default false]
        (** Show universe data on hover *)
  ; show_state_hash_on_hover : bool [@default false]
        (** Show state hash on hover, useful for debugging *)
  ; show_comments_on_hover : bool [@default false]
        (** Show comments on hover, useful for debugging *)
  ; show_doc_on_hover : bool [@default false]
        (** Show documentation on hover *)
  ; show_pr_vernac_on_hover : bool [@default false]
        (** Print sentence on hover using Rocq's, useful for debugging *)
  ; pp_json : bool [@default false]
        (** Whether to pretty print the protocol JSON on the wire *)
  ; send_perf_data : bool [@default true]
        (** Whether to send the perf data notification *)
  ; send_diags : bool [@default true]
        (** Whether to send the diagnostics notification *)
  ; verbosity : int [@default 2]
        (** Verbosity, 1 = reduced, 2 = default. As of today reduced will
            disable all logging, and the diagnostics and perf_data notification *)
  ; check_only_on_request : bool [@default false]
        (** Experimental setting to check document lazily *)
  ; send_diags_extra_data : bool [@default false]
        (** Send extra diagnostic data on the `data` diagnostic field. *)
  ; send_serverStatus : bool [@default true]
        (** Send server status client notification to the client *)
  ; send_execinfo : bool [@default false]
        (** Send execution information to client *)
  ; completion : Completion.t [@default Completion.default]
  }

let default =
  { mem_stats = false
  ; gc_quick_stats = true
  ; client_version = "any"
  ; eager_diagnostics = true
  ; goal_after_tactic = true
  ; messages_follow_goal = false
  ; show_coq_info_messages = false
  ; show_notices_as_diagnostics = false
  ; admit_on_bad_qed = true
  ; debug = false
  ; unicode_completion = None
  ; max_errors = 150
  ; state_cache_size = 10_000
  ; pp_type = 0
  ; show_stats_on_hover = false
  ; show_loc_info_on_hover = false
  ; show_universes_on_hover = false
  ; show_state_hash_on_hover = false
  ; show_comments_on_hover = false
  ; show_doc_on_hover = false
  ; show_pr_vernac_on_hover = false
  ; verbosity = 2
  ; pp_json = false
  ; send_perf_data = true
  ; send_diags = true
  ; check_only_on_request = false
  ; send_diags_extra_data = false
  ; send_serverStatus = true
  ; send_execinfo = false
  ; completion = Completion.default
  }

let v = ref default
