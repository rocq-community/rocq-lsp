import { TextDocumentFilter } from "vscode-languageclient";
import { CoqLspClientConfig } from "../lib/types";

export interface UnicodeCompletionConfig {
  enabled: "off" | "normal" | "extended";
  commit_chars: string[];
}

export interface CompletionConfig {
  unicode: UnicodeCompletionConfig;
}

export interface CoqLspServerConfig {
  client_version: string;
  eager_diagnostics: boolean;
  goal_after_tactic: boolean;
  messages_follow_goal: boolean;
  show_coq_info_messages: boolean;
  show_notices_as_diagnostics: boolean;
  admit_on_bad_qed: boolean;
  debug: boolean;
  max_errors: number;
  pp_type: 0 | 1 | 2;
  show_stats_on_hover: boolean;
  show_loc_info_on_hover: boolean;
  show_universes_on_hover: boolean;
  show_state_hash_on_hover: boolean;
  show_comments_on_hover: boolean;
  show_pr_vernac_on_hover: boolean;
  check_only_on_request: boolean;
  send_perf_data: boolean;
  send_execinfo: boolean;
  state_cache_size: number;
  completion: CompletionConfig;
}

export namespace CoqLspServerConfig {
  export function create(
    client_version: string,
    wsConfig: any
  ): CoqLspServerConfig {
    return {
      client_version: client_version,
      eager_diagnostics: wsConfig.eager_diagnostics,
      goal_after_tactic: wsConfig.goal_after_tactic,
      messages_follow_goal: wsConfig.messages_follow_goal,
      show_coq_info_messages: wsConfig.show_coq_info_messages,
      show_notices_as_diagnostics: wsConfig.show_notices_as_diagnostics,
      admit_on_bad_qed: wsConfig.admit_on_bad_qed,
      debug: wsConfig.debug,
      max_errors: wsConfig.max_errors,
      state_cache_size: wsConfig.state_cache_size,
      pp_type: wsConfig.pp_type,
      show_stats_on_hover: wsConfig.show_stats_on_hover,
      show_loc_info_on_hover: wsConfig.show_loc_info_on_hover,
      show_universes_on_hover: wsConfig.show_universes_on_hover,
      show_state_hash_on_hover: wsConfig.show_state_hash_on_hover,
      show_comments_on_hover: wsConfig.show_comments_on_hover,
      show_pr_vernac_on_hover: wsConfig.show_pr_vernac_on_hover,
      check_only_on_request: wsConfig.check_only_on_request,
      send_perf_data: wsConfig.send_perf_data,
      send_execinfo: wsConfig.send_execinfo,
      // VSCode wraps wsConfig.completion into a Proxy, which cannot
      // be sent to a Web Worker, tricky stuff...
      completion: JSON.parse(JSON.stringify(wsConfig.completion)),
    };
  }
}

function pp_type_to_pp_format(pp_type: 0 | 1 | 2): "Str" | "Pp" | "Box" {
  switch (pp_type) {
    case 0:
      return "Str";
    case 1:
      return "Pp";
    case 2:
      return "Box";
  }
}

export namespace ClientConfig {
  export function create(wsConfig: any): CoqLspClientConfig {
    let obj: CoqLspClientConfig = {
      show_goals_on: wsConfig.show_goals_on,
      pp_format: pp_type_to_pp_format(wsConfig.pp_type),
      check_on_scroll: wsConfig.check_on_scroll,
      compact_hypotheses: wsConfig.compact_hypotheses,
      messages_limit: wsConfig.messages_limit,
    };
    return obj;
  }
}

export namespace CoqSelector {
  // All Coq files, regardless of the scheme.
  export const all: TextDocumentFilter[] = [
    { language: "coq" },
    { language: "markdown", pattern: "**/*.mv" },
    { language: "latex", pattern: "**/*.lv" },
    { language: "latex", pattern: "**/*.v.tex" },
  ];

  // Local Coq files, suitable for interaction with a local server
  export const local: TextDocumentFilter[] = all.map((selector) => {
    return { ...selector, scheme: "file" };
  });

  // VSCode Live Share URIs
  export const vsls: TextDocumentFilter[] = all.map((selector) => {
    return { ...selector, scheme: "vsls" };
  });

  // Virtual workspaces such as github.dev
  export const virtual: TextDocumentFilter[] = all.map((selector) => {
    return { ...selector, scheme: "vscode-vfs" };
  });

  // Files that are owned by our server, local + virtual
  export const owned: TextDocumentFilter[] = local.concat(virtual);
}
