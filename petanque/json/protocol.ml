(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors          *)
(************************************************************************)
(* Flèche => RL agent: petanque                                         *)
(************************************************************************)

open Lang
open Petanque

(* Serialization for agent types *)
module Lsp = Fleche_lsp
open JAgent

(* RPC-side server mappings, internal; we could split this in a different module
   eventually as to make this clearer. *)
module HType = struct
  type ('p, 'r) t =
    | Immediate of (token:Coq.Limits.Token.t -> 'p -> ('r, Error.t) Request.R.t)
    | FullDoc of
        { uri_fn : 'p -> LUri.File.t
        ; handler :
               token:Coq.Limits.Token.t
            -> doc:Fleche.Doc.t
            -> 'p
            -> ('r, Error.t) Request.R.t
        }
    | PosInDoc of
        { uri_fn : 'p -> LUri.File.t
        ; pos_fn : 'p -> int * int
        ; handler :
               token:Coq.Limits.Token.t
            -> doc:Fleche.Doc.t
            -> point:int * int
            -> 'p
            -> ('r, Error.t) Request.R.t
        }
end

module type Handler = sig
  (* Server-side RPC specification *)
  module Params : sig
    type t [@@deriving of_yojson]
  end

  (* Server-side RPC specification *)
  module Response : sig
    type t [@@deriving to_yojson]
  end

  val handler : (Params.t, Response.t) HType.t
end

(* Note that here we follow JSON-RPC / LSP capitalization conventions *)
module Request = struct
  module type S = sig
    val method_ : string

    (* Protocol params specification *)
    module Params : sig
      type t [@@deriving yojson]
    end

    (* Protocol response specification *)
    module Response : sig
      type t [@@deriving yojson]
    end

    module Handler : Handler
  end
end

(* get_root_state RPC *)
module GetRootState = struct
  let method_ = "petanque/get_root_state"

  module Params = struct
    type t =
      { opts : Run_opts.t option [@default None]
      ; uri : Lsp.JLang.LUri.File.t
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = int Run_result.t [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params

    module Response = struct
      type t = State.t Run_result.t [@@deriving yojson]
    end

    let handler =
      HType.FullDoc
        { uri_fn = (fun { Params.uri; _ } -> uri)
        ; handler =
            (fun ~token:_ ~doc { opts; uri = _ } ->
              Agent.get_root_state ?opts ~doc ())
        }
  end
end

(* get_state_at_pos RPC *)
module GetStateAtPos = struct
  let method_ = "petanque/get_state_at_pos"

  module Params = struct
    type t =
      { uri : Lsp.JLang.LUri.File.t
      ; opts : Run_opts.t option [@default None]
      ; position : Lsp.JLang.Point.t
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = int Run_result.t [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params

    module Response = struct
      type t = State.t Run_result.t [@@deriving yojson]
    end

    let handler =
      HType.PosInDoc
        { uri_fn = (fun { Params.uri; _ } -> uri)
        ; pos_fn = (fun { position; _ } -> (position.line, position.character))
        ; handler =
            (fun ~token:_ ~doc ~point { uri = _; opts; position = _ } ->
              Agent.get_state_at_pos ?opts ~doc ~point ())
        }
  end
end

(* start RPC *)
module Start = struct
  let method_ = "petanque/start"

  module Params = struct
    type t =
      { uri : Lsp.JLang.LUri.File.t
      ; opts : Run_opts.t option [@default None]
      ; pre_commands : string option [@default None]
      ; thm : string
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = int Run_result.t [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params

    module Response = struct
      type t = State.t Run_result.t [@@deriving yojson]
    end

    let handler =
      HType.FullDoc
        { uri_fn = (fun { Params.uri; _ } -> uri)
        ; handler =
            (fun ~token ~doc { Params.uri = _; opts; pre_commands; thm } ->
              Agent.start ~token ~doc ?opts ?pre_commands ~thm ())
        }
  end
end

(* run_tac RPC *)
module RunTac = struct
  let method_ = "petanque/run"

  module Params = struct
    type t =
      { opts : Run_opts.t option [@default None]
      ; st : int
      ; tac : string
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = int Run_result.t [@@deriving yojson]
  end

  module Handler = struct
    module Params = struct
      type t =
        { opts : Run_opts.t option
              [@default None] (* Whether to memoize the execution *)
        ; st : State.t
        ; tac : string
        }
      [@@deriving yojson]
    end

    module Response = struct
      type t = State.t Run_result.t [@@deriving yojson]
    end

    let handler =
      HType.Immediate
        (fun ~token { Params.opts; st; tac } ->
          Agent.run ~token ?opts ~st ~tac ())
  end
end

(* run_tac RPC *)
module RunAtPoint = struct
  let method_ = "petanque/run_at_point"

  module Params = struct
    type t =
      { textDocument : Lsp.Doc.VersionedTextDocumentIdentifier.t
      ; position : Lsp.JLang.Point.t
      ; command : string
      ; opts : Run_opts.t option [@default None]
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = unit Run_result.t [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      HType.PosInDoc
        { uri_fn = (fun { Params.textDocument; _ } -> textDocument.uri)
        ; pos_fn = (fun { position; _ } -> (position.line, position.character))
        ; handler =
            (fun ~token ~doc ~point
                 { textDocument = _; position = _; command; opts } ->
              Agent.run_at_pos ~token ?opts ~doc ~point ~command ())
        }
  end
end

(* goals RPC *)
module Goals = struct
  let method_ = "petanque/goals"

  module Params = struct
    type t =
      { st : int
      ; opts : Goal_opts.t option [@default None]
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = Goals.t [@@deriving yojson]
  end

  module Handler = struct
    module Params = struct
      type t =
        { st : State.t
        ; opts : Goal_opts.t option [@default None]
        }
      [@@deriving yojson]
    end

    module Response = Response

    let handler =
      HType.Immediate
        (fun ~token { Params.st; opts } -> Agent.goals ~token ?opts ~st ())
  end
end

(* premises RPC *)
module Premises = struct
  let method_ = "petanque/premises"

  module Params = struct
    type t = { st : int } [@@deriving yojson]
  end

  module Response = struct
    type t = Premise.t list [@@deriving yojson]
  end

  module Handler = struct
    module Params = struct
      type t = { st : State.t } [@@deriving yojson]
    end

    module Response = Response

    let handler =
      HType.Immediate (fun ~token { Params.st } -> Agent.premises ~token ~st)
  end
end

(* StateEqual *)
module StateEqual = struct
  let method_ = "petanque/state/eq"

  module Params = struct
    type t =
      { kind : Inspect.t option [@default None]
      ; st1 : int
      ; st2 : int
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = bool [@@deriving yojson]
  end

  module Handler = struct
    module Params = struct
      type t =
        { kind : Inspect.t option [@default None]
        ; st1 : State.t
        ; st2 : State.t
        }
      [@@deriving yojson]
    end

    module Response = Response

    let handler =
      HType.Immediate
        (fun ~token:_ { Params.kind; st1; st2 } ->
          Ok (Agent.State.equal ?kind st1 st2))
  end
end

module StateHash = struct
  let method_ = "petanque/state/hash"

  module Params = struct
    type t = { st : int } [@@deriving yojson]
  end

  module Response = struct
    type t = int [@@deriving yojson]
  end

  module Handler = struct
    module Params = struct
      type t = { st : State.t } [@@deriving yojson]
    end

    module Response = Response

    let handler =
      HType.Immediate (fun ~token:_ { Params.st } -> Ok (Agent.State.hash st))
  end
end

module StateProofEqual = struct
  let method_ = "petanque/state/proof/equal"

  module Params = StateEqual.Params
  module Response = StateEqual.Response

  module Handler = struct
    module Params = StateEqual.Handler.Params
    module Response = Response

    let handler =
      HType.Immediate
        (fun ~token:_ { Params.kind; st1; st2 } ->
          let pst1, pst2 = Agent.State.(lemmas st1, lemmas st2) in
          Ok (Option.equal (Agent.State.Proof.equal ?kind) pst1 pst2))
  end
end

module StateProofHash = struct
  let method_ = "petanque/state/proof/hash"

  module Params = StateHash.Params

  module Response = struct
    type t = int option [@@deriving yojson]
  end

  module Handler = struct
    module Params = StateHash.Handler.Params
    module Response = Response

    let handler =
      HType.Immediate
        (fun ~token:_ { Params.st } ->
          let pst = Agent.State.lemmas st in
          Ok (Option.map Agent.State.Proof.hash pst))
  end
end

(* Store statistics, shared by the routes below *)
module StoreStats = struct
  type t =
    { live : int
    ; roots : int
    ; docs : int
    ; clients : int
    ; unowned : int
    }
  [@@deriving yojson]

  let of_states { Fleche.States.live; roots; docs; clients; unowned } =
    { live; roots; docs; clients; unowned }
end

(* Release the client's claim on some handles. Unknown or already-freed ids are
   ignored, so a client may repeat a free without consequence. *)
module StateFree = struct
  let method_ = "petanque/state/free"

  module Params = struct
    (* Deliberately raw ints: a [State.t] here would fail to deserialise on an
       id that was already freed, taking the whole batch with it. *)
    type t = { st : int list } [@@deriving yojson]
  end

  module Response = struct
    type t =
      { freed : int
      ; live : int
      }
    [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      HType.Immediate
        (fun ~token:_ { Params.st } ->
          let freed, live = JAgent.State.free st in
          Ok { Response.freed; live })
  end
end

(* What the state store is holding, and for whom. *)
module StateStats = struct
  let method_ = "petanque/state/stats"

  module Params = struct
    type t = { gc : bool [@default false] } [@@deriving yojson]
  end

  module Response = StoreStats

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      HType.Immediate
        (fun ~token:_ { Params.gc } ->
          let stats =
            if gc then Fleche.States.gc () else Fleche.States.stats ()
          in
          Ok (StoreStats.of_states stats))
  end
end

(* Drop every cache. Blunt: the next request re-checks from scratch. Documents
   keep the states they are made of. *)
module CacheTrim = struct
  let method_ = "petanque/cache/trim"

  module Params = struct
    type t = { full_major : bool [@default false] } [@@deriving yojson]
  end

  module Response = StoreStats

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      HType.Immediate
        (fun ~token:_ { Params.full_major } ->
          let stats = Fleche.Memo.clear_all () in
          if full_major then Gc.full_major ();
          Ok (StoreStats.of_states stats))
  end
end

module ListNotations = struct
  let method_ = "petanque/list_notations_in_statement"

  module Params = struct
    type t =
      { st : int
      ; statement : string
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = Notation_analysis.Info.t list Run_result.t [@@deriving yojson]
  end

  module Handler = struct
    module Params = struct
      type t =
        { st : State.t
        ; statement : string
        }
      [@@deriving yojson]
    end

    module Response = Response

    let handler =
      HType.Immediate
        (fun ~token { Params.st; statement } ->
          Agent.list_notations_in_statement ~token ~st ~statement ())
  end
end

module PetAst = struct
  let method_ = "petanque/ast"

  module Params = struct
    type t =
      { st : int
      ; text : string
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = Ast.t option Run_result.t [@@deriving yojson]
  end

  module Handler = struct
    module Params = struct
      type t =
        { st : State.t
        ; text : string
        }
      [@@deriving yojson]
    end

    module Response = struct
      type t = Ast.t option Run_result.t [@@deriving yojson]
    end

    let handler =
      HType.Immediate
        (fun ~token { Params.st; text } -> Agent.ast ~token ~st ~text ())
  end
end

(* ast_at_pos RPC *)
module AstAtPos = struct
  let method_ = "petanque/ast_at_pos"

  module Params = struct
    type t =
      { uri : Lsp.JLang.LUri.File.t
      ; position : Lsp.JLang.Point.t
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = Ast.t option [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      HType.PosInDoc
        { uri_fn = (fun { Params.uri; _ } -> uri)
        ; pos_fn = (fun { position; _ } -> (position.line, position.character))
        ; handler =
            (fun ~token:_ ~doc ~point { uri = _; position = _ } ->
              Agent.ast_at_pos ~doc ~point ())
        }
  end
end

module ProofInfo = struct
  let method_ = "petanque/proof_info"

  module Params = struct
    type t = { st : int } [@@deriving yojson]
  end

  module Response = struct
    type t = JAgent.Proof_info.t option [@@deriving yojson]
  end

  module Handler = struct
    module Params = struct
      type t = { st : State.t } [@@deriving yojson]
    end

    module Response = Response

    let handler =
      HType.Immediate
        (fun ~token { Params.st } -> Agent.proof_info ~token ~st ())
  end
end

module ProofInfoAtPos = struct
  let method_ = "petanque/proof_info_at_pos"

  module Params = struct
    type t =
      { textDocument : Lsp.Doc.VersionedTextDocumentIdentifier.t
      ; position : Lsp.JLang.Point.t
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = JAgent.Proof_info.t option [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      HType.PosInDoc
        { uri_fn = (fun { Params.textDocument; _ } -> textDocument.uri)
        ; pos_fn = (fun { position; _ } -> (position.line, position.character))
        ; handler =
            (fun ~token ~doc ~point { textDocument = _; position = _ } ->
              Agent.proof_info_at_pos ~token ~doc ~point ())
        }
  end
end
