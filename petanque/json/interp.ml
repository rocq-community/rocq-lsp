(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors          *)
(************************************************************************)
(* Flèche => RL agent: petanque                                         *)
(************************************************************************)

module Req = Request
open Protocol
module A = Petanque.Agent

(* Payload specialized to json / string *)
type json_rpc_result = (Yojson.Safe.t, string) Req.R.t

(* These types ares basically duplicated with controller/request.ml; move to a
   common lib (lsp?) *)
module Action = struct
  type t =
    | Now of (token:Coq.Limits.Token.t -> json_rpc_result)
    | Doc of
        { uri : Lang.LUri.File.t
        ; handler :
            token:Coq.Limits.Token.t -> doc:Fleche.Doc.t -> json_rpc_result
        }
    | Pos of
        { uri : Lang.LUri.File.t
        ; point : int * int
        ; handler :
               token:Coq.Limits.Token.t
            -> doc:Fleche.Doc.t
            -> point:int * int
            -> json_rpc_result
        }
end
(* End of controller/request.ml *)

let of_pet_err e = Req.R.map_error ~f:Petanque.Agent.Error.to_string e

(* Basically a functor from R.Handler.t to Action.t, but closing over params *)
let do_request (module R : Protocol.Request.S) ~params =
  let of_pet res = Result.map R.Handler.Response.to_yojson res |> of_pet_err in
  let handler params =
    match R.Handler.handler with
    | Immediate handler ->
      Action.Now (fun ~token -> handler ~token params |> of_pet)
    | FullDoc { uri_fn; handler } ->
      let uri = uri_fn params in
      let handler ~token ~doc = handler ~token ~doc params |> of_pet in
      Action.Doc { uri; handler }
    | PosInDoc { uri_fn; pos_fn; handler } ->
      let uri = uri_fn params in
      let point = pos_fn params in
      let handler ~token ~doc ~point =
        handler ~token ~doc ~point params |> of_pet
      in
      Action.Pos { uri; point; handler }
  in
  match R.Handler.Params.of_yojson (`Assoc params) with
  | Ok params -> handler params
  | Error message ->
    (* JSON-RPC Parse error *)
    let code = -32700 in
    Action.Now (fun ~token:_ -> Error (Req.Error.make code message))

type 'a handle = token:Coq.Limits.Token.t -> Action.t -> 'a

let handle_request ~(do_handle : 'a handle) ~unhandled ~token ~method_ ~params =
  match method_ with
  | s when String.equal GetStateAtPos.method_ s ->
    do_handle ~token (do_request (module GetStateAtPos) ~params)
  | s when String.equal GetRootState.method_ s ->
    do_handle ~token (do_request (module GetRootState) ~params)
  | s when String.equal Start.method_ s ->
    do_handle ~token (do_request (module Start) ~params)
  | s when String.equal RunTac.method_ s ->
    do_handle ~token (do_request (module RunTac) ~params)
  | s when String.equal RunAtPoint.method_ s ->
    do_handle ~token (do_request (module RunAtPoint) ~params)
  | s when String.equal Goals.method_ s ->
    do_handle ~token (do_request (module Goals) ~params)
  | s when String.equal Premises.method_ s ->
    do_handle ~token (do_request (module Premises) ~params)
  | s when String.equal StateEqual.method_ s ->
    do_handle ~token (do_request (module StateEqual) ~params)
  | s when String.equal StateHash.method_ s ->
    do_handle ~token (do_request (module StateHash) ~params)
  | s when String.equal StateProofEqual.method_ s ->
    do_handle ~token (do_request (module StateProofEqual) ~params)
  | s when String.equal StateProofHash.method_ s ->
    do_handle ~token (do_request (module StateProofHash) ~params)
  | s when String.equal StateFree.method_ s ->
    do_handle ~token (do_request (module StateFree) ~params)
  | s when String.equal StateStats.method_ s ->
    do_handle ~token (do_request (module StateStats) ~params)
  | s when String.equal CacheTrim.method_ s ->
    do_handle ~token (do_request (module CacheTrim) ~params)
  | s when String.equal ListNotations.method_ s ->
    do_handle ~token (do_request (module ListNotations) ~params)
  | s when String.equal PetAst.method_ s ->
    do_handle ~token (do_request (module PetAst) ~params)
  | s when String.equal AstAtPos.method_ s ->
    do_handle ~token (do_request (module AstAtPos) ~params)
  | s when String.equal ProofInfo.method_ s ->
    do_handle ~token (do_request (module ProofInfo) ~params)
  | s when String.equal ProofInfoAtPos.method_ s ->
    do_handle ~token (do_request (module ProofInfoAtPos) ~params)
  | _ -> unhandled ~token ~method_
