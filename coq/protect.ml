(*************************************************************************)
(* Copyright 2015-2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2019-2024 Inria           -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2024-2025 Emilio J. Gallego Arias  -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                     -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors           *)
(*************************************************************************)
(* Rocq Language Server Protocol: Rocq Effect Handling                   *)
(*************************************************************************)

module Error = struct
  type 'l t =
    | User of 'l Message.Payload.t
    | Anomaly of 'l Message.Payload.t

  let map ~f = function
    | User e -> User (f e)
    | Anomaly e -> Anomaly (f e)
end

module R = struct
  type ('a, 'l) t =
    | Completed of ('a, 'l Error.t) result
    | Interrupted (* signal sent, eval didn't complete *)

  let error msg =
    let payload = Message.Payload.make msg in
    Completed (Error (Error.User payload))

  let map ~f = function
    | Completed (Result.Ok r) -> Completed (Result.Ok (f r))
    | Completed (Result.Error r) -> Completed (Result.Error r)
    | Interrupted -> Interrupted

  let map_error ~f = function
    | Completed (Error e) -> Completed (Error (Error.map ~f e))
    | Completed (Ok r) -> Completed (Ok r)
    | Interrupted -> Interrupted

  (* Similar to Message.map, but missing the priority field, this is due to Coq
     having to sources of feedback, an async one, and the exn sync one.
     Ultimately both carry the same [payload].

     See coq/coq#5479 for some information about this, among some other relevant
     issues. AFAICT, the STM tried to use a full async error reporting however
     due to problems the more "legacy" exn is the actuall error mechanism in
     use *)
  let map_loc ~f =
    let f = Message.Payload.map ~f in
    map_error ~f
end

let qf_of_coq qf =
  let range = Quickfix.loc qf in
  let newText = Quickfix.pp qf |> Pp.string_of_ppcmds in
  { Lang.Qf.range; newText }

(* Eval and reify exceptions *)
let eval_exn ~token ~f x =
  match Limits.limit ~token ~f x with
  | Ok res -> R.Completed (Ok res)
  | Error _ ->
    R.Interrupted
  | exception exn ->
    let e, info = Exninfo.capture exn in
    let range = Loc.(get_loc info) in
    let msg = CErrors.iprint (e, info) in
    let quickFix =
      match Quickfix.from_exception exn with
      | Ok [] | Error _ -> None
      | Ok qf -> Some (List.map qf_of_coq qf)
    in
    let payload = Message.Payload.make ?range ?quickFix msg in
    if CErrors.is_async e || CErrors.is_sync_anomaly e then
      R.Completed (Error (Anomaly payload))
    else R.Completed (Error (User payload))

let _bind_exn ~f x =
  match x with
  | R.Interrupted -> R.Interrupted
  | R.Completed (Error e) -> R.Completed (Error e)
  | R.Completed (Ok r) -> f r

let fb_queue : Loc.t Message.t list ref = ref []

module E = struct
  type ('a, 'l) t =
    { r : ('a, 'l) R.t
    ; feedback : 'l Message.t list
    }

  let eval ~token ~f x =
    let r = eval_exn ~token ~f x in
    let feedback = List.rev !fb_queue in
    let () = fb_queue := [] in
    { r; feedback }

  let map ~f { r; feedback } = { r = R.map ~f r; feedback }

  let map_loc ~f { r; feedback } =
    { r = R.map_loc ~f r; feedback = List.map (Message.map ~f) feedback }

  let bind ~f { r; feedback } =
    match r with
    | R.Interrupted -> { r = R.Interrupted; feedback }
    | R.Completed (Error e) -> { r = R.Completed (Error e); feedback }
    | R.Completed (Ok r) ->
      let { r; feedback = fb2 } = f r in
      { r; feedback = feedback @ fb2 }

  let ok v = { r = Completed (Ok v); feedback = [] }
  let error err = { r = R.error err; feedback = [] }

  module O = struct
    let ( let+ ) x f = map ~f x
    let ( let* ) x f = bind ~f x
  end

  let rec mapM ~f il =
    let open O in
    match il with
    | [] -> ok []
    | x :: xl ->
      let* r = f x in
      let* rl = mapM ~f xl in
      ok (r :: rl)
end

(* Eval with reified exceptions and feedback *)
let eval ~token ~f x = E.eval ~token ~f x
