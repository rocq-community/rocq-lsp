(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors          *)
(************************************************************************)
(* Flèche => RL agent: petanque                                         *)
(************************************************************************)

(* Serialization for agent types *)
module Lsp = Fleche_lsp

(* A handle is an id in the state store. Serialising a state is what hands it to
   a client, so that is where the client's claim on it starts; the claim ends
   when the client frees the handle, or when the process does. *)
module State = struct
  module S = Fleche.States

  type t = Petanque.Agent.State.t
  type _t = int [@@deriving yojson]

  let register st =
    let id = S.register (Petanque.Agent.State.to_coq st) in
    S.retain id S.Client;
    id

  let not_found raw =
    Error
      (Format.asprintf "key %d for object %s not found" raw
         Petanque.Agent.State.name)

  let of_yojson json =
    match _t_of_yojson json with
    | Error _ as err -> err
    | Ok raw -> (
      match Option.bind (S.of_int raw) S.get with
      | None -> not_found raw
      | Some st -> Ok (Petanque.Agent.State.of_coq st))

  let to_yojson st = register st |> S.to_int |> _t_to_yojson

  (* [free ids] gives up the client's claim on [ids]. The states stay cached
     until the store's bound ages them out, so a freed handle that is soon
     re-derived is still a cache hit. Unknown or already freed ids are ignored,
     repeating a free is harmless. [freed] counts the claims actually given up,
     not the ids named: an id a document owns but no handle named was never the
     client's to free, and counting it would make the number useless for
     spotting a leak. *)
  let free ids =
    let release freed raw =
      match S.of_int raw with
      | None -> freed
      | Some id -> if S.release id S.Client then freed + 1 else freed
    in
    let freed = List.fold_left release 0 ids in
    let { S.live; _ } = S.stats () in
    (freed, live)
end

module Inspect = struct
  type t = [%import: Petanque.Agent.State.Inspect.t] [@@deriving yojson]
end

(* The typical protocol dance *)
module Error = struct
  type t = [%import: Petanque.Agent.Error.t] [@@deriving yojson]
end

module Run_opts = struct
  type t = [%import: Petanque.Agent.Run_opts.t] [@@deriving yojson]
end

module Run_result = struct
  type 'a t = [%import: 'a Petanque.Agent.Run_result.t] [@@deriving yojson]
end

(* Both are needed as of today *)
module Stdlib = Lsp.JStdlib
module Result = Stdlib.Result

module Goal_opts = struct
  type t = [%import: Petanque.Agent.Goal_opts.t] [@@deriving yojson]
end

module Goals = struct
  type t = (string, string) Lsp.JCoq.Goals.reified option [@@deriving yojson]
end

module Ast = struct
  type t = Lsp.JCoq.Ast.t [@@deriving yojson]
end

module Lang = Lsp.JLang

module Premise = struct
  module Info = struct
    type t = [%import: Petanque.Agent.Premise.Info.t] [@@deriving yojson]
  end

  type t = [%import: Petanque.Agent.Premise.t] [@@deriving yojson]
end

module Proof_info = struct
  type t = [%import: Petanque.Agent.Proof_info.t] [@@deriving yojson]
end

module Notation_analysis = Fleche_lsp.JCoq.Notation_analysis
