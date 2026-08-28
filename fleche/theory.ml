(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors           *)
(************************************************************************)
(* Flèche => document manager: theories                                 *)
(************************************************************************)

module IS = Lang.Compat.IntSet

(* Handler for document *)
module Handle = struct
  type t =
    { doc : Doc.t
    ; cp_requests : IS.t
          (* For now we just store the request id to wake up on completion,
             later on we may want to store a more interesting type, for example
             "wake up when a location is reached", or always to continue the
             streaming *)
    ; pt_requests : (int * (int * int)) list (* id, point *)
    }

  let pt_eq (id1, (l1, c1)) (id2, (l2, c2)) = id1 = id2 && l1 = l2 && c1 = c2

  let pt_gt x y =
    let lx, cx = x in
    let ly, cy = y in
    lx > ly || (lx = ly && cx > cy)

  let doc_table : (Lang.LUri.File.t, t) Hashtbl.t = Hashtbl.create 39

  let create ~uri ~doc =
    (match Hashtbl.find_opt doc_table uri with
    | None -> ()
    | Some _ ->
      Io.Log.trace "do_open" "file %a not properly closed by client"
        Lang.LUri.File.pp uri);
    Hashtbl.add doc_table uri { doc; cp_requests = IS.empty; pt_requests = [] }

  let close ~uri =
    Doc_own.remove uri;
    Hashtbl.remove doc_table uri

  let with_doc ~kind ~f ~uri ~default =
    match Hashtbl.find_opt doc_table uri with
    | None ->
      Io.Log.trace kind "document %a not available" Lang.LUri.File.pp uri;
      default ()
    | Some handle -> f handle handle.doc

  let _find_opt ~uri = Hashtbl.find_opt doc_table uri

  let _update_doc ~handle ~(doc : Doc.t) =
    Hashtbl.replace doc_table doc.uri { handle with doc }

  let pt_ids l = List.map (fun (id, _) -> id) l |> IS.of_list

  (* Clear requests *)
  let clear_requests ~uri =
    match Hashtbl.find_opt doc_table uri with
    | None -> IS.empty
    | Some { cp_requests; pt_requests; doc } ->
      let invalid_reqs = IS.union cp_requests (pt_ids pt_requests) in
      Hashtbl.replace doc_table uri
        { doc; cp_requests = IS.empty; pt_requests = [] };
      invalid_reqs

  (* Clear requests and update doc *)
  let update_doc_version ~(doc : Doc.t) =
    let invalid_reqs = clear_requests ~uri:doc.uri in
    Hashtbl.replace doc_table doc.uri
      { doc; cp_requests = IS.empty; pt_requests = [] };
    invalid_reqs

  let map_cp_requests ~uri ~f =
    match Hashtbl.find_opt doc_table uri with
    | Some { doc; cp_requests; pt_requests } ->
      let cp_requests = f cp_requests in
      Hashtbl.replace doc_table uri { doc; cp_requests; pt_requests }
    | None -> ()

  let attach_cp_request ~uri ~id =
    let f cp_requests = IS.add id cp_requests in
    map_cp_requests ~uri ~f

  let remove_cp_request ~uri ~id =
    let f cp_requests = IS.remove id cp_requests in
    map_cp_requests ~uri ~f

  let map_pt_requests ~uri ~f =
    match Hashtbl.find_opt doc_table uri with
    | Some { doc; cp_requests; pt_requests } ->
      let pt_requests = f pt_requests in
      Hashtbl.replace doc_table uri { doc; cp_requests; pt_requests }
    | None -> ()

  (* This needs to be insertion sort! *)
  let pt_insert x xs = Lang.Compat.List.insert pt_gt x xs

  let attach_pt_request ~uri ~id ~point =
    let f pt_requests = pt_insert (id, point) pt_requests in
    map_pt_requests ~uri ~f

  let remove_pt_request ~uri ~id ~point =
    let f pt_requests = Lang.Compat.List.remove pt_eq (id, point) pt_requests in
    map_pt_requests ~uri ~f

  (* For now only on completion, I think we want check to return the list of
     requests that can be served / woken up *)
  let do_requests ~doc ~handle =
    let handle = { handle with doc } in
    match doc.completed with
    | Yes _ ->
      let pt_ids = pt_ids handle.pt_requests in
      let wake_up = IS.union pt_ids handle.cp_requests in
      let pt_requests = [] in
      let cp_requests = IS.empty in
      ({ handle with cp_requests; pt_requests }, wake_up)
    | Stopped range ->
      let fullfilled, delayed =
        List.partition
          (fun (_id, point) -> Doc.Target.reached ~range point)
          handle.pt_requests
      in
      let handle = { handle with pt_requests = delayed } in
      (handle, pt_ids fullfilled)
    | Failed _ -> (handle, IS.empty)
    | WorkspaceUpdated _ -> (handle, IS.empty)

  (* trigger pending incremental requests *)
  let update_doc_info ~handle ~(doc : Doc.t) =
    let handle, requests = do_requests ~doc ~handle in
    Hashtbl.replace doc_table doc.uri handle;
    requests

  let workspace_update () =
    let invalid_req = ref IS.empty in
    let update_doc_env _uri (h : t) =
      let { doc; cp_requests; pt_requests } = h in
      invalid_req := IS.union !invalid_req cp_requests;
      invalid_req :=
        IS.union !invalid_req (List.map fst pt_requests |> IS.of_list);
      let { Doc.Env.init; workspace; files } = doc.env in
      let files = Coq.Files.bump files in
      let env = Doc.Env.make ~init ~workspace ~files in
      Some
        { doc = Doc.update_env ~doc ~env
        ; cp_requests = IS.empty
        ; pt_requests = []
        }
    in
    Hashtbl.filter_map_inplace update_doc_env doc_table;
    (* We forget the old .vo files in the .vo cache too *)
    Memo.Intern.clear ();
    (* The bump above retires the [Require] entries of every document, but not
       those of a caller that has no document to bump: pétanque's [run] keys on
       the generation it started with, so a stale entry would survive there. *)
    Memo.Require.clear ();
    !invalid_req
end

(* This is temporary for 0.1.9 and our ER project, we need to reify this to a
   general structure *)
module Register : sig
  (** Run an action before a constructing the document root state. *)
  module InjectRequire : sig
    type t = io:Io.CallBack.t -> Coq.Workspace.Require.t list

    val add : t -> unit
    val fire : io:Io.CallBack.t -> Coq.Workspace.Require.t list
  end

  (** Run an action when a document has completed checking, attention, with or
      without errors. *)
  module Completed : sig
    type t = io:Io.CallBack.t -> token:Coq.Limits.Token.t -> doc:Doc.t -> unit

    val add : t -> unit
    val fire : io:Io.CallBack.t -> token:Coq.Limits.Token.t -> doc:Doc.t -> unit
  end
end = struct
  module InjectRequire = struct
    type t = io:Io.CallBack.t -> Coq.Workspace.Require.t list

    let callback : t list ref = ref []
    let add fn = callback := fn :: !callback
    let fire ~io = List.concat_map (fun f -> f ~io) !callback
  end

  module Completed = struct
    type t = io:Io.CallBack.t -> token:Coq.Limits.Token.t -> doc:Doc.t -> unit

    let callback : t list ref = ref []
    let add fn = callback := fn :: !callback

    let fire ~io ~token ~doc =
      (* TODO: Add a field to IO representing plugin context *)
      let io = io in
      List.iter (fun f -> f ~io ~token ~doc) !callback
  end
end

let send_diags ~io ~token:_ ~doc =
  let diags = Doc.diags doc in
  if List.length diags > 0 || !Config.v.send_diags then
    let uri, version = (doc.uri, doc.version) in
    Io.Report.diagnostics ~io ~uri ~version diags

let send_perf_data ~io ~token:_ ~(doc : Doc.t) =
  (* The if below needs to be moved to registrationt time, but for now we keep
     it for now until the plugin workflow is clearer *)
  if !Config.v.send_perf_data then
    let uri, version = (doc.uri, doc.version) in
    Io.Report.perfData ~io ~uri ~version (Perf_analysis.make doc)

let () = Register.Completed.add send_perf_data
let () = Register.Completed.add send_diags

module Reason = struct
  type t =
    | OpenDocument
    | ChangeDocument
    | ViewHint
    | RequestDoc of int
    | RequestPos of int * (int * int)

  let pp fmt = function
    | OpenDocument -> Format.fprintf fmt "%s" "didOpen"
    | ChangeDocument -> Format.fprintf fmt "%s" "didChange"
    | ViewHint -> Format.fprintf fmt "%s" "viewHint"
    | RequestDoc id -> Format.fprintf fmt "%s {id: %d}" "Request Doc" id
    | RequestPos (id, (l, c)) ->
      Format.fprintf fmt "%s {id: %d | l: %d c: %d}" "Request Pos" id l c
end

module Check : sig
  val schedule : uri:Lang.LUri.File.t -> reason:Reason.t -> unit
  val deschedule : uri:Lang.LUri.File.t -> unit

  val maybe_check :
    io:Io.CallBack.t -> token:Coq.Limits.Token.t -> (IS.t * Doc.t) option

  val set_scheduler_hint : uri:Lang.LUri.File.t -> point:int * int -> unit
  val clear_schedule : unit -> unit
end = struct
  let pending = ref []

  let pend_pop = function
    | [] -> []
    | _ :: p -> p

  (** Push to front; beware of complexity here? Usually we don't have more than
      10-20 files open in this use case; other use cases may require a more
      efficient data-structure. *)
  let clean_uri uri pend =
    List.filter (fun u -> not (Lang.LUri.File.equal uri u)) pend

  let pend_push uri pend = uri :: clean_uri uri pend

  (** Try until [Some] *)
  let rec pend_try f = function
    | [] -> None
    | l :: tt -> (
      match f l with
      | None -> pend_try f tt
      | Some r -> Some r)

  let hint : (Lang.LUri.File.t * (int * int)) option ref = ref None

  let get_check_target ~(doc : Doc.t) pt_requests =
    let target_of_pt_handle (_, (l, c)) = Doc.Target.Position (l, c) in
    match Option.map target_of_pt_handle (List.nth_opt pt_requests 0) with
    | None ->
      Option.bind !hint (fun (uri, (l, c)) ->
          if Lang.LUri.File.equal uri doc.uri then
            match doc.completed with
            | Yes _ | Failed _ | WorkspaceUpdated _ -> None
            | Stopped range when Doc.Target.reached ~range (l, c) -> None
            | Stopped _ -> Some (Doc.Target.Position (l, c))
          else None)
    | Some t -> Some t

  let report_start ~io doc =
    let uri = doc.Doc.uri in
    let uri_short = Lang.LUri.File.to_string_file uri |> Filename.basename in
    Io.Report.serverStatus ~io (ServerInfo.Status.Running uri_short)

  let report_idle ~io =
    let mem =
      Format.asprintf "%a" Stats.pp_words
        (Gc.((quick_stat ()).heap_words) |> Float.of_int)
    in
    Io.Report.serverStatus ~io (ServerInfo.Status.Idle mem)

  (* IMPORTANT: We only remove documents from the queue when they either:

     a) reach completed status, done in [do_check] b) there are no more work
     present and we are in lazy mode (done below in [check])

     This strategy has some downfalls, as we don't really separate idle work
     from non-idle work here, for example. *)
  let do_check ~io ~token ~handle ~doc target =
    let () = report_start ~io doc in
    let doc = Doc.check ~io ~token ~target ~doc () in
    let () = report_idle ~io in
    let requests = Handle.update_doc_info ~handle ~doc in
    if Doc.Completion.is_completed doc.completed then (
      Register.Completed.fire ~io ~token ~doc;
      pending := pend_pop !pending);
    (requests, doc)

  (* Notification handling; reply is optional / asynchronous *)
  let check ~io ~token ~uri =
    Io.Log.trace "maybe_check" "considering document %a" Lang.LUri.File.pp uri;
    let kind = "Check.check" in
    let default () =
      pending := pend_pop !pending;
      None
    in
    let f (handle : Handle.t) doc =
      (* See if we have a fine-grain target to go *)
      let target = get_check_target ~doc handle.pt_requests in
      match target with
      | None
      (* If we are in lazy mode and we don't have any full document requests
         pending, we just deschedule *)
        when !Config.v.check_only_on_request && IS.is_empty handle.cp_requests
        ->
        Io.Log.trace "maybe_check" "nothing to do, descheduling";
        pending := pend_pop !pending;
        None
      | (None | Some _) as tgt ->
        let target = Stdlib.Option.value ~default:Doc.Target.End tgt in
        Io.Log.trace "maybe_check" "building target %a" Doc.Target.pp target;
        Some (do_check ~io ~token ~handle ~doc target)
    in
    Handle.with_doc ~kind ~uri ~default ~f

  let maybe_check ~io ~token =
    pend_try (fun uri -> check ~io ~token ~uri) !pending

  let schedule ~uri ~reason =
    if Debug.schedule then Io.Log.trace "schedule" "reason: %a" Reason.pp reason;
    pending := pend_push uri !pending

  let deschedule ~uri = pending := clean_uri uri !pending

  let set_scheduler_hint ~uri ~point =
    if Lang.Compat.List.is_empty !pending then
      let () = hint := Some (uri, point) in
      let reason = Reason.ViewHint in
      schedule ~uri ~reason (* if the hint is set we wanna override it *)
    else if not (Stdlib.Option.is_none !hint) then hint := Some (uri, point)

  (* Clear the schedule queue, for a workspace update *)
  let clear_schedule () =
    pending := [];
    hint := None
end

let open_ ~io ~token ~env ~uri ~languageId ~raw ~version =
  let extra_requires = Register.InjectRequire.fire ~io in
  let env = Doc.Env.inject_requires ~extra_requires env in
  let doc = Doc.create ~token ~env ~uri ~languageId ~raw ~version in
  Handle.create ~uri ~doc;
  let reason = Reason.OpenDocument in
  Check.schedule ~uri ~reason

let change ~io:_ ~token ~(doc : Doc.t) ~version ~raw =
  let uri = doc.uri in
  Io.Log.trace "bump file" "%a / version: %d" Lang.LUri.File.pp uri version;
  let tb = Unix.gettimeofday () in
  let doc = Doc.bump_version ~token ~version ~raw doc in
  let diff = Unix.gettimeofday () -. tb in
  Io.Log.trace "bump file" "took %f seconds" diff;
  (* Just in case for the future, we update the document before requesting it to
     be checked *)
  let invalid = Handle.update_doc_version ~doc in
  let reason = Reason.ChangeDocument in
  Check.schedule ~uri ~reason;
  invalid

let change ~io ~token ~(doc : Doc.t) ~version ~raw =
  if version > doc.version then change ~io ~token ~doc ~version ~raw
  else
    (* That's a weird case, get got changes without a version bump? Do nothing
       for now *)
    IS.empty

let change ~io ~token ~uri ~version ~raw =
  let kind = "Theory.change" in
  let default () = IS.empty in
  let f _ doc = change ~io ~token ~doc ~version ~raw in
  Handle.with_doc ~kind ~f ~uri ~default

let close ~uri =
  (* XXX: Our handling of the "queue" is not good, handle should take care of
     that as to avoid desync between the queue and the handle table. *)
  Check.deschedule ~uri;
  Handle.close ~uri

let workspace_update () =
  Check.clear_schedule ();
  Handle.workspace_update ()

module Request = struct
  type request =
    | Immediate
    | FullDoc
    | PosInDoc of
        { point : int * int
        ; version : int option
        }

  type t =
    { id : int
    ; uri : Lang.LUri.File.t
    ; postpone : bool
    ; request : request
    }

  type action =
    | Now of Doc.t
    | Postpone
    | Cancel

  let request_in_range ~(doc : Doc.t) ~version (line, col) =
    let in_range =
      match doc.completed with
      | Yes _ -> true
      | Failed range | Stopped range -> Doc.Target.reached ~range (line, col)
      | WorkspaceUpdated _ -> false
    in
    let in_range =
      match version with
      | None -> in_range
      | Some version -> doc.version >= version && in_range
    in
    in_range

  (** Add a request to be served; returns [true] if request is added to the
      queue , [false] if the request can be already answered. *)
  let add { id; uri; postpone; request } =
    let kind = "Request.add" in
    let default () = Cancel in
    (* should be Cancelled? *)
    match request with
    | Immediate -> Handle.with_doc ~kind ~default ~uri ~f:(fun _ doc -> Now doc)
    | FullDoc ->
      Handle.with_doc ~kind ~default ~uri ~f:(fun _ doc ->
          match (Doc.Completion.is_completed doc.completed, postpone) with
          | true, _ -> Now doc
          | false, false -> Cancel
          | false, true ->
            Handle.attach_cp_request ~uri ~id;
            let reason = Reason.RequestDoc id in
            Check.schedule ~uri ~reason;
            Postpone)
    | PosInDoc { point; version } ->
      Handle.with_doc ~kind ~default ~uri ~f:(fun _ doc ->
          let in_range = request_in_range ~doc ~version point in
          match (in_range, postpone) with
          | true, _ -> Now doc
          | false, true ->
            Handle.attach_pt_request ~uri ~id ~point;
            let reason = Reason.RequestPos (id, point) in
            Check.schedule ~uri ~reason;
            Postpone
          | false, false -> Cancel)

  (** Removes the request from the list of things to wake up *)
  let remove { id; uri; postpone = _; request } =
    match request with
    | Immediate -> ()
    | FullDoc -> Handle.remove_cp_request ~uri ~id
    | PosInDoc { point; _ } -> Handle.remove_pt_request ~uri ~id ~point
end
