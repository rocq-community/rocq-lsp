open Constrexpr

let debug = ref false

let intern_reference qid =
  if !debug then
    Feedback.msg_warning Pp.(str "ir [<-] " ++ Libnames.pr_qualid qid);
  let r =
    try Some (Nametab.locate_extended qid) with
    | Not_found -> None
    (* XXX behavior here is dubious unfortunately, as we see a var as a ref,
       GlobalizationError is raised *)
    | Nametab.GlobalizationError _ -> None
    | _ ->
      Feedback.msg_warning Pp.(str "uuuuuuhgggh");
      None
  in
  let r = Option.bind r Smartlocate.global_of_extended_global in
  if !debug then
    Feedback.msg_warning Pp.(str "ir [->] " ++ Pp.pr_opt Printer.pr_global r);
  r

(* From a term to its representation with notations *)
let recover_notation env sigma t =
  let gt = Constrintern.intern_constr env sigma t in
  let flags = PrintingFlags.Extern.current () in
  let flags = { flags with notations = true } in
  let eenv = Constrextern.extern_env ~flags env sigma in
  Constrextern.extern_glob_type eenv gt |> fun t ->
  match t.CAst.v with
  | CNotation _ -> Some t
  | _ -> None

let _recover_notation env sigma t =
  try recover_notation env sigma t (* Due to wrong env passed *)
  with exn ->
    Feedback.msg_warning
      (let iexn = Exninfo.capture exn in
       Pp.(str "error in recover_notation: " ++ CErrors.iprint iexn));
    None

(* From a notation to a notation-free term *)
let notation_raw env sigma t =
  if !debug then
    Feedback.msg_warning
      Pp.(
        str "nr [<-] "
        ++ Ppconstr.pr_constr_expr
             ~flags:(Ppconstr.current_flags ())
             env sigma t);
  (* Wish: In place of full internalization + notation-free extern, we could have an operation
   *
   * [expand_notation : constr_expr -> constr_expr]
   * [expand_implicits : constr_expr -> constr_expr]
   *
   *)
  let gt = Constrintern.intern_constr env sigma t in
  let r =
    let flags = PrintingFlags.Extern.current () in
    let flags = { flags with notations = false } in
    let eenv = Constrextern.extern_env ~flags env sigma in
    Constrextern.extern_glob_type eenv gt
  in
  if !debug then
    Feedback.msg_warning
      Pp.(
        str "nr [->] "
        ++ Ppconstr.pr_constr_expr
             ~flags:(Ppconstr.current_flags ())
             env sigma r);
  r

let notation_raw env sigma t =
  try
    Some (notation_raw env sigma t)
    (* Due to wrong env passed, usually when traversing terms *)
  with exn ->
    Feedback.msg_warning
      (let iexn = Exninfo.capture exn in
       Pp.(str "error in notation_raw: " ++ CErrors.iprint iexn));
    None

module Id = struct
  type 'a t =
    { relative : Libnames.qualid
    ; absolute : Libnames.full_path option
    ; typ : 'a option
    }

  (* Uh, this requires to use the global env... *)
  let type_of_global gref =
    let env = Global.env () in
    let typ, auctx = Typeops.type_of_global_in_context env gref in
    let typ = Arguments_renaming.rename_type env typ gref in
    let auctx = Printer.fill_names auctx in
    let sigma = Evd.from_auctx env auctx in
    Constrextern.extern_type ~flags:(PrintingFlags.current ()) env sigma
      (EConstr.of_constr typ)

  let type_of_global gref = try Some (type_of_global gref) with _ -> None

  let path_of_global gref =
    try Some (Nametab.path_of_global gref) with _ -> None

  let make qid =
    let gref = intern_reference qid in
    let typ = Option.bind gref type_of_global in
    { relative = qid; absolute = Option.bind gref path_of_global; typ }

  let map_typ ~f info = { info with typ = Option.map f info.typ }
end
