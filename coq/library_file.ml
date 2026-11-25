(*************************************************************************)
(* Copyright 2015-2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2019-2024 Inria           -- Dual License LGPL 2.1+ / GPL3+ *)
(* Copyright 2024-2025 Emilio J. Gallego Arias  -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                     -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors            *)
(*************************************************************************)
(* Rocq Language Server Protocol: .vo file API                           *)
(*************************************************************************)

type t = Names.DirPath.t

let name n = n
let loaded ~token ~st = State.in_state ~token ~st ~f:Library.loaded_libraries ()

(* Function to extract definitions and lemmas from the environment *)
module DynHandle = Libobject.Dyn.Map (struct
  type 'a t = 'a -> unit
end)

(* Prefix is the module / section things are defined *)
let const_handler
    (fn : Names.Constant.t -> Decls.logical_kind -> Constr.t -> unit) prefix
    (id, obj) =
  let open Names in
  let kn = KerName.make prefix.Libobject.obj_mp id in
  let cst = Global.constant_of_kn kn in
  let gr = GlobRef.ConstRef cst in
  let env = Global.env () in
  let typ, _ = Typeops.type_of_global_in_context env gr in
  let kind = Declare.Internal.Constant.kind obj in
  fn cst kind typ

let iter_constructors indsp u fn env nconstr =
  for i = 1 to nconstr do
    let typ =
      Inductive.type_of_constructor
        ((indsp, i), u)
        (Inductive.lookup_mind_specif env indsp)
    in
    fn (Names.GlobRef.ConstructRef (indsp, i)) typ
  done

let ind_handler fn prefix (id, (_obj : DeclareInd.Internal.inductive_obj)) =
  let open Names in
  let kn = KerName.make prefix.Libobject.obj_mp id in
  let mind = Global.mind_of_kn kn in
  let mib = Global.lookup_mind mind in
  let env = Global.env () in
  let iter_packet i mip =
    let ind = (mind, i) in
    let u =
      UVars.make_abstract_instance
        (Declareops.inductive_polymorphic_context mib)
    in
    let typ =
      Inductive.type_of_inductive (Inductive.lookup_mind_specif env ind, u)
    in
    let () = fn (GlobRef.IndRef ind) typ in
    let len = Array.length mip.Declarations.mind_user_lc in
    iter_constructors ind u fn env len
  in
  Array.iteri iter_packet mib.mind_packets

let handler fn_c fn_i prefix =
  DynHandle.add Declare.Internal.Constant.tag (const_handler fn_c prefix)
  @@ DynHandle.add DeclareInd.Internal.objInductive (ind_handler fn_i prefix)
  @@ DynHandle.empty

let handle h (Libobject.Dyn.Dyn (tag, o)) =
  match DynHandle.find tag h with
  | f -> f o
  | exception Stdlib.Not_found -> ()

let obj_action fn_c fn_i prefix lobj =
  match lobj with
  | Libobject.AtomicObject o -> handle (handler fn_c fn_i prefix) o
  | _ -> ()

let constructor_name c idx =
  let cname =
    Nametab.shortest_qualid_of_global Names.Id.Set.empty
      (Names.GlobRef.ConstructRef (c, idx))
  in
  Libnames.string_of_qualid cname

let constructor_info (gref : Names.GlobRef.t) =
  match gref with
  | ConstructRef (ind, idx) ->
    let ind_dp = Names.(MutInd.modpath (fst ind) |> ModPath.dp) in
    Some (ind_dp, constructor_name ind idx)
  | VarRef _ | ConstRef _ | IndRef _ -> None

let belongs_to_lib dps dp =
  List.exists (fun p -> Libnames.is_dirpath_prefix_of p dp) dps

module Entry = struct
  type t =
    { name : string
    ; typ : Constr.t
    ; file : string
    }
end

let find_v_file dir =
  match Loadpath.locate_absolute_library dir with
  (* EJGA: we want to improve this as to pass the error to the client *)
  | Error _ -> "error when trying to locate the .v file"
  | Ok file -> file

let toc dps : Entry.t list =
  let res : Entry.t list ref = ref [] in
  let obj_action =
    let fn_c (cst : Names.Constant.t) (_ : Decls.logical_kind) (typ : Constr.t)
        =
      let cst_dp = Names.(Constant.modpath cst |> ModPath.dp) in
      if belongs_to_lib dps cst_dp then
        (* let () = F.eprintf "cst found: %s@\n%!" (Names.Constant.to_string
           cst) in *)
        let name = Names.Constant.to_string cst in
        let file = find_v_file cst_dp in
        res := { name; typ; file } :: !res
      else ()
    in
    (* We do nothing for inductives, note this is called both on constructors
       and inductives, with the name and type *)
    let fn_i (gref : Names.GlobRef.t) (typ : Constr.t) =
      match constructor_info gref with
      | None -> ()
      | Some (ind_dp, name) ->
        if belongs_to_lib dps ind_dp then
          let file = find_v_file ind_dp in
          res := { name; typ; file } :: !res
    in
    obj_action fn_c fn_i
  in
  let () = Declaremods.iter_all_interp_segments obj_action in
  List.rev !res

let toc ~token ~st dps = State.in_state ~token ~st ~f:toc dps
