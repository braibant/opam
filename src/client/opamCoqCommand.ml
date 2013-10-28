

open OpamTypes
open OpamState.Types

let ncoq = OpamPackage.Name.of_string "coq"

(* The rationale behind this file is that it automates the calls to
   more primitive opam commands. Despite the name though, it sits
   above OpamClient. Indeed, we need to perform calls to the safe API
   exposed by OpamClient, rather than direct calls to more primitive
   functions (e.g. exposed OpamSwitchCommand). The later solution
   proved troublesome, because it led to spurious error messages, and
   incorrect switches between staes. *)

(* create a new switch, based on the current compiler and the given
   coq version. It will reinstall the packages that existed in the
   current switch. The naming scheme of the new switch is
   "camlversion--coqversion".  
   
   The keep option tells whether the new switch should contain the
   same pakcages as the previous one

*)
let install ~keep coq =
  let t = OpamState.load_state "coq-install-1" in
  let old_switch = t.switch in 

  (* compute package version map for the new switch *)
  let map = begin
    if keep 
    then 
      let map = OpamState.installed_map t in
      let map = OpamPackage.Name.Map.remove (OpamPackage.name coq) map in 
      OpamPackage.Name.Map.add (OpamPackage.name coq) (OpamPackage.version coq) map 
    else
      OpamPackage.Name.Map.add (OpamPackage.name coq) (OpamPackage.version coq) (OpamPackage.Name.Map.empty)
  end in
  let packages = List.map (fun (n,v) -> OpamPackage.create n v) (OpamPackage.Name.Map.bindings map) in 
  let package_set = OpamPackage.Set.of_list packages in 

  let roots = OpamPackage.names_of_packages package_set in 

  (* The set of constrained atoms from packages (with versions) *)
  let atoms = OpamSolution.eq_atoms_of_packages package_set in

  (* compiler name for the new switch *)
  let compiler = t.compiler in 
  
  let switch = OpamSwitch.of_string (OpamCompiler.to_string compiler ^ "--" ^ OpamPackage.to_string coq) in 

  OpamGlobals.msg "New switch for Coq: %s\n"(OpamSwitch.to_string switch);

  OpamClient.SafeAPI.SWITCH.install ~quiet:false ~warning:true ~update_config:false switch compiler;

  OpamClient.SafeAPI.SWITCH.switch ~quiet:true ~warning:false switch;
  
  let t = OpamState.load_state "coq-install-2" in

  (* reinstall the packages with the right versions in the new switch *)
  let wish_install = [] in 
  let wish_remove = [] in 
  let wish_upgrade = atoms in 
  try 
    let  solution = OpamSolution.resolve_and_apply ~force:true t (Install roots) 
      {wish_install; wish_remove ; wish_upgrade} in
    OpamSolution.check_solution t solution;
    (* Pin the Coq version in this switch  *)
    (* TODO: currently, pinning the package remove the build dir, which is troublesome for our --local packages... *)
    let pin = { pin_package = OpamPackage.name coq; pin_option = Version (OpamPackage.version coq) } in
    OpamClient.SafeAPI.PIN.pin ~force:true pin
  with
  | e -> 
    begin 
      OpamClient.SafeAPI.SWITCH.switch ~quiet:false ~warning:false old_switch;
      OpamClient.SafeAPI.SWITCH.remove switch
    end

let pinning_version t switch package =
  let pinned = OpamFile.Pinned.safe_read (OpamPath.Switch.pinned t.root switch) in
  let name = OpamPackage.name package in
  try
    match OpamPackage.Name.Map.find name pinned with
    | Version v -> OpamPackage.create name v
    | _ -> raise Not_found
  with Not_found ->  package

(* since some packages may be pinned, there is some work to do here to get the right version *)
let installed_versions t =
  let map = OpamSwitch.Map.fold (fun switch _ accu ->
    let installed_f = OpamPath.Switch.installed t.root switch in
    let installed = OpamFile.Installed.safe_read installed_f in
    let coqs = OpamPackage.Set.filter (fun nv -> OpamPackage.name nv = ncoq) installed in 
    OpamPackage.Set.fold (fun coq map -> 
      let coq = pinning_version t switch coq in 
      if OpamPackage.Map.mem coq map 
      then
	let aliases = OpamPackage.Map.find coq map in
        let map = OpamPackage.Map.remove coq map in
	OpamPackage.Map.add coq (switch::aliases) map
      else
	OpamPackage.Map.add coq [switch] map
    ) coqs accu
  ) t.aliases OpamPackage.Map.empty
  in 
  (* let _ = OpamPackage.Map.iter (fun k v -> OpamGlobals.msg "%s -> %s\n" (OpamPackage.to_string k) (OpamMisc.pretty_list (List.map OpamSwitch.to_string v))) map in *)
  map

let available_coqs t =
  let package_index =
    OpamFile.Package_index.safe_read (OpamPath.package_index t.root) in
  OpamPackage.Map.fold (fun p _ accu ->
    if OpamPackage.name p = ncoq 
    then OpamPackage.Set.add p accu 
    else accu
  ) package_index OpamPackage.Set.empty

let list ~installed ~all =
  let t = OpamState.load_state "switch-list" in
  let map = installed_versions t  in 
  (* switch -- status -- coq-version -- coq descr *)

  let s_installed = "I" in
  let s_current = "C" in 
  let s_not_installed = "--" in 

  let installed,coqs = OpamPackage.Map.fold (fun coq -> 
    (* Return the actual versionned package  *)
    List.fold_right (fun s (acc,coqs) -> 
      let switch = OpamSwitch.to_string s in 
      let scoq = OpamPackage.to_string coq in 
      let status = if s = t.switch then s_current else s_installed in 
      let descr = OpamFile.Descr.safe_read (OpamPath.descr t.root coq) in 
      let coqs = OpamPackage.Set.add coq coqs in  
      (switch,status,scoq,descr)::acc, coqs
    )) map ([], OpamPackage.Set.empty) in

  let all = 
    if all 
    then 
      let available_coqs = available_coqs t in 
      let other_coqs = OpamPackage.Set.diff available_coqs coqs in 
      let other_coqs = OpamPackage.Set.fold 
	(fun coq acc ->
	  let descr = OpamFile.Descr.safe_read (OpamPath.descr t.root coq) in 
	  (s_not_installed, s_not_installed, OpamPackage.to_string coq, descr) :: acc) other_coqs []
	 in 
      installed @ other_coqs
    else 
      installed
  in 
  let max_switch, max_status, max_coq =
    List.fold_left (fun (n,s,c) (switch, status, coq, _) ->
      let n = max (String.length switch) n in
      let s = max (String.length status) s in
      let c = max (String.length coq) c in
      (n, s, c)
    ) (0,0,0) all in

  let count = ref (List.length all) in

  let print_coq (switch, status, coq, descr) =
    decr count;
    let bold_current s =
      if switch = OpamSwitch.to_string t.switch
      then OpamGlobals.colorise `bold s
      else s in
    let colored_switch = bold_current switch in
    let colored_status =
      if status = s_not_installed then status else
        bold_current (OpamGlobals.colorise `blue status) in
    let colored_coq =
      bold_current (OpamGlobals.colorise `yellow coq) in
    let colored_descr = bold_current (OpamFile.Descr.synopsis descr) in
    let colored_body =
      if !OpamGlobals.verbose then
        match OpamMisc.strip (OpamFile.Descr.body descr) with
        | "" -> ""
        | d  -> "\n"^d^"\n"
      else "" in
    OpamGlobals.msg "%s %s %s  %s%s\n"
      (OpamMisc.indent_left colored_switch ~visual:switch max_switch)
      (OpamMisc.indent_right colored_status ~visual:status max_status)
      (OpamMisc.indent_left colored_coq ~visual:coq max_coq)
      colored_descr colored_body in

  List.iter print_coq all


let switch ~keep coq =
  let t = OpamState.load_state "coq-switch-1" in
  let map = installed_versions t in 
  let _ = OpamPackage.Map.iter (fun k v -> OpamGlobals.msg "%s -> %s" (OpamPackage.to_string k) (OpamMisc.pretty_list (List.map OpamSwitch.to_string v))) map in
  try 
    begin match OpamPackage.Map.find coq map with
    | [] -> assert false
    | [t] -> OpamClient.SafeAPI.SWITCH.switch ~quiet:true ~warning:true t
    | _ -> OpamGlobals.error "Multiple switches exists for %s. Please use [opam switch] to select one."  
      (OpamPackage.to_string coq)
    end
  with Not_found -> 
    OpamGlobals.msg "No switch for %s. Installing one now...\n" (OpamPackage.to_string coq);
    install ~keep coq
  
let remove coq = 
  let t = OpamState.load_state "coq-remove-1" in
  let map = installed_versions t in 
  try 
    begin match OpamPackage.Map.find coq map with
    | [] -> assert false
    | [t] -> OpamClient.SafeAPI.SWITCH.remove t
    | _ -> OpamGlobals.error "Multiple switches exists for %s. Please use [opam switch remove] to select the one to remove."  
      (OpamPackage.to_string coq)
    end
  with Not_found -> 
    begin 
      OpamGlobals.msg "No switch for %s. Nothing to do. \n" (OpamPackage.to_string coq);
      ()
    end
  
 
      

  
   
  
  
