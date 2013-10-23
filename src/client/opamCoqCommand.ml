

open OpamTypes
open OpamState.Types

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
   "camlversion--coqversion". *)
let install coq =
  let t = OpamState.load_state "coq-install-1" in
  let old_switch = t.switch in 

  (* compute package version map for the new switch *)
  let map = OpamState.installed_map t in
  let map = OpamPackage.Name.Map.remove (OpamPackage.name coq) map in 
  let map = OpamPackage.Name.Map.add (OpamPackage.name coq) (OpamPackage.version coq) map in 

  let packages = List.map (fun (n,v) -> OpamPackage.create n v) (OpamPackage.Name.Map.bindings map) in 
  let package_set = OpamPackage.Set.of_list packages in 

  let roots = OpamPackage.names_of_packages package_set in 

  (* let _ = OpamPackage.Name.Set.iter (fun n -> OpamGlobals.msg "%s\n" (OpamPackage.Name.to_string n)) roots in  *)

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
    OpamSolution.check_solution t solution
  with
  | e -> 
    begin 
      OpamClient.SafeAPI.SWITCH.switch ~quiet:false ~warning:false old_switch;
      OpamClient.SafeAPI.SWITCH.remove switch
    end


let list ~installed ~all =
  let t = OpamState.load_state "switch-list" in

  (* switch -- status -- coq-version -- coq descr *)

  let s_installed = "I" in
  let s_current = "C" in 
  let s_not_installed = "--" in 

  let installed = OpamPackage.Map.fold (fun coq -> 
    List.fold_right (fun s acc -> 
      let switch = OpamSwitch.to_string s in 
      let scoq = OpamPackage.to_string coq in 
      let status = if s = t.switch then s_current else s_installed in 
      let descr = OpamFile.Descr.safe_read (OpamPath.descr t.root coq) in 
      (switch,status,scoq,descr)::acc 
    )) (OpamState.installed_versions t (OpamPackage.Name.of_string "coq")) [] in

  let all = installed in 

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
  
let switch coq =
  let t = OpamState.load_state "coq-switch-1" in
  let map = OpamState.installed_versions t (OpamPackage.Name.of_string "coq") in
  
  try 
    begin match OpamPackage.Map.find coq map with
    | [] -> assert false
    | [t] -> OpamClient.SafeAPI.SWITCH.switch ~quiet:true ~warning:false t
    | _ -> OpamGlobals.error "Multiple switches exists for %s. Please use [opam switch] to select one."  
      (OpamPackage.to_string coq)
    end
  with Not_found -> 
    OpamGlobals.msg "No switch for %s. Installing one now...\n" (OpamPackage.to_string coq);
    install coq
  
let remove coq = 
  let t = OpamState.load_state "coq-remove-1" in
  let map = OpamState.installed_versions t (OpamPackage.Name.of_string "coq") in
  
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
  
 
      

  
   
  
  
