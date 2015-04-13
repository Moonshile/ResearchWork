(** Check a invariant with NuSMV

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils

open Core.Std

let smv_path = "/home/duan/Downloads/NuSMV/bin/NuSMV"

(* Raises when there are some errors in the NuSMV code *)
exception Error_in_smv

(** Judge if a given invariant is true invariant

    @param filename is the temp file to store smv inv file, default is "inv.smv"
    @param quiet true (default) to prevent to print output of smt solver to screen
    @param smv_file the original smv file for computing reachable set
    @param inv the inv to be judged
    @return true if is true invariant else false
*)
let is_inv ?(filename="inv.smv") ?(quiet=true) ~smv_file inv =
  let (stdout, stderr) =
    let check_str = sprintf "go\ncompute_reachable\ncheck_invar -p \"%s\"\nquit\n" inv in
    let args = ["-dcx"; "-int"; "-old"; smv_file] in
    exec_with_input ~prog:smv_path ~args check_str
  in
  if not quiet then Prt.info "The invariant to be checked is:\n";printf "%s\n" inv;
  let pattern = Re2.Regex.create_exn "-- inv.+\n" in
  try
    let smv_res = String.strip (Re2.Regex.find_first_exn pattern stdout) in
    let res = String.is_suffix smv_res ~suffix:"true" in
    if quiet then res else (Prt.info "Result of smv check is:\n";printf "%s\n" smv_res; res)
  with
  | _ -> Prt.error "NuSMV error info:\n";printf "%s" stderr; raise Error_in_smv

