(** Check a invariant with NuSMV

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils

open Core.Std

(* Raises when there are some errors in the NuSMV code *)
exception Error_in_smv
exception Name_not_known

let protocol_name = ref ""

let set_smv_context name smv_file_content =
  protocol_name := name;
  let _res = Client.Smv.compute_reachable name smv_file_content in
  let diameter = ref 0 in
  while !diameter = 0 do
    Unix.sleep 1;
    diameter := Client.Smv.query_reachable name;
  done;
  !diameter


(** Judge if a given invariant is true invariant

    @param quiet true (default) to prevent to print output of smt solver to screen
    @param inv the inv to be judged
    @return true if is true invariant else false
*)
let is_inv_by_smv ?(quiet=true) inv =
  if (!protocol_name) = "" then raise Name_not_known
  else begin Client.Smv.check_inv (!protocol_name) inv end
