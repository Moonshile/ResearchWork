(** Check a invariant with NuSMV

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

(* Raises when there are some errors in the NuSMV code *)
exception Error_in_smv

(** Judge if a given invariant is true invariant

    @param quiet true (default) to prevent to print output of smt solver to screen
    @param smv_file the original smv file for computing reachable set
    @param formula the formula to be judged
    @return true if is true invariant else false
*)
val is_inv_by_smv : ?quiet:bool -> smv_file:string -> string -> bool

