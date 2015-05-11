(** Check a invariant with NuSMV

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

(* Raises when there are some errors in the NuSMV code *)
exception Error_in_smv
exception Name_not_known

val set_smv_context : string -> string -> int

(** Judge if a given invariant is true invariant

    @param quiet true (default) to prevent to print output of smt solver to screen
    @param inv the inv to be judged
    @return true if is true invariant else false
*)
val is_inv_by_smv : ?quiet:bool -> string -> bool

