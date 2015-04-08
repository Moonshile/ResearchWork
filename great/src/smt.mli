(** Check a formula with SMT solver

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

(** Raises when there is an error in the formula to be judged *)
exception Error_in_formula

(** Judge if a given formula is tautology

    @param filename is the temp file to store smt2 formula, default is "inv.smt2"
    @param print_to_scr set true to print output of smt solver to screen else false
    @param formula the formula to be judged
    @return true if is tautology else false
*)
val is_tautology : ?filename:string -> ?print_to_scr:bool -> formula:string -> unit -> bool
