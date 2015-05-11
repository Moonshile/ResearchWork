(** Check a formula with SMT solver

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

(** Raises when there is an error in the formula to be judged *)
exception Error_in_formula

val set_context : string -> string -> bool

(** Judge if a given formula is satisfiable

    @param filename is the temp file to store smt2 formula, default is "inv.smt2"
    @param quiet true (default) to prevent to print output of smt solver to screen
    @param formula the formula to be judged
    @return true if is satisfiable else false
*)
val is_satisfiable : ?quiet:bool -> string -> bool
