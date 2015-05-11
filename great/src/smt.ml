(** Check a formula with SMT solver

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils

open Core.Std

(** Raises when there is an error in the formula to be judged *)
exception Error_in_formula
exception Name_not_known

let protocol_name = ref ""

let set_smt_context name context =
  protocol_name := name;
  Client.Smt2.set_context name context

(** Judge if a given formula is satisfiable

    @param quiet true (default) to prevent to print output of smt solver to screen
    @param f the formula to be judged
    @return true if is satisfiable else false
*)
let is_satisfiable ?(quiet=true) f =
  if (!protocol_name) = "" then raise Name_not_known
  else begin Client.Smt2.check (!protocol_name) f end

