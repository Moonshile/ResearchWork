(** Check a formula with SMT solver

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils

open Core.Std

let smt_solver = "z3"

(** Raises when there is an error in the formula to be judged *)
exception Error_in_formula

(** Judge if a given formula is satisfiable

    @param filename is the temp file to store smt2 formula, default is "inv.smt2"
    @param quiet true (default) to prevent to print output of smt solver to screen
    @param formula the formula to be judged
    @return true if is satisfiable else false
*)
let is_satisfiable ?(quiet=true) formula =
  let (smt, _) =
    exec_with_input ~prog:smt_solver ~args:["-smt2"; "-in"] formula
  in
  let not_quiet () =
    if not quiet then
      (Prt.info "The smt2 formula to be checked is:\n";printf "%s\n" formula)
    else begin () end
  in
  not_quiet ();
  let print_smt printer = printer (sprintf "Result of smt check is:\n%s" smt) in
  if not (any ["sat"; "unsat"] ~f:(fun prefix -> String.is_prefix smt ~prefix)) then
    (print_smt Prt.error; raise Error_in_formula)
  else begin
    let res = String.is_prefix smt ~prefix:"sat" in
    if quiet then res else (print_smt Prt.info; res)
  end

