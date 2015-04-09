(** Check a formula with SMT solver

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils

open Core.Std

let smt_solver = "z3"

(** Raises when there is an error in the formula to be judged *)
exception Error_in_formula

(* create a file named filename and fill it with content *)
let create_file filename content =
  let outf = Out_channel.create filename in
  fprintf outf "%s\n" content;
  Out_channel.close outf

(** Judge if a given formula is tautology

    @param filename is the temp file to store smt2 formula, default is "inv.smt2"
    @param quiet true (default) to prevent to print output of smt solver to screen
    @param formula the formula to be judged
    @return true if is tautology else false
*)
let is_tautology ?(filename="inv.smt2") ?(quiet=true) ~formula () =
  let smt = 
    create_file filename formula;
    exec ~prog:smt_solver ~args:["-smt2"; filename]
  in
  if not quiet then printf "The smt2 formula to be checked is:\n%s\n" formula;
  let print_smt = printf "Result of smt check is:\n%s" smt in
  if not (any ["sat"; "unsat"] ~f:(fun prefix -> String.is_prefix smt ~prefix)) then
    (print_smt; raise Error_in_formula)
  else
    let res = String.is_prefix smt ~prefix:"unsat" in
    if quiet then res else (print_smt; res)

