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
    @param quiet set true to print output of smt solver to screen else false
    @param formula the formula to be judged
    @return true if is tautology else false
*)
let is_tautology ?(filename="inv.smt2") ?(quiet=false) ~formula () =
  let smt = 
    create_file filename formula;
    exec ~prog:smt_solver ~args:["-smt2"; filename]
  in
  if not (any ["sat"; "unsat"] ~f:(fun prefix -> String.is_prefix smt ~prefix)) then
    (printf "%s" smt; raise Error_in_formula)
  else
    let res = String.is_prefix smt ~prefix:"unsat" in
    if quiet then (printf "%s" smt; res) else res

