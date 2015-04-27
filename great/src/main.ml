

open Core.Std
open Utils
open Paramecium
open Formula

(* Common parameter definitions and references *)
let defi = paramdef "i" "node"
let defj = paramdef "j" "node"

let i = paramref "i"
let j = paramref "j"

(* Constants *)
let _I = const (strc "i")
let _T = const (strc "t")
let _C = const (strc "c")
let _E = const (strc "e")
let _True = const (boolc true)
let _False = const (boolc false)

(* Self-defined types *)
let types = [
  enum "state" (str_consts ["i"; "t"; "c"; "e"]);
  enum "bool" (bool_consts [true; false]);
  enum "node" (int_consts [1; 2; 3]);
]

(* Variables *)
let vardefs = [
  arrdef "x" [] "bool";
  arrdef "n" [defi] "state";
]

(* Initialization *)
let init =
  let a1 = assign (arr "x" []) _True in
  let a21 = assign (arr "n" [paramfix "node" (intc 1)]) _I in
  let a22 = assign (arr "n" [paramfix "node" (intc 2)]) _I in
  let a23 = assign (arr "n" [paramfix "node" (intc 3)]) _I in
  parallel [a1; a21; a22; a23]

let rule_try = 
  let name = "try" in
  let params = [defi] in
  let formula = eqn (var (arr "n" [i])) _I in
  let statement = assign (arr "n" [i]) _T in
  rule name params formula statement

let rule_crit =
  let name = "crit" in
  let params = [defi] in
  let formula =
    let f1 = eqn (var (arr "n" [i])) _T in
    let f2 = eqn (var (arr "x" [])) _True in
    andList [f1; f2]
  in
  let statement =
    let s1 = assign (arr "n" [i]) _C in
    let s2 = assign (arr "x" []) _False in
    parallel [s1; s2]
  in
  rule name params formula statement

let rule_exit =
  let name = "exit" in
  let params = [defi] in
  let formula = eqn (var (arr "n" [i])) _C in
  let statement = assign (arr "n" [i]) _E in
  rule name params formula statement

let rule_idle =
  let name = "idle" in
  let params = [defi] in
  let formula = eqn (var (arr "n" [i])) _E in
  let statement =
    let s1 = assign (arr "x" []) _True in
    let s2 = assign (arr "n" [i]) _I in
    parallel [s1; s2]
  in
  rule name params formula statement

let rules = [rule_try; rule_crit; rule_exit; rule_idle]

let coherence =
  let name = "coherence" in
  let params = [defi; defj] in
  let formula =
    let f1 = eqn (var (arr "n" [i])) _C in
    let f2 = eqn (var (arr "n" [j])) _C in
    andList [f1; f2]
  in
  prop name params formula

let properties = [coherence]

let protocol = {
  name = "mutualEx";
  types;
  vardefs;
  init;
  rules;
  properties;
};;

open InvFinder;;

let prop_params = [("i", paramfix "node" (intc 1)); ("j", paramfix "node" (intc 2))]

let [(cinv, invs, relations)] = find ~protocol ~prop_params:[prop_params];;

let invs_str = 
  invs
  |> List.map ~f:neg
  |> List.map ~f:(simplify ~types ~vardefs)
  |> List.map ~f:ToStr.Smv.form_act;;

let relations_str = List.map relations ~f:(to_str ~types ~vardefs);;

Prt.info (String.concat ~sep:"\n" relations_str);;
printf "\n";;
Prt.warning (String.concat ~sep:"\n" invs_str);;
printf "\n";;