

open Core.Std
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

let rule_params = [("i", paramfix "node" (intc 1));]
let prop_params = [("i", paramfix "node" (intc 1)); ("j", paramfix "node" (intc 2))]

let crule = concrete_rule rule_crit rule_params
let cinv = concrete_prop coherence prop_params

let table = tabular_crules_cinv [crule] cinv
  ~new_inv_id:0 ~smv_file:"mutualEx.smv" ~types ~vardefs

let tables = find ~protocol ~prop_params:[prop_params]

let [(cinv, invs, relations)] = tables

let invs_str = List.map invs ~f:ToStr.Smv.form_act

(*let [(_, [(invs, relations)])] = find protocol [[prop_params]]*)

(*
let form =
  let f1 = eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "c")) in
  let f2 = eqn (var (arr "n" [paramfix "node" (intc 2)])) (const (strc "c")) in
  andList [f1; f2]
in
form
|> is_tautology ~quiet:false ~types:protocol.types ~vardefs:protocol.vardefs
|> printf "%b\n";;

let form =
  let f1 = eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "c")) in
  let f2 = neg (eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "c"))) in
  orList [f1; f2]
in
form
|> is_tautology ~quiet:false ~types:protocol.types ~vardefs:protocol.vardefs
|> printf "%b\n";;

try
  let form =
    let f1 = eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "c")) in
    let f2 = eqn (var (arr "n" [paramfix "node" (intc 2)])) (const (strc "ee")) in
    andList [f1; f2]
  in
  form
  |> is_tautology ~quiet:false ~types:protocol.types ~vardefs:protocol.vardefs
  |> printf "%b\n"
with _ -> ();;

try
  let form =
    let f1 = eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "c")) in
    let f2 = eqn (var (arr "n" [paramfix "node" (intc 2)])) (const (strc "c")) in
    andList [f1; f2]
  in
  ToStr.Smv.form_act (neg form)
  |> Smv.is_inv_by_smv ~quiet:false ~smv_file:"/home/duan/mutualEx.smv"
  |> printf "%b\n"
with _ -> ();;

let form1 =
  let f1 = eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "c")) in
  let f2 = eqn (var (arr "n" [paramfix "node" (intc 2)])) (const (strc "c")) in
  andList [f1; f2]
in
let form2 =
  let f1 = eqn (var (arr "n" [paramfix "node" (intc 2)])) (const (strc "c")) in
  let f2 = eqn (var (arr "n" [paramfix "node" (intc 3)])) (const (strc "c")) in
  andList [f1; f2]
in
printf "\nThe two formulae:\n%s\n%s\nare %ssymmetric\n" (ToStr.Smv.form_act form1) 
  (ToStr.Smv.form_act form2) (if Formula.form_are_symmetric form1 form2 then "" else "not ");;
*)