

open Core.Std
open Loach

(* Common parameter definitions and references *)
let defi = paramdef "i" "node"
let defj = paramdef "j" "node"

let i = paramref "i"
let j = paramref "i"

(* Constants *)
let _I = const (strc "I")
let _T = const (strc "T")
let _C = const (strc "C")
let _E = const (strc "E")
let _True = const (boolc true)
let _False = const (boolc false)

(* Self-defined types *)
let types = [
  enum "state" (str_consts ["I"; "T"; "C"; "E"]);
  enum "bool" (bool_consts [true; false]);
  enum "node" (int_consts [1; 2; 3]);
]

(* Variables *)
let vardefs = [
  singledef "x" "bool";
  arrdef "n" [defi] "state";
]

(* Initialization *)
let init =
  let a1 = assign (single "x") _True in
  let an = assign (arr "n" [paramindex "node" (indexref "i")]) _I in
  let a2 = AbsStatement(an, [indexdef "i" "node"]) in
  parallel [a1; a2;]

let rules = [
(
let name = "try" in
let params = [defi] in
let formula = eqn (var (arr "n" [i])) _I in
let statement = assign (arr "n" [i]) _T in
rule name params formula statement
);
(
let name = "crit" in
let params = [defi] in
let formula =
  let f1 = eqn (var (arr "n" [i])) _T in
  let f2 = eqn (var (single "x")) _True in
  andList [f1; f2]
in
let statement =
  let s1 = assign (arr "n" [i]) _C in
  let s2 = assign (single "x") _False in
  parallel [s1; s2]
in
rule name params formula statement
);
(
let name = "exit" in
let params = [defi] in
let formula = eqn (var (arr "n" [i])) _C in
let statement = assign (arr "n" [i]) _E in
rule name params formula statement
);
(
let name = "idle" in
let params = [defi] in
let formula = eqn (var (arr "n" [i])) _E in
let statement =
  let s1 = assign (single "x") _True in
  let s2 = assign (arr "n" [i]) _I in
  parallel [s1; s2]
in
rule name params formula statement
);
]

let properties = [
(
let name = "coherence" in
let params = [defi; defj] in
let formula =
  let f1 = eqn (var (arr "n" [i])) _C in
  let f2 = eqn (var (arr "n" [j])) _C in
  andList [f1; f2]
in
prop name params formula
);
]

let protocol = Trans.act ~loach:{
  types;
  vardefs;
  init;
  rules;
  properties;
};;

let open Paramecium in
let form =
  let f1 = eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "C")) in
  let f2 = eqn (var (arr "n" [paramfix "node" (intc 2)])) (const (strc "C")) in
  andList [f1; f2]
in
ToStr.Smt2.act ~types:protocol.types ~vardefs:protocol.vardefs form
|> Smt.is_tautology ~quiet:false
|> printf "%b\n";;

let open Paramecium in
let form =
  let f1 = eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "C")) in
  let f2 = neg (eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "C"))) in
  andList [f1; f2]
in
ToStr.Smt2.act ~types:protocol.types ~vardefs:protocol.vardefs form
|> Smt.is_tautology ~quiet:false
|> printf "%b\n";;

try
  let open Paramecium in
  let form =
    let f1 = eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "C")) in
    let f2 = eqn (var (arr "n" [paramfix "node" (intc 2)])) (const (strc "EE")) in
    andList [f1; f2]
  in
  ToStr.Smt2.act ~types:protocol.types ~vardefs:protocol.vardefs form
  |> Smt.is_tautology ~quiet:false
  |> printf "%b\n"
with _ -> ();;

try
  let open Paramecium in
  let form =
    let f1 = eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "C")) in
    let f2 = eqn (var (arr "n" [paramfix "node" (intc 2)])) (const (strc "C")) in
    andList [f1; f2]
  in
  ToStr.Smv.form_act (neg form)
  |> Smv.is_inv_by_smv ~quiet:false ~smv_file:"/home/duan/mutualEx.smv"
  |> printf "%b\n"
with _ -> ();;

let open Paramecium in
let form1 =
  let f1 = eqn (var (arr "n" [paramfix "node" (intc 1)])) (const (strc "C")) in
  let f2 = eqn (var (arr "n" [paramfix "node" (intc 2)])) (const (strc "C")) in
  andList [f1; f2]
in
let form2 =
  let f1 = eqn (var (arr "n" [paramfix "node" (intc 2)])) (const (strc "C")) in
  let f2 = eqn (var (arr "n" [paramfix "node" (intc 3)])) (const (strc "C")) in
  andList [f1; f2]
in
printf "\nThe two formulae:\n%s\n%s\nare %ssymmetric\n" (ToStr.Smv.form_act form1) 
  (ToStr.Smv.form_act form2) (if Formula.form_are_symmetric form1 form2 then "" else "not ");;
