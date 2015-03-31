open Loach

(* Self-defined types *)
let types = [
  StrEnum("state", ["I"; "T"; "C"; "E"]);
  StrEnum("bool", ["True"; "False"]);
  IntEnum("Parameter", [1; 2; 3]);
]

(* Variables *)
let vars = [
  Singledef("x", "bool");
  Arraydef("n", ["Parameter"], "state");
]

(* Initialization *)
let init =
  let a1 = Assign(Global "x", Const(Strc "True")) in
  let an = Assign(Array("n", [Var(Param "j")]), Const(Strc "I")) in
  let a2 = AbsStatement(an, [Singledef("j", "Parameter")]) in
  Parallel [a1; a2;]

let rules = [
(
let params = [Singledef("i", "Parameter")] in
let rule_try =
  let name = "try" in
  let formula = Eqn(Var(Array("n", [Var(Param "i")])), Const(Strc "I")) in
  let statement = Assign(Array("n", [Var(Param "i")]), Const(Strc "T")) in
  Rule(name, formula, statement)
in
AbsRule(rule_try, params)
);
(
let params = [Singledef("i", "Parameter")] in
let rule_crit =
  let name = "crit" in
  let formula =
    let f1 = Eqn(Var(Array("n", [Var(Param "i")])), Const(Strc "T")) in
    let f2 = Eqn(Var(Global "x"), Const(Strc "True")) in
    And [f1; f2]
  in
  let statement =
    let s1 = Assign(Array("n", [Var(Param "i")]), Const(Strc "C")) in
    let s2 = Assign(Global "x", Const(Strc "False")) in
    Parallel [s1; s2]
  in
  Rule(name, formula, statement)
in
AbsRule(rule_crit, params)
);
(
let params = [Singledef("i", "Parameter")] in
let rule_exit =
  let name = "exit" in
  let formula = Eqn(Var(Array("n", [Var(Param "i")])), Const(Strc "C")) in
  let statement = Assign(Array("n", [Var(Param "i")]), Const(Strc "E")) in
  Rule(name, formula, statement)
in
AbsRule(rule_exit, params)
);
(
let params = [Singledef("i", "Parameter")] in
let rule_idle =
  let name = "idle" in
  let formula = Eqn(Var(Array("n", [Var(Param "i")])), Const(Strc "E")) in
  let statement =
    let s1 = Assign(Global "x", Const(Strc("True"))) in
    let s2 = Assign(Array("n", [Var(Param "i")]), Const(Strc "I")) in
    Parallel [s1; s2]
  in
  Rule(name, formula, statement)
in
AbsRule(rule_idle, params)
);
]

let properties = [
(
let params = [Singledef("i", "Parameter"); Singledef("j", "Parameter")] in
let coherence =
  let name = "coherence" in
  let formula =
    let f1 = Eqn(Var(Array("n", [Var(Param "i")])), Const(Strc "C")) in
    let f2 = Eqn(Var(Array("n", [Var(Param "j")])), Const(Strc "C")) in
    And [f1; f2]
  in
  Prop(name, formula)
in
AbsProp(coherence, params)
);
]

let protocol = {
  types;
  vars;
  init;
  rules;
  properties;
};;

translate ~loach:protocol
