open Loach

(* Self-defined types *)
let types = [
  StrEnum("state", ["I"; "T"; "C"; "E"]);
  StrEnum("bool", ["True"; "False"]);
  IntEnum("Parameter", [1; 2; 3]);
]

(* Variables *)
let vars = [
  Vardef("x", "bool");
  Vardef("n1", "state");
  Vardef("n2", "state");
  Vardef("n3", "state");
]

(* Initialization *)
let init =
  let a1 = Assign(Global("x"), Const(Strc("True"))) in
  let a2 = Assign(Global("n1"), Const(Strc("I"))) in
  let a3 = Assign(Global("n2"), Const(Strc("I"))) in
  let a4 = Assign(Global("n3"), Const(Strc("I"))) in
  Parallel [a1; a2; a3; a4]


