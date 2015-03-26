open Loach

(* Self-defined types *)
let state_type = StrEnum(["I"; "T"; "C"; "E"])
let bool_type = StrEnum(["True"; "False"])
let para_type = IntEnum([1; 2; 3])

(* Initialization *)
let init =
  let a1 = Assign(Global("x", bool_type), Const(Strc("True"))) in
  let a2 = Assign(Global("n1", state_type), Const(Strc("I"))) in
  let a3 = Assign(Global("n2", state_type), Const(Strc("I"))) in
  let a4 = Assign(Global("n3", state_type), Const(Strc("I"))) in
  Parallel [a1; a2; a3; a4]


