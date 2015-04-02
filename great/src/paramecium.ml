(** The most fundamental language for this tool

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

(*------------------------------ Types ---------------------------------*)

(** Constants *)
type const =
  | Intc of int
  | Strc of string
  | Boolc of bool

let intc i = Intc i
let strc s = Strc s
let boolc b = Boolc b

(** Basic types available, including integers and enumerations.
    Types are defined by their names and range.
*)
type typedef =
  | Enum of string * const list

let enum name values = Enum(name, values)

(** Index definitions
    + Indexdef, name and typename
*)
type indexdef =
  | Indexdef of string * string

let indexdef name typename = Indexdef(name, typename)

(** Index references
    + Index, name
*)
type index =
  | Index of string

let index name = Index name

(** Variable definitions, each with its name and name of its type
    + Array var: each with its name, names list of its array-types and name of its type
*)
type vardef =
  | Arraydef of string * indexdef list * string

let arraydef name indexdef typename = Arraydef(name, indexdef, typename)

(** Variable reference *)
type var =
  | Array of string * index list

let array name index = Array(name, index)

(** Represents expressions, including
    + Constants
    + Variable references
    + Condition expressions
*)
type exp =
  | Const of const
  | Var of var
  | Cond of formula * exp * exp
(** Boolean expressions, including
    + Boolean constants, Chaos as True, Miracle as false
    + Equation expression
    + Other basic logical operations, including negation,
      conjuction, disjuction, and implication
*)
and formula =
  | Chaos
  | Miracle
  | Eqn of exp * exp
  | Neg of formula
  | AndList of formula list
  | OrList of formula list
  | Imply of formula * formula

let const c = Const c
let var v = Var v
let cond f e1 e2 = Cond(f, e1, e2)

let chaos = Chaos
let miracle = Miracle
let eqn e1 e2 = Eqn(e1, e2)
let neg f = Neg f
let andList fs = AndList fs
let orList fs = OrList fs
let imply f1 f2 = Imply(f1, f2)

(** Assignment statements, including
    + Single assignment
    + Parallel assignment
*)
type statement =
  | Assign of var * exp
  | Parallel of statement list

let assign v e = Assign(v, e)
let parallel statements = Parallel statements

(** Represents rules which consists of guard and assignments *)
type rule = 
  | Rule of string * indexdef list * formula * statement

let rule name indexdef f s = Rule(name, indexdef, f, s)

(** Represents properties *)
type prop =
  | Prop of string * indexdef list * formula

let prop name indexdef f = Prop(name, indexdef, f)

(** Represents the whole protocol *)
type protocol = {
  types: typedef list;
  vardefs: vardef list;
  init: statement;
  rules: rule list;
  properties: prop list;
}
