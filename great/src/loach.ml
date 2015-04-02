(** Language for cache coherence protocols in support of
    parameterization and local variables

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)
open Utils

open Core.Std

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

let enum name letues = Enum(name, letues)

(** Parameter definitions
    + paramdef, name and typename
*)
type paramdef =
  | Paramdef of string * string

let paramdef name typename = Paramdef(name, typename)

(** Parameter references
    + Paramref, name
*)
type paramref =
  | Paramref of string

let paramref name = Paramref name

(** Index definitions, for abstract components
    + Indexdef: name, type name
*)
type indexdef =
  | Indexdef of string * string

let indexdef name tname = Indexdef(name, tname)

(** Index reference
    + Indexref: name
*)
type indexref =
  | Indexref of string

let indexref name = Indexref name

(** Variable definitions, each with its name and name of its type
    + Arraydef: name, param definitions, index definitions, type name
    + Singledef: name, type name
*)
type vardef =
  | Arraydef of string * paramdef list * indexdef list * string
  | Singledef of string * string

let arraydef name paramdef indexdef typename = Arraydef(name, paramdef, indexdef, typename)
let singledef name typename = Singledef(name, typename)

(** Variable reference
    + Array: name, paramref, indexref
    + Single: name
*)
type var =
  | Array of string * paramref list * indexref list
  | Single of string

let array name paramref indexref = Array(name, paramref, indexref)
let single name = Single name

(** Represents expressions, including
    + Constants
    + Variable references
    + Condition expressions
    + Parameter
    + Index
*)
type exp =
  | Const of const
  | Var of var
  | Cond of formula * exp * exp
  | Param of paramref
  | Index of indexref
(** Boolean expressions, including
    + Boolean constants, Chaos as True, Miracle as false
    + Equation expression
    + Other basic logical operations, including negation,
      conjuction, disjuction, and implication
    + Abstract formulae, a set of formulae which has conjunction relation
*)
and formula =
  | Chaos
  | Miracle
  | Eqn of exp * exp
  | Neg of formula
  | AndList of formula list
  | OrList of formula list
  | Imply of formula * formula
  | AbsForm of formula * indexdef list

let const c = Const c
let var v = Var v
let cond f e1 e2 = Cond(f, e1, e2)
let param paramref = Param paramref
let index indexref = Index indexref

let chaos = Chaos
let miracle = Miracle
let eqn e1 e2 = Eqn(e1, e2)
let neg f = Neg f
let andList fs = AndList fs
let orList fs = OrList fs
let imply f1 f2 = Imply(f1, f2)
let absForm f indexdefs = AbsForm(f, indexdefs)

(** Assignment statements, including
    + Single assignment
    + Parallel assignment
    + AbsStatement: a set of statements which has Parallel relation
*)
type statement =
  | Assign of var * exp
  | Parallel of statement list
  | AbsStatement of statement * indexdef list

let assign v e = Assign(v, e)
let parallel statements = Parallel statements
let absStatement s indexdefs = AbsStatement(s, indexdefs)

(** Represents rules which consists of guard and assignments
    + Rule: name, parameters, guard, assignments
    + AbsRule: a list of rules
*)
type rule = 
  | Rule of string * paramdef list * formula * statement
  | AbsRule of rule * indexdef list

let rule name paramdef f s = Rule(name, paramdef, f, s)
let absRule r indexdefs = AbsRule(r, indexdefs)

(** Represents properties
    + Prop: name, parameters, formula
    + AbsProp: a list of properties
*)
type prop =
  | Prop of string * paramdef list * formula
  | AbsProp of prop * indexdef list

let prop name paramdef f = Prop(name, paramdef, f)
let absProp p indexdefs = AbsProp(p, indexdefs)

(** Represents the whole protocol *)
type protocol = {
  types: typedef list;
  vardefs: vardef list;
  init: statement;
  rules: rule list;
  properties: prop list;
}

(*----------------------------- Exceptions ----------------------------------*)

exception Wrong_index
exception Wrong_function_call

(*----------------------------- Functions ----------------------------------*)

(** Convert a int list to const list *)
let int_consts ints = List.map ints ~f:intc
(** Convert a string list to const list *)
let str_consts strs = List.map strs ~f:strc
(** Convert a boolean list to const list *)
let bool_consts bools = List.map bools ~f:boolc

(** Find the letues range of a type by its name

    @param name: name of the type
    @param types: list of type definitions
    @return the concrete type, i.e., const list
*)
let name2type ~tname ~types =
  let Enum(_, consts) = List.find_exn types ~f:(fun (Enum(n, _)) -> n = tname) in
  consts

(* Generate combination of all possible values of a `indexdef` set *)
let combine_params indexdefs types =
  indexdefs
  |> List.map ~f:(fun (Indexdef(_, tname)) -> name2type ~tname ~types)
  |> combination

