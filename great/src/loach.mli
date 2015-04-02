(** Language for cache coherence protocols in support of
    parameterization and local variables

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Core.Std

(*------------------------------ Types ---------------------------------*)

(** Constants *)
type const =
  | Intc of int
  | Strc of string
  | Boolc of bool

val intc : int -> const
val strc : string -> const
val boolc : bool -> const

(** Basic types available, including integers and enumerations.
    Types are defined by their names and range.
*)
type typedef =
  | Enum of string * const list

val enum : string -> const list -> typedef

(** Parameter definitions
    + paramdef, name and typename
*)
type paramdef =
  | Paramdef of string * string

val paramdef : string -> string -> paramdef

(** Parameter references
    + Paramref, name
*)
type paramref =
  | Paramref of string

val paramref : string -> paramref

(** Index definitions, for abstract components
    + Indexdef: name, type name
*)
type indexdef =
  | Indexdef of string * string

val indexdef : string -> string -> indexdef

(** Index reference
    + Indexref: name
*)
type indexref =
  | Indexref of string

val indexref : string -> indexref

(** Variable definitions, each with its name and name of its type
    + Arraydef: name, param definitions, index definitions, type name
    + Singledef: name, type name
*)
type vardef =
  | Arraydef of string * paramdef list * indexdef list * string
  | Singledef of string * string

val arraydef : string -> paramdef list -> indexdef list -> string -> vardef
val singledef : string -> string -> vardef

(** Variable reference
    + Array: name, paramref, indexref
    + Single: name
*)
type var =
  | Array of string * paramref list * indexref list
  | Single of string

val array : string -> paramref list -> indexref list -> var
val single : string -> var

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

val const : const -> exp
val var : var -> exp
val cond : formula -> exp -> exp -> exp
val param : paramref -> exp
val index : indexref -> exp

val chaos : formula
val miracle : formula
val eqn : exp -> exp -> formula
val neg : formula -> formula
val andList : formula list -> formula
val orList : formula list -> formula
val imply : formula -> formula -> formula
val absForm : formula -> indexdef list -> formula

(** Assignment statements, including
    + Single assignment
    + Parallel assignment
    + AbsStatement: a set of statements which has Parallel relation
*)
type statement =
  | Assign of var * exp
  | Parallel of statement list
  | AbsStatement of statement * indexdef list

val assign : var -> exp -> statement
val parallel : statement list -> statement
val absStatement : statement -> indexdef list -> statement

(** Represents rules which consists of guard and assignments
    + Rule: name, parameters, guard, assignments
    + AbsRule: a list of rules
*)
type rule = 
  | Rule of string * paramdef list * formula * statement
  | AbsRule of rule * indexdef list

val rule : string -> paramdef list -> formula -> statement -> rule
val absRule : rule -> indexdef list -> rule

(** Represents properties
    + Prop: name, parameters, formula
    + AbsProp: a list of properties
*)
type prop =
  | Prop of string * paramdef list * formula
  | AbsProp of prop * indexdef list

val prop : string -> paramdef list -> formula -> prop
val absProp : prop -> indexdef list -> prop

(** Represents the whole protocol *)
type protocol = {
  types: typedef list;
  vardefs: vardef list;
  init: statement;
  rules: rule list;
  properties: prop list;
}

(*----------------------------- Exceptions ----------------------------------*)


(*----------------------------- Functions ----------------------------------*)

(** Convert a int list to const list *)
val int_consts : int list -> const list
(** Convert a string list to const list *)
val str_consts : string list -> const list
(** Convert a boolean list to const list *)
val bool_consts : bool list -> const list

(*----------------------------- Translate module ---------------------------------*)


