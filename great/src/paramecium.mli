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

val intc : int -> const
val strc : string -> const
val boolc : bool -> const

(** Basic types available, including integers and enumerations.
    Types are defined by their names and range.
*)
type typedef =
  | Enum of string * const list

val enum : string -> const list -> typedef

(** Index definitions
    + Indexdef, name and typename
*)
type indexdef =
  | Indexdef of string * string

val indexdef : string -> string -> indexdef

(** Index references
    + Index, name
*)
type index =
  | Index of string

val index : string -> index

(** Variable definitions, each with its name and name of its type
    + Array var: each with its name, names list of its array-types and name of its type
*)
type vardef =
  | Arraydef of string * indexdef list * string

val arraydef : string -> indexdef list -> string -> vardef

(** Variable reference *)
type var =
  | Array of string * index list

val array : string -> index list -> var

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

val const : const -> exp
val var : var -> exp
val cond : formula -> exp -> exp -> exp

val chaos : formula
val miracle : formula
val eqn : exp -> exp -> formula
val neg : formula -> formula
val andList : formula list -> formula
val orList : formula list -> formula
val imply : formula -> formula -> formula

(** Assignment statements, including
    + Single assignment
    + Parallel assignment
*)
type statement =
  | Assign of var * exp
  | Parallel of statement list

val assign : var -> exp -> statement
val parallel : statement list -> statement

(** Represents rules which consists of guard and assignments *)
type rule = 
  | Rule of string * indexdef list * formula * statement

val rule : string -> indexdef list -> formula -> statement -> rule

(** Represents properties *)
type prop =
  | Prop of string * indexdef list * formula

val prop : string -> indexdef list -> formula -> prop

(** Represents the whole protocol *)
type protocol = {
  types: typedef list;
  vardefs: vardef list;
  init: statement;
  rules: rule list;
  properties: prop list;
}
