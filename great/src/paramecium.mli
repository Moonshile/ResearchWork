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

(** Parameter definitions
    + paramdef, name and typename
*)
type paramdef =
  | Paramdef of string * string

val paramdef : string -> string -> paramdef

(** Parameter references
    + Paramref, name
    + Paramfix, value
*)
type paramref =
  | Paramref of string
  | Paramfix of const

val paramref : string -> paramref
val paramfix : const -> paramref

(** Variable definitions, each with its name and name of its type
    + Array var: name, param definitions, type name
*)
type vardef =
  | Arrdef of string * paramdef list * string

val arrdef : string -> paramdef list -> string -> vardef

(** Variable reference *)
type var =
  | Arr of string * paramref list

val arr : string -> paramref list -> var

(** Represents expressions, including
    + Constants
    + Variable references
    + Condition expressions
    + Parameter
*)
type exp =
  | Const of const
  | Var of var
  | Cond of formula * exp * exp
  | Param of paramref
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
val param : paramref -> exp

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

(** Represents rules which consists of guard and assignments
    + Rule: name, parameters, guard, assignments
*)
type rule = 
  | Rule of string * paramdef list * formula * statement

val rule : string -> paramdef list -> formula -> statement -> rule

(** Represents properties
    + Prop: name, parameters, formula
*)
type prop =
  | Prop of string * paramdef list * formula

val prop : string -> paramdef list -> formula -> prop

(** Represents the whole protocol *)
type protocol = {
  types: typedef list;
  vardefs: vardef list;
  init: statement;
  rules: rule list;
  properties: prop list;
}

(*----------------------------- Exceptions ----------------------------------*)

(** The actual parameters can't match with their definitions *)
exception Unmatched_parameters

(** Unexhausted instantiation
    This exception should never be raised. Once raised, There should be a bug in this tool.
*)
exception Unexhausted_inst

(*----------------------------- Functions ----------------------------------*)

(** Find the letues range of a type by its name *)
val name2type : tname:string -> types:typedef list -> const list

(** attach consts i to string name *)
val attach_list : string -> const list -> string

(** Apply a paramref with param, i.e., cast it to consts *)
val apply_paramref : paramref -> param:(string * const) list -> paramref

(** Apply array with param *)
val apply_array : var -> param:(string * const) list -> var

(** Apply exp with param *)
val apply_exp : exp -> param:(string * const) list -> exp

(** Apply formula with param *)
val apply_form : formula -> param:(string * const) list -> formula

(** Apply statement with param *)
val apply_statement : statement -> param:(string * const) list -> statement

(** Apply rule with param *)
val apply_rule : rule -> param:(string * const) list -> rule

(** Apply property with param *)
val apply_prop : prop -> param:(string * const) list -> prop

