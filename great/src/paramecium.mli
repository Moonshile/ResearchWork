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

(*----------------------------- Module ToStr ----------------------------------*)

(** Module for translate to string *)
module ToStr : sig

  (** Translate to smt2 string *)
  module Smt2 : sig

    (** Translate to smt2 string

        @param types the type definitions of the protocol
        @param vardefs the variable definitions of the protocol
        @param form the formula to be translated
        @return the smt2 string
    *)
    val act : types:typedef list -> vardefs:vardef list -> form:formula -> string

  end

end

(*----------------------------- Module InvFinder ----------------------------------*)

(** Module for find invariants and causal relations *)
module InvFinder : sig

  (** Concrete rule

      + ConcreteRule: rule, concrete param list
  *)
  type concrete_rule =
    | ConcreteRule of rule * (string * const) list

  val concrete_rule : rule -> (string * const) list -> concrete_rule

  (** Concrete property

      + ConcreteProp: property, concrete param list
  *)
  type concrete_prop =
    | ConcreteProp of prop * (string * const) list

  val concrete_prop : prop -> (string * const) list -> concrete_prop

  (** Causal relations

    + InvHoldForRule1
    + InvHoldForRule2
    + InvHoldForRule3: the new concrete invariant found
  *)
  type relation =
    | InvHoldForRule1
    | InvHoldForRule2
    | InvHoldForRule3 of concrete_prop

  val invHoldForRule1 : relation
  val invHoldForRule2 : relation
  val invHoldForRule3 : concrete_prop -> relation

  (** InvFinder type, i.e., causal relation table *)
  type t = {
    rule: concrete_rule;
    inv: concrete_prop;
    relation: relation;
  }

  (** Find invs and causal relations of a protocol

      @param protocol the protocol
      @return causal relation table
  *)
  (*val find : protocol:protocol -> t list*)
end

