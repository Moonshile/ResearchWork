(** Language for cache coherence protocols in support of
    parameterization and local variables

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Core.Std

open Utils

(*------------------------------ Types ---------------------------------*)

(** Parameters for variables, rules, invariants, and so on *)
type param =
  | Param of string * int list

(** Basic types available, including integers and enumerations *)
type basic_type =
  | IntEnum of int list
  | StrEnum of string list

(** Constants *)
type const =
  | Intc of int
  | Strc of string

(** Variables
    + Global variables
    + Local variables
    + Parameterized variables
*)
type var =
  | Global of string * basic_type
  | Local of string
  | Paramed of string * param
(** Represents expressions, including
    + Constans of basic_types
    + Global variables of basic_types, with their own names
    + Condition expressions
*)
and exp =
  | Const of const
  | Var of var
  | Cond of formula * exp * exp
(** Boolean expressions, including
    + Boolean constants
    + Equation expression
    + Other basic logical operations, including negation,
      conjuction, disjuction, and implication
    + Forall, i.e., true for all formulae parameterized by params
    + Exists, i.e., exists a true in all formulae parameterized
*)
and formula =
  | True
  | False
  | Eqn of exp * exp
  | Neg of formula
  | And of formula list
  | Or of formula list
  | Imply of formula * formula
  | Forall of param * formula
  | Exists of param * formula

(** Assignment statements, including
    + Single assignment
    + Parallel assignment
    + For statement, i.e., for all assignment parameterized, do sth
    + If statement, equal to `if <condition> do <statement> end`
    + Ifelse statement, equal to 
      `if <condition> do <statement> else do <statement> end`
*)
type statement =
  | Assign of var * exp
  | Parallel of statement list
  | For of param * statement
  | If of formula * statement
  | Ifelse of formula * statement * statement

(** Represents rules which consists of guard and assignments *)
type rule =
  | Rule of string * formula * statement
  | ParamedRule of string * param * formula * statement

(** Represents properties which could be parameterized *)
type prop =
  | Prop of string * formula
  | ParamedProp of string * param * formula

(** Represents the whole protocol *)
type protocol = {
  init: statement;
  rules: rule list;
  properties: prop list;
}

(*----------------------------- Exceptions ----------------------------------*)

exception Wrong_parameter

(*----------------------------- Functions ---------------------------------*)

(** Translate language of Loach to Paramecium

    @param loach cache coherence protocol written in Loach
    @return the protocol in Paramecium
*)
val translate : loach:protocol -> Paramecium.protocol
