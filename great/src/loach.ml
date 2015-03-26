(** Language for cache coherence protocols in support of
    parameterization and local variables

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Core.Std

open Utils

(*------------------------------ Types ---------------------------------*)

(** Basic types available, including integers and enumerations.
    Types are defined by their names and range.
*)
type typedef =
  | IntEnum of string * int list
  | StrEnum of string * string list

(** Variable definitions
    + Var: each with its name and name of its type
    + Array var: each with its name, names list of its array-types and name of its type
*)
type vardef =
  | Vardef of string * string
  | Arraydef of string * string list * string

(** Constants *)
type const =
  | Intc of int
  | Strc of string

(** Variables
    + Global variables referenced by name
    + Parameterized variables referenced by name and actual parameters
*)
type var =
  | Global of string
  | Array of string * exp list
  | Param of string * exp list
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
    + Abstract formula, which is parameterized its definition with parameters
*)
and formula =
  | True
  | False
  | Eqn of exp * exp
  | Neg of formula
  | And of formula list
  | Or of formula list
  | Imply of formula * formula
  | AbsForm of formula * vardef list

(** Assignment statements, including
    + Single assignment
    + Parallel assignment
    + Abstract statement, which is parameterized its definition with parameters
*)
type statement =
  | Assign of var * exp
  | Parallel of statement list
  | AbsAssign of statement * vardef list

(** Represents rules which consists of guard and assignments
    + Rule with its name, guard and assigments
    + Abstract rule, which is parameterized its definition with parameters
*)
type rule = 
  | Rule of string * formula * statement
  | AbsRule of rule * vardef list

(** Represents properties
    + Property with its name and formula
    + Abstract property, which is parameterized its definition with parameters
*)
type prop =
  | Prop of string * formula
  | AbsProp of prop * vardef list

(** Represents the whole protocol *)
type protocol = {
  types: typedef list;
  vars: var list;
  init: statement list;
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
let translate ~loach:{types; vars; init; rules; properties} =
  let state_type = Paramecium.StrEnum("state", ["I"; "T"; "C"; "E"]) in
  { Paramecium.types = [state_type];
    vars = [];
    init = [Assign(Global("x"), Const(Strc("I")))];
    rules = [];
    properties = [];
  }
