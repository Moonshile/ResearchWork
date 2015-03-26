(** The most fundamental language for this tool

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

(*------------------------------ Types ---------------------------------*)

(** Basic types available, including integers and enumerations *)
type basic_type =
  | IntEnum of int list
  | StrEnum of string list

(** Constants *)
type const =
  | Intc of int
  | Strc of string

(** Variables *)
type var =
  | Global of string * basic_type

(** Represents expressions, including
    + Constans of basic_types
    + Global variables of basic_types, with their own names
    + Condition expressions
*)
type exp =
  | Const of const
  | Var of var
  | Cond of formula * exp * exp
(** Boolean expressions, including
    + Boolean constants
    + Equation expression
    + Other basic logical operations, including negation,
      conjuction, disjuction, and implication
*)
and formula =
  | True
  | False
  | Eqn of exp * exp
  | Neg of formula
  | And of formula list
  | Or of formula list
  | Imply of formula * formula

(** Assignment statements, including
    + Single assignment
    + Parallel assignment
*)
type statement =
  | Assign of var * exp
  | Parallel of statement list

(** Represents rules which consists of guard and assignments *)
type rule = 
  | Rule of string * formula * statement

(** Represents properties *)
type prop =
  | Prop of string * formula

(** Represents the whole protocol *)
type protocol = {
  init: statement;
  rules: rule list;
  properties: prop list;
}
