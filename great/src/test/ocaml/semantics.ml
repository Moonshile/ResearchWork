
(* This module is to model semantics of cache protocols *)
module CacheProtocol =
  struct
    (* Variable type consists of global variables which are visible to all nodes *)
    (* and variables parameterized by node id which are visible to the node *)
    type var = Global of string | Param of string * int

    (* Expressions here are only simple expressions, consists of variables, constants, and *)
    (* conditional expressions. *)
    (* There is no more expression with other operators for arithmetical calculation *)
    type exp =
        Var of var
      | Const of int
      | Cond of formula * exp * exp
    (* Formulae are for logical operation *)
    and formula =
        (* equation *)
      | Eqn of exp * exp
        (* negation *)
      | Neg of formula
      | And of formula list
      | Or of formula list
      | Imply of formula * formula
      | Forall of int * (int -> formula)
      | Exist of int * (int -> formula)
        (* True and False value in cache protocols *)
      | CTrue
      | CFalse

    (* Statements are assigments *)
    type statement =
        Assign of var * exp
        (* A set of assignments which will be executed parallel *)
      | Parallel of statement list
        (* Parameterized assignments *)
      | For of int * (int -> statement)

    (* Transition rules in cache protocols *)
    type rule = Guard of formula * statement
  end;;

