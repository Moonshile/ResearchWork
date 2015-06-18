(** Operations of formula based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Paramecium

(** Judge if a formula is tautology
    If negation of the formula is not satisfiable, then the formula is tautology

    @param filename is the temp file to store smt2 formula, default is "inv.smt2"
    @param quiet true (default) to prevent to print output of smt solver to screen
    @param types type definitions
    @param vardefs variable definitions
*)
val is_tautology : ?quiet:bool -> formula -> bool

val is_satisfiable : ?quiet:bool -> formula -> bool

(** Cast a formula to a list of formulae with and relation between them *)
val flat_and_to_list : formula -> formula list

(** Judge if tow formulae are symmetric *)
val form_are_symmetric : formula -> formula -> bool

(** Judge if f2 could be implied by f1 *)
val could_imply : formula -> formula -> bool

(** For andList, flat its all components,
    for others, flat to a single list
*)
val flat_to_andList : formula -> formula

(** Cast a formula to a list of formulae with or relation between them *)
val flat_or_to_list : formula -> formula list

(** For orList, flat its all components,
    for others, flat to a single list
*)
val flat_to_orList : formula -> formula

(** Simplify a formula *)
val simplify : formula -> formula

(** Raises when there are many parameter references more than range of its type *)
exception Too_many_parameters_of_same_type

(** Normalize a parameterized formula *)
val normalize : formula -> types:typedef list -> formula
