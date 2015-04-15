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
val is_tautology : ?filename:string -> ?quiet:bool -> types:typedef list -> vardefs:vardef list -> formula -> bool

(** For andList, flat its all components,
    for others, flat to a single list
*)
val flat_to_andList : formula -> formula

(** For orList, flat its all components,
    for others, flat to a single list
*)
val flat_to_orList : formula -> formula

(** Judge if tow formulae are symmetric *)
val form_are_symmetric : formula -> formula -> bool
