(** Operations of formula based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Paramecium
open Smt
open Core.Std

(** Judge if a formula is tautology
    If negation of the formula is not satisfiable, then the formula is tautology

    @param filename is the temp file to store smt2 formula, default is "inv.smt2"
    @param quiet true (default) to prevent to print output of smt solver to screen
    @param types type definitions
    @param vardefs variable definitions
*)
let is_tautology ?(filename="inv.smt2") ?(quiet=true) ~types ~vardefs form =
  not (is_satisfiable ~filename ~quiet (ToStr.Smt2.act ~types ~vardefs (neg form)))

(** Cast a formula to a list of formulae with and relation between them *)
let rec flat_and_to_list form =
  match form with
  | AndList(fl) -> List.concat (List.map fl ~f:flat_and_to_list)
  | Chaos
  | Miracle
  | Eqn(_)
  | Neg(_)
  | OrList(_)
  | Imply(_) -> [form]

(** For andList, flat its all components,
    for others, flat to a single list
*)
let flat_to_andList form =
  andList (flat_and_to_list form)

(** Cast a formula to a list of formulae with or relation between them *)
let rec flat_or_to_list form =
  match form with
  | OrList(fl) -> List.concat (List.map fl ~f:flat_or_to_list)
  | Chaos
  | Miracle
  | Eqn(_)
  | Neg(_)
  | AndList(_)
  | Imply(_) -> [form]

(** For orList, flat its all components,
    for others, flat to a single list
*)
let flat_to_orList form =
  orList (flat_or_to_list form)

(** Judge if tow formulae are symmetric *)
(* TODO what if variables of the formulae have chaos sequence? *)
let form_are_symmetric f1 f2 =
  let param_info  = Generalize.paraminfo [] [] in
  let (f1', _) = Generalize.form_act f1 param_info in
  let (f2', _) = Generalize.form_act f2 param_info in
  f1' = f2'
