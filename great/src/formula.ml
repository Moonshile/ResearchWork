(** Operations of formula based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Paramecium

open Core.Std

(** For andList, flat its all components,
    for others, flat to a single list
*)
let flat_to_andList form =
  let rec to_list form =
    match form with
    | AndList(fl) -> List.concat (List.map fl ~f:to_list)
    | Chaos
    | Miracle
    | Eqn(_)
    | Neg(_)
    | OrList(_)
    | Imply(_) -> [form]
  in
  andList (to_list form)

(** For orList, flat its all components,
    for others, flat to a single list
*)
let flat_to_orList form =
  let rec to_list form =
    match form with
    | OrList(fl) -> List.concat (List.map fl ~f:to_list)
    | Chaos
    | Miracle
    | Eqn(_)
    | Neg(_)
    | AndList(_)
    | Imply(_) -> [form]
  in
  orList (to_list form)
