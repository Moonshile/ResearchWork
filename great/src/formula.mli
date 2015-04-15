(** Operations of formula based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Paramecium

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
