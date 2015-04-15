(** Generalize a concrete formula based on Paramecium to parameterized format

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Paramecium

open Core.Std

type t =
  | Paraminfo of paramdef list * (string * paramref) list

val paraminfo : paramdef list -> (string * paramref) list -> t

(** Convert paramref *)
val paramref_act : paramref -> t -> paramref * t

(** Convert a list of components *)
val components_act : 'a list -> 'b -> ('a -> 'b -> 'c * 'b) -> 'c list * 'b

(** Convert var *)
val var_act : var -> t -> var * t

(** Convert exp *)
val exp_act : exp -> t -> exp * t

(** Convert formula *)
val form_act : formula -> t -> formula * t
