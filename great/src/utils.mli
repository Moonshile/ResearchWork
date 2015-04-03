(** This library provides some useful functions

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

(*----------------------------- Functions ----------------------------------*)

(** Generate all possible combinations for a specific set of list.
    i.e., generate Cartesian Production of the lists
    For example, given [[1;2]; [1;3]] produces [[1;1]; [1;3]; [2;1]; [2;3]]

    @param list the given set of list
    @return the generated combinations
*)
val combination : 'a list list -> 'a list list

(** Judge if all elements in list satisfy function f

    @param the list
    @param f a function maps elements in list to bool
    @return true if satisfy else false
*)
val all : 'a list -> f:('a -> bool) -> bool
