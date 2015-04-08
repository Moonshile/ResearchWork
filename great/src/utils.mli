(** This library provides some useful functions

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

(*----------------------------- Exceptions ----------------------------------*)

(* This exception is for stop warnings. It will never be raised. *)
exception Empty_exception

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

(** Judge if any elements in list satisfies function f

    @param the list
    @param f a function maps elements in list to bool
    @return true if satisfies else false
*)
val any : 'a list -> f:('a -> bool) -> bool

(** Denotes there are errors while execute a program *)
exception Exec_error

(** Execute a program with some arguments then fetch the stdout.
    This function will block the main process.

    @param prog the program to be executed
    @param args arguments
    @return stdout string
*)
val exec : prog:string -> args:string list -> string
