(** This library provides some useful functions

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Core.Std

(** Generate all possible combinations for a specific set of lists.
    i.e., generate Cartesian Production of the lists
    For example, given [[1;2]; [1;3]] produces [[1;1]; [1;3]; [2;1]; [2;3]]

    @param list the given set of lists, whose elements will be omitted if it is []
    @return the generated combinations
*)
let combination list =
  let append_all alist ele = List.map ~f:(fun x -> x@[ele]) alist in
  let com_next res b =
    match (res, b) with
    | (res, []) -> res
    | ([], b) -> List.map ~f:(fun x -> [x]) b
    | _ -> List.concat (List.map ~f:(append_all res) b)
  in
  List.fold ~init:[] ~f:com_next list

(** Judge if all elements in list satisfy function f

    @param the list
    @param f a function maps elements in list to bool
    @return true if satisfy else false
*)
let all list ~f =
  list
  |> List.map ~f
  |> List.fold ~f:(fun res x -> res && x) ~init:true

