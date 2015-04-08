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

(** Judge if any elements in list satisfies function f

    @param the list
    @param f a function maps elements in list to bool
    @return true if satisfies else false
*)
let any list ~f =
  list
  |> List.map ~f
  |> List.fold ~f:(fun res x -> res || x) ~init:false

(** Denotes there are errors while execute a program *)
exception Exec_error

(* Generate a new string buffer with size *)
let new_str_buf size =
  let buf = String.create size in
  String.fill buf ~pos:0 ~len:size '\000';
  buf

(* Read all contents in a filedsr *)
let read_to_end filedsr =
  let rec read filedsr results =
    let buf = new_str_buf 1024 in
    let size = Unix.read filedsr ~buf in
    if size = 0 then
      results
    else
      read filedsr ((String.sub buf 0 size)::results)
  in
  String.concat (List.rev (read filedsr []))

(** Execute a program with some arguments then fetch the stdout.
    This function will block the main process.

    @param prog the program to be executed
    @param args arguments
    @return stdout string
*)
let exec ~prog ~args =
  let open Unix.Process_info in
  let sub = Unix.create_process ~prog ~args in
  read_to_end sub.stdout
