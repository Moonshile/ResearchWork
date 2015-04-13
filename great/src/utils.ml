(** This library provides some useful functions

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Core.Std

(*----------------------------- Exceptions ----------------------------------*)

(* This exception is for stop warnings. It will never be raised. *)
exception Empty_exception

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
      read filedsr ((String.sub buf ~pos:0 ~len:size)::results)
  in
  String.concat (List.rev (read filedsr []))

(** Execute a program with some arguments then fetch the output.
    This function will block the main process.

    @param prog the program to be executed
    @param args arguments
    @return stdout string
*)
let exec ~prog ~args =
  let open Unix.Process_info in
  let sub = Unix.create_process ~prog ~args in
  (read_to_end sub.stdout, read_to_end sub.stderr)

(** Execute a program with some arguments and some input strings then fetch the output.
    This function will block the main process.

    @param prog the program to be executed
    @param args arguments
    @param input input string
    @return stdout string
*)
let exec_with_input ~prog ~args input =
  let open Unix.Process_info in
  let sub = Unix.create_process ~prog ~args in
  let size = Unix.write sub.stdin ~buf:input in
  if size = 0 then
    raise Empty_exception
  else
    (read_to_end sub.stdout, read_to_end sub.stderr)

(** Some usefule colorful print functions *)
module Prt = struct

  (** Wrong color value *)
  exception Wrong_color

  (** Basic color *)
  type basic_color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  (** More color
      + Basic basic color
      + Bold bold color
      + RGB 6x6x6 color cube
      + Gray 24 grayscale levels
  *)
  type color =
    | Basic of basic_color
    | Bold of basic_color
    | RGB of int * int * int
    | Gray of int

  let basic c = Basic c
  let bold c = Bold c
  let rgb r g b =
    let in_range i = i >= 0 && i < 6 in
    if in_range r && in_range g && in_range b then
      RGB (r, g, b)
    else
      raise Wrong_color
  let gray i = if i >=0 && i < 24 then Gray i else raise Wrong_color

  (* Cast basic color to integer representation in terminal color *)
  let basic_color_to_int = function
    | Black -> 0
    | Red -> 1
    | Green -> 2
    | Yellow -> 3
    | Blue -> 4
    | Magenta -> 5
    | Cyan -> 6
    | White -> 7

  (* Cast all color to integer *)
  let color_to_int = function
    | Basic basic_color -> basic_color_to_int basic_color
    | Bold basic_color -> 8 + basic_color_to_int basic_color
    | RGB (r, g, b) -> 16 + b + 6*g + 36*r
    | Gray i -> 232 + i

  (** Print colorful text on terminal

      @param text string to be printed
      @param color color of string
  *)
  let colorful ~text ~color =
    printf "\027[38;5;%dm%s\027[0m" (color_to_int color) text

  (** Print info string *)
  let info text =
    colorful ~text ~color:(basic Cyan)

  (** Print warning string *)
  let warning text =
    colorful ~text ~color:(basic Yellow)

  (** Print error string *)
  let error text =
    colorful ~text ~color:(basic Red)

end