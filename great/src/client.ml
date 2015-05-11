(** Client for connect to smv/smt2 server

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Core.Std;;
open Utils;;

let host = UnixLabels.inet_addr_of_string "192.168.1.204"

let port = 50008

type request_type =
  | ERROR
  | WAITING
  | OK
  | COMPUTE_REACHABLE
  | QUERY_REACHABLE
  | CHECK_INV
  | SMV_QUIT
  | SET_SMT2_CONTEXT
  | QUERY_SMT2
  | QUERY_STAND_SMT2

let request_type_to_str rt =
  match rt with
  | ERROR -> "-2"
  | WAITING -> "-1"
  | OK -> "0"
  | COMPUTE_REACHABLE -> "1"
  | QUERY_REACHABLE -> "2"
  | CHECK_INV -> "3"
  | SMV_QUIT -> "7"
  | SET_SMT2_CONTEXT -> "4"
  | QUERY_SMT2 -> "5"
  | QUERY_STAND_SMT2 -> "6"

let str_to_request_type str =
  match str with
  | "-2" -> ERROR
  | "-1" -> WAITING
  | "0" -> OK
  | "1" -> COMPUTE_REACHABLE
  | "2" -> QUERY_REACHABLE
  | "3" -> CHECK_INV
  | "7" -> SMV_QUIT
  | "4" -> SET_SMT2_CONTEXT
  | "5" -> QUERY_SMT2
  | "6" -> QUERY_STAND_SMT2
  | _ -> raise Empty_exception

let make_request str =
  let sock = Unix.socket UnixLabels.PF_INET UnixLabels.SOCK_STREAM 0 in
  let res = String.make 1024 '\000' in
  Unix.connect sock (UnixLabels.ADDR_INET(host, port));
  let _writed = Unix.write sock ~buf:str in
  let len = Unix.read sock ~buf:res in
  String.sub res ~pos:0 ~len

let command_id = ref 0

let request cmd req_str =
  let cmd  = request_type_to_str cmd in
  let cmd_id = !command_id in
  let req = sprintf "%s,%d,%s" cmd cmd_id req_str in
  incr command_id; printf "%d\n" (!command_id);
  make_request req

module Smv = struct
  
  let compute_reachable name content =
    request COMPUTE_REACHABLE (sprintf "%s,%s" name content)

  let query_reachable name =
    request QUERY_REACHABLE name

  let check_inv name inv =
    request CHECK_INV (sprintf "%s,%s" name inv)

  let quit name =
    request SMV_QUIT name

end

module Smt2 = struct

  let set_context name context =
    request SET_SMT2_CONTEXT (sprintf "%s,%s" name context)

  let check name f =
    request QUERY_SMT2 (sprintf "%s,%s" name f)

  let check_stand context f =
    request QUERY_STAND_SMT2 (sprintf "%s,%s" context f)

end

