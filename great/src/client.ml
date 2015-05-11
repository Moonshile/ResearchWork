(** Client for connect to smv/smt2 server

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Core.Std;;
open Utils;;

let host = UnixLabels.inet_addr_of_string "192.168.1.204"

let port = 50008

type requst_type =
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

let requst_type_to_str rt =
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

let command_id = ref 0;

module Smv = struct
  
  let compute_reachable name content =
    let cmd = requst_type_to_str COMPUTE_REACHABLE in
    let cmd_id = !command_id in
    let req = sprintf "%s,%d,%s,%s" cmd cmd_id name content in
    incr command_id; printf "%d" (!command_id);
    make_request req

  let query_reachable name =
    let cmd = requst_type_to_str QUERY_REACHABLE in
    let cmd_id = !command_id in
    let req = sprintf "%s,%d,%s" cmd cmd_id name in
    incr command_id; printf "%d" (!command_id);
    make_request req

  let check_inv name inv =
    let cmd = requst_type_to_str CHECK_INV in
    let cmd_id = !command_id in
    let req = sprintf "%s,%d,%s,%s" cmd cmd_id name inv in
    incr command_id; printf "%d" (!command_id);
    make_request req

  let quit name =
    let cmd = requst_type_to_str SMV_QUIT in
    let cmd_id = !command_id in
    let req = sprintf "%s,%d,%s" cmd cmd_id name in
    incr command_id; printf "%d" (!command_id);
    make_request req

end

module Smt2 = struct

  let set_context name context =
    let cmd = requst_type_to_str SET_SMT2_CONTEXT in
    let cmd_id = !command_id in
    let req = sprintf "%s,%d,%s,%s" cmd cmd_id name context in
    incr command_id; printf "%d" (!command_id);
    make_request req

  let check name f =
    let cmd = requst_type_to_str QUERY_SMT2 in
    let cmd_id = !command_id in
    let req = sprintf "%s,%d,%s,%s" cmd cmd_id name f in
    incr command_id; printf "%d" (!command_id);
    make_request req

  let check_stand context f =
    let cmd = requst_type_to_str QUERY_STAND_SMT2 in
    let cmd_id = !command_id in
    let req = sprintf "%s,%d,%s,%s" cmd cmd_id context f in
    incr command_id; printf "%d" (!command_id);
    make_request req

end

