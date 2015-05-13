(** Client for connect to smv/smt2 server

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Core.Std;;
open Utils;;

exception Server_exception

let host = UnixLabels.inet_addr_of_string "127.0.0.1"

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
  let sock = Unix.socket ~domain:UnixLabels.PF_INET ~kind:UnixLabels.SOCK_STREAM ~protocol:0 in
  let res = String.make 1024 '\000' in
  Unix.connect sock ~addr:(UnixLabels.ADDR_INET(host, port));
  let _writed = Unix.write sock ~buf:str in
  let len = Unix.read sock ~buf:res in
  Unix.close sock;
  String.sub res ~pos:0 ~len

let command_id = ref 0

let request cmd req_str =
  let cmd  = request_type_to_str cmd in
  let cmd_id = !command_id in
  let req = sprintf "%s,%d,%s" cmd cmd_id req_str in
  let wrapped = sprintf "%d,%s" (String.length req) req in
  incr command_id; (*printf "%d\n" (!command_id);*)
  let res = String.split (make_request wrapped) ~on:',' in
  match res with
  | [] -> raise Empty_exception
  | status::res' -> 
    let s = str_to_request_type status in
    if s = ERROR then raise Server_exception
    else begin (s, res') end

module Smv = struct
  
  let compute_reachable name content =
    let (status, _) = request COMPUTE_REACHABLE (sprintf "%s,%s" name content) in
    status = OK

  let query_reachable name =
    let (status, diameter) = request QUERY_REACHABLE name in
    if status = OK then 
      match diameter with
      | "-1"::[] -> raise Server_exception
      | d::[] -> Int.of_string d
      | _ -> raise Server_exception
    else begin 0 end

  let check_inv name inv =
    let (_, res) = request CHECK_INV (sprintf "%s,%s" name inv) in
    match res with
    | r::[] -> Bool.of_string r
    | _ -> raise Server_exception

  let quit name =
    let (s, _) = request SMV_QUIT name in
    s = OK

end

module Smt2 = struct

  let set_context name context =
    let (s, _) = request SET_SMT2_CONTEXT (sprintf "%s,%s" name context) in
    s = OK

  let check name f =
    let (_, res) = request QUERY_SMT2 (sprintf "%s,%s" name f) in
    match res with
    | r::[] ->
      if r = "unsat" then false
      else if r = "sat" then true
      else raise Server_exception
    | _ -> raise Server_exception

  let check_stand context f =
    let (_, res) = request QUERY_STAND_SMT2 (sprintf "%s,%s" context f) in
    match res with
    | r::[] -> 
      if r = "unsat" then false
      else if r = "sat" then true
      else raise Server_exception
    | _ -> raise Server_exception

end

