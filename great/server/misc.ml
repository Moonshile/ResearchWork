(** Basic library for establishing a service

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Core.Std;;
open Unix;;

(** Create a socket of type stream in the Internet domain with the default
    protocol and prepares it to accept new connection requests on the 
    address `addr` with `bind` and `listen`.
*)
let install_tcp_server_socket addr =
  let s = socket PF_INET SOCK_STREAM 0 in
  try
    bind s ~addr;
    listen s ~max:100;
    s
  with
  | z -> close s; raise z

(** Create a socket and enters an infinite loop. At each iteration of the
    loop it waits for a connection request and treats it with function
    treat_connection.
*)
let tcp_server treat_connection addr =
  Signal.Expert.handle Signal.pipe Signal.ignore;
  let server_sock = install_tcp_server_socket addr in
  while true do
    let client = retry_until_no_eintr (fun () -> accept server_sock) in
    treat_connection server_sock client
  done

(** Treat each connection sequentially, only appropriate for quick services *)
let sequential_treatment _server service client = service client

(* try to do something, whether it succeeds or not, finally do another *)
let try_finalize f x finally y =
  let res =
    try f x
    with | exn -> finally y; raise exn
  in
  finally y; res

(** Fork a new process for each connection *)
let fork_treatment server service (client_sock, _ as client) =
  let treat () = 
    match fork () with
    | `In_the_child -> close server; service client; exit 0
    | `In_the_parent(_) -> ()
  in
  try_finalize treat () close client_sock

(** Fork a new process for each connection and recover it *)
let double_fork_treatment server service (client_sock, _ as client) =
  let treat () =
    match fork () with
    | `In_the_child ->
      if fork () <> `In_the_child then exit 0;
      close server; service client; exit 0
    | `In_the_parent(k) ->
      ignore (retry_until_no_eintr (fun () -> waitpid k))
  in
  try_finalize treat () close client_sock

