
open Core.Std
open Utils
open Client

let open_debug () = debug_switch := true

let set_smv_server_host host () =
  Smv.host := UnixLabels.inet_addr_of_string host

let set_smv_server_port port () =
  Smv.port := port

let set_smt_server_host host () =
  Smt2.host := UnixLabels.inet_addr_of_string host

let set_smt_server_port port () =
  Smt2.port := port

let command program =
  Command.basic
    ~summary:"Find invariants of parameterized systems."
    Command.Spec.(
      empty
      +> flag "-g" no_arg ~doc:"open debuG mode"
      +> flag "-vh" (optional string) ~doc:"set ip address as Host of smV server"
      +> flag "-vp" (optional int) ~doc:"set Port of smV server"
      +> flag "-th" (optional string) ~doc:"set ip address as Host of smT server"
      +> flag "-tp" (optional int) ~doc:"set Port of smT server"
    )
    (fun g vh vp th tp () ->
      begin
        match g with
        | true -> open_debug ()
        | false  -> ()
      end;
      begin
        match vh with
        | Some host -> set_smv_server_host host ()
        | None -> ()
      end;
      begin
        match vp with
        | Some port -> set_smv_server_port port ()
        | None -> ()
      end;
      begin
        match th with
        | Some host -> set_smt_server_host host ()
        | None -> ()
      end;
      begin
        match tp with
        | Some port -> set_smt_server_port port ()
        | None -> ()
      end;
      program ()
    )

let run_with_cmdline program = Command.run (command program)
