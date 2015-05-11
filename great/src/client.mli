(** Client for connect to smv/smt2 server

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

exception Server_exception

module Smv : sig
  val compute_reachable : string -> string -> bool
  val query_reachable : string -> int
  val check_inv : string -> string -> string
  val quit : string -> bool
end

module Smt2 : sig
  val set_context : string -> string -> bool
  val check : string -> string -> bool
  val check_stand : string -> string -> bool
end



