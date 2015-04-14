(** Translate Paramecium to string of other languages

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Paramecium

(*----------------------------- Module To SMT String ----------------------------------*)

(** Translate to smt2 string *)
module Smt2 : sig

  (** Translate to smt2 string

      @param types the type definitions of the protocol
      @param vardefs the variable definitions of the protocol
      @param form the formula to be translated
      @return the smt2 string
  *)
  val act : types:typedef list -> vardefs:vardef list -> formula -> string

end

(*----------------------------- Module To SMV String ----------------------------------*)

(** Translate to smv string *)
module Smv : sig

  (** Translate formula to smv string

      @param form the formula to be translated
      @return the smv string
  *)
  val form_act : formula -> string

end
