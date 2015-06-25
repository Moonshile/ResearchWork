(** Language for cache coherence protocols

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

(*------------------------------ Types ---------------------------------*)

open Paramecium

(** Unexhausted instantiation
    This exception should never be raised. Once raised, There should be a bug in this tool.
*)
exception Unexhausted_inst

(** Global variable *)
val global : string -> var

(** Record definition *)
val record_def : string -> paramdef list-> vardef list -> vardef list

(** Record *)
val record : var list -> var

(** Forall formula *)
val forallFormula : types:typedef list -> paramdef list -> formula -> formula

(** Exist formula *)
val existFormula : types:typedef list -> paramdef list -> formula -> formula

(** Assignment statements *)
type statement =
  | Assign of var * exp
  | Parallel of statement list
  | IfStatement of formula * statement
  | IfelseStatement of formula * statement * statement
  | ForStatement of statement * paramdef list

val assign : var -> exp -> statement
val parallel : statement list -> statement
val ifStatement : formula -> statement -> statement
val ifelseStatement : formula -> statement -> statement -> statement
val forStatement : statement -> paramdef list -> statement

type rule = 
  | Rule of string * paramdef list * formula * statement

val rule : string -> paramdef list -> formula -> statement -> rule

(** Represents the whole protocol *)
type protocol = {
  name: string;
  types: typedef list;
  vardefs: vardef list;
  init: statement;
  rules: rule list;
  properties: prop list;
}

(*----------------------------- Exceptions ----------------------------------*)

(*----------------------------- Functions ----------------------------------*)

(*----------------------------- Translate module ---------------------------------*)

(** Translate language of this level to the next lower level *)
module Trans : sig

  exception Unexhausted_flatten

  (** Translate language of Loach to Paramecium

      @param loach cache coherence protocol written in Loach
      @return the protocol in Paramecium
  *)
  val act : loach:protocol -> Paramecium.protocol
end


module ToSmv : sig
  val protocol_act : protocol -> string
end

