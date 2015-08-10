(** For find invariants and causal relations based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Paramecium

(** Raised when parallel statements haven't been cast to assign list *)
exception Unexhausted_flat_parallel

(** Raised when circular parallel assignments detected *)
exception Circular_parallel_assign

(** Raised when require to check a inv has too many paramters *)
exception Parameter_overflow

(** Concrete rule

    + ConcreteRule: instantiated rule, concrete param list
*)
type concrete_rule =
  | ConcreteRule of string * paramref list
with sexp

val concrete_rule : rule -> paramref list -> concrete_rule

(** Concrete property

    + ConcreteProp: property, concrete param list
*)
type concrete_prop =
  | ConcreteProp of prop * paramref list
with sexp

val concrete_prop : prop -> paramref list -> concrete_prop

(** Causal relations

  + InvHoldForRule1
  + InvHoldForRule2
  + InvHoldForRule3: the new concrete invariant found
*)
type relation =
  | InvHoldForRule1
  | InvHoldForRule2
  | InvHoldForRule3 of concrete_prop
with sexp

val invHoldForRule1 : relation
val invHoldForRule2 : relation
val invHoldForRule3 : concrete_prop -> relation

(** InvFinder type, i.e., causal relation table *)
type t = {
  rule: concrete_rule;
  inv: concrete_prop;
  relation: relation;
}
with sexp


val rule_2_concrete : rule -> paramref list list -> (concrete_rule list * Paramecium.paramref list) list

(** Convert t to a string *)
val to_str : t -> string

(** Find invs and causal relations of a protocol

    @param protocol the protocol
    @param prop_params property parameters given
    @return a triple list: the concrete prop, new invs generated by the prop, and relations
*)
val find : ?smv:string -> ?smv_bmc:string -> ?murphi:string -> Loach.protocol ->
  concrete_prop list * t list

