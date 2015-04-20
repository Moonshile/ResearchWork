(** For find invariants and causal relations based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Paramecium

(** Raised when parallel statements haven't been cast to assign list *)
exception Unexhausted_flat_parallel

(** Raised when circular parallel assignments detected *)
exception Circular_parallel_assign

(** Concrete rule

    + ConcreteRule: rule, concrete param list
*)
type concrete_rule =
  | ConcreteRule of rule * (string * paramref) list

val concrete_rule : rule -> (string * paramref) list -> concrete_rule

(** Concrete property

    + ConcreteProp: property, concrete param list
*)
type concrete_prop =
  | ConcreteProp of prop * (string * paramref) list

val concrete_prop : prop -> (string * paramref) list -> concrete_prop

(** Causal relations

  + InvHoldForRule1
  + InvHoldForRule2
  + InvHoldForRule3: the new concrete invariant found
*)
type relation =
  | InvHoldForRule1
  | InvHoldForRule2
  | InvHoldForRule3 of concrete_prop

val invHoldForRule1 : relation
val invHoldForRule2 : relation
val invHoldForRule3 : concrete_prop -> relation

(** InvFinder type, i.e., causal relation table *)
type t = {
  rule: concrete_rule;
  inv: concrete_prop;
  relation: relation;
}

(** Find invs and causal relations of a protocol

    @param protocol the protocol
    @return causal relation table
*)
val find : protocol:protocol -> (string * paramref) list list list ->
  (string * (formula list * t list) list) list