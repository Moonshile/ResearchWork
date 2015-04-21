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

(** Find new inv and relations with concrete rule and concrete invariant
    
    @param cinv a concrete property
    @param old_invs invariants in formula format that are already discovered
    @param smv_file the smv file
    @return a tuple of new invariant and relation, the new invariant list is empty or contains 1 inv
*)
val tabular_expans : concrete_rule -> cinv:concrete_prop -> old_invs:formula list -> 
  smv_file:string -> types:typedef list -> vardefs:vardef list -> 
  formula list * t

(** Find new inv and relations with concrete rules and a concrete invariant
    
    @param new_inv_id the id generated for new invariant
    @return a triple: next id of new inv, new invs discovered, relations
*)
val tabular_crules_cinv : concrete_rule list -> concrete_prop -> 
  new_inv_id:int -> smv_file:string -> types:typedef list -> vardefs:vardef list -> 
  int * formula list * t list

(** Find invs and causal relations of a protocol

    @param protocol the protocol
    @param prop_params property parameters given
    @return a triple list: the concrete prop, new invs generated by the prop, and relations
*)
val find : protocol:protocol -> prop_params:(string * paramref) list list -> 
  (concrete_prop * formula list * t list) list