(** For find invariants and causal relations based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils
open Smt
open Smv
open Paramecium

open Core.Std

(** Concrete rule

    + ConcreteRule: rule, concrete param list
*)
type concrete_rule =
  | ConcreteRule of rule * (string * const) list

let concrete_rule r ps = ConcreteRule(r, ps)

(** Concrete property

    + ConcreteProp: property, concrete param list
*)
type concrete_prop =
  | ConcreteProp of prop * (string * const) list

let concrete_prop property ps = ConcreteProp(property, ps)

(** Causal relations

  + InvHoldForRule1
  + InvHoldForRule2
  + InvHoldForRule3: the new concrete invariant found
*)
type relation =
  | InvHoldForRule1
  | InvHoldForRule2
  | InvHoldForRule3 of concrete_prop

let invHoldForRule1 = InvHoldForRule1
let invHoldForRule2 = InvHoldForRule2
let invHoldForRule3 p = InvHoldForRule3 p

(** InvFinder type, i.e., causal relation table *)
type t = {
  rule: concrete_rule;
  inv: concrete_prop;
  relation: relation;
}

(* Convert rule to concrete rules *)
let rule_2_concrete r ~types =
  let Rule(_, paramdefs, _, _) = r in
  cart_product_with_name paramdefs types
  |> List.map ~f:(fun params -> concrete_rule r params)

(* Convert property to concrete property *)
let prop_2_concrete property ~types =
  let Prop(_, paramdefs, _) = property in
  cart_product_with_name paramdefs types
  |> List.map ~f:(fun params -> concrete_prop property params)

(* Convert statements to a list of assignments *)
let rec statement_2_assigns statement =
  match statement with
  | Parallel(sl) -> List.concat (List.map sl ~f:statement_2_assigns)
  | Assign(v, e) -> [(v, e)]

(* Strengthen the guard of evaluated exps with formula f *)
let strengthen ~form evaled_exp =
  List.map evaled_exp ~f:(fun (h, e) -> (andList [form; h], e))

(* Evaluate exp with assignments
    Result has format (condition, value)
*)
let rec expEval exp ~assigns =
  match exp with
  | Const(_) -> [(chaos, exp)]
  | Var(v) ->
    let value = List.Assoc.find_exn assigns v in
    (match value with
    | Cond(f, e1, e2) -> [(f, e1); (neg f, e2)]
    | Const(_)
    | Var(_) -> [(chaos, value)]
    | Param(_) -> raise Unexhausted_inst)
  | Cond(form, e1, e2) ->
    let evaled_e1 = expEval e1 ~assigns in
    let evaled_e2 = expEval e2 ~assigns in
    let evaled_form = formEval form ~assigns in
    (* strengthen value condition with the True components in evaled_form *)
    let evaled_T =
      List.filter evaled_form ~f:(fun (_, f'') -> f'' = Chaos)
      |> List.map ~f:(fun (form, _) -> strengthen ~form evaled_e1)
      |> List.concat
    in
    (* strengthen value condition with the False components in evaled_form *)
    let evaled_F = 
      List.filter evaled_form ~f:(fun (_, f'') -> f'' = Miracle)
      |> List.map ~f:(fun (form, _) -> strengthen ~form evaled_e2)
      |> List.concat
    in
    evaled_T@evaled_F
  | Param(_) -> raise Unexhausted_inst
(* Evaluate formula with assignments
    Result has format (condition, form)
*)
and formEval form ~assigns =
  let handle_tuples ~f tuples =
    match tuples with
    | [(f1, g1); (f2, g2)] -> (andList [f1; f2], f g1 g2)
    | _ -> raise Empty_exception
  in
  match form with
  | Eqn(e1, e2) ->
    combination [expEval e1 ~assigns; expEval e2 ~assigns]
    |> List.map ~f:(handle_tuples ~f:eqn)
  | Neg(f) ->
    formEval f ~assigns
    |> List.map ~f:(fun (g, f') -> (g, neg f'))
  | AndList([]) -> [(chaos, andList [])]
  | AndList(f::glist) ->
    combination [formEval f ~assigns; formEval (andList glist) ~assigns]
    |> List.map ~f:(handle_tuples ~f:(fun g1 g2 -> andList [g1; g2]))
  | OrList([]) -> [(chaos, orList [])]
  | OrList(f::glist) ->
    combination [formEval f ~assigns; formEval (orList glist) ~assigns]
    |> List.map ~f:(handle_tuples ~f:(fun g1 g2 -> orList [g1; g2]))
  | Imply(ant, cons) ->
    combination [formEval ant ~assigns; formEval cons ~assigns]
    |> List.map ~f:(handle_tuples ~f:imply)
  | Chaos -> [(chaos, chaos)]
  | Miracle -> [(chaos, miracle)]

(* preCond *)
let preCond f statements =
  formEval f ~assigns:(statement_2_assigns statements)
  |> List.dedup



(* Deal with case invHoldForRule1 *)
let deal_with_case_1 crule cinv (invs, relations) =
  (invs, { rule = crule;
    inv = cinv;
    relation = invHoldForRule1;
  }::relations)



(* Deal with case invHoldForRule2 *)
let deal_with_case_2 crule cinv (invs, relations) =
  (invs, { rule = crule;
    inv = cinv;
    relation = invHoldForRule2;
  }::relations)

(* Deal with case invHoldForRule3 *)
let deal_with_case_3 crule cinv remainder (invs, relations) =
  let inv' = invs in (* TODO *)
  (inv', { rule = crule;
    inv = cinv;
    relation = invHoldForRule3 inv';
  }::relations)



let tabular_expans crule cinv (invs, relations) ~types ~vardefs =
  let ConcreteRule(r, param) =  crule in
  let Rule(name, _, form, statement) = apply_rule r ~param in
  let ConcreteProp(p, param) = cinv in
  let Prop(_, _, inv_inst) = apply_prop p ~param in
  (* preCond *)
  let obligations = preCond form statement in
  (* case 2 *)
  let (id_obligations, remainder) =
    List.partition_tf obligations ~f:(fun (_, inv') -> inv_inst = inv')
  in
  let (invs, relations) = deal_with_case_2 crule cinv (invs, relations) in
  if remainder = [] then
    (invs, relations)
  else begin
    (* case 1 *)
    let (taut_obligations, remainder) =
      let new_form (g, inv') = 
        neg (andList [form; g; inv'])
        |> ToStr.Smt2.act ~types ~vardefs
      in
      List.partition_tf remainder ~f:(fun ob -> is_tautology (new_form ob))
    in
    let (invs, relations) = deal_with_case_1 crule cinv (invs, relations) in
    if remainder = [] then
      (invs, relations)
    else
      (* case 3 *)
      deal_with_case_3 crule cinv remainder (invs, relations)
  end

(** Find invs and causal relations of a protocol

    @param protocol the protocol
    @return causal relation table
*)
