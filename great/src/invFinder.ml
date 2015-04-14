(** For find invariants and causal relations based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils
open Smt
open Smv
open Paramecium

open Core.Std

(** Raised when parallel statements haven't been cast to assign list *)
exception Unexhausted_flat_parallel

(** Concrete rule

    + ConcreteRule: rule, concrete param list
*)
type concrete_rule =
  | ConcreteRule of rule * (string * paramref) list

let concrete_rule r ps = ConcreteRule(r, ps)

(** Concrete property

    + ConcreteProp: property, concrete param list
*)
type concrete_prop =
  | ConcreteProp of prop * (string * paramref) list

let concrete_prop property ps = ConcreteProp(property, ps)

(** Causal relations

  + InvHoldForRule1
  + InvHoldForRule2
  + InvHoldForRule3: the new concrete invariant found
*)
type relation =
  | InvHoldForRule1
  | InvHoldForRule2
  | InvHoldForRule3 of formula

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
  cart_product_with_paramfix paramdefs types
  |> List.map ~f:(fun params -> concrete_rule r params)

(* Convert property to concrete property *)
let prop_2_concrete property ~types =
  let Prop(_, paramdefs, _) = property in
  cart_product_with_paramfix paramdefs types
  |> List.map ~f:(fun params -> concrete_prop property params)

(* Convert concrete property to formula *)
let concrete_prop_2_form cprop =
  let ConcreteProp(property, p) = cprop in
  let Prop(_, _, form) = apply_prop property ~p in
  form

let new_inv_name_base = "inv__"

(* Generate names for new invariants found *)
let next_inv_name new_invs = sprintf "%s%d" new_inv_name_base (List.length new_invs)

(********************************* Module Parameterize *************************************)

(* Convert instantiated components to its corresponding paramterized format *)
module Parameterize = struct

  type t =
    | Paraminfo of paramdef list * (string * paramref) list

  let paraminfo paramdefs params = Paraminfo(paramdefs, params)

  let paramdef_name_base = "p__"

  (* Generate paramdef names *)
  let next_name new_paramdefs = sprintf "%s%d" paramdef_name_base (List.length new_paramdefs)

  (* Convert paramref *)
  let paramref_act pr param_info =
    let Paraminfo(paramdefs, params) = param_info in
    match pr with
    | Paramref(_) -> raise Unexhausted_inst
    | Paramfix(tname, c) ->
      let name = next_name paramdefs in
      let new_def = paramdef name tname in
      let new_param = (name, pr) in
      (paramref name, paraminfo (new_def::paramdefs) (new_param::params))

  (* Convert a list of components *)
  let rec components_act components param_info f =
    match components with
    | [] -> ([], param_info)
    | c::components' ->
      let (c', param_info') = f c param_info in
      let (c'', param_info'') = components_act components' param_info' f in
      (c'::c'', param_info'')

  (* Convert var *)
  let var_act v param_info =
    let Arr(name, prs) = v in
    let (prs', param_info') = components_act prs param_info paramref_act in
    (arr name prs', param_info')

  (* Convert exp *)
  let rec exp_act e param_info =
    match e with
    | Const(_) -> (e, param_info)
    | Var(v) ->
      let (v', param_info') = var_act v param_info in
      (var v', param_info')
    | Cond(f, e1, e2) ->
      let (f', param_info1) = form_act f param_info in
      let (e1', param_info2) = exp_act e1 param_info1 in
      let (e2', param_info3) = exp_act e2 param_info2 in
      (cond f' e1' e2', param_info3)
    | Param(pr) ->
      let (pr', param_info') = paramref_act pr param_info in
      (param pr', param_info')
  (* Convert formula *)
  and form_act f param_info =
    match f with
    | Chaos
    | Miracle -> (f, param_info)
    | Eqn(e1, e2) ->
      let (e1', param_info1) = exp_act e1 param_info in
      let (e2', param_info2) = exp_act e2 param_info1 in
      (eqn e1' e2', param_info2)
    | Neg(f) ->
      let (f', param_info') = form_act f param_info in
      (neg f', param_info')
    | AndList(fl) ->
      let (fl', param_info') = components_act fl param_info form_act in
      (andList fl', param_info')
    | OrList(fl) ->
      let (fl', param_info') = components_act fl param_info form_act in
      (orList fl', param_info')
    | Imply(f1, f2) ->
      let (f1', param_info1) = form_act f1 param_info in
      let (f2', param_info2) = form_act f2 param_info1 in
      (imply f1' f2', param_info2)

end

(* Convert formula to concrete property *)
let form_2_concreate_prop form new_invs =
  let (form', Parameterize.Paraminfo(paramdefs, params)) =
    Parameterize.form_act form (Parameterize.paraminfo [] [])
  in
  let property = prop (next_inv_name new_invs) paramdefs form' in
  concrete_prop property params

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
    | Param(Paramfix(_, c)) -> [(chaos, const c)]
    | Param(Paramref _) -> raise Unexhausted_inst)
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
  | Param(Paramfix(_, c)) -> [(chaos, const c)]
  | Param(Paramref _) -> raise Unexhausted_inst
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
    cartesian_product [expEval e1 ~assigns; expEval e2 ~assigns]
    |> List.map ~f:(handle_tuples ~f:eqn)
  | Neg(f) ->
    formEval f ~assigns
    |> List.map ~f:(fun (g, f') -> (g, neg f'))
  | AndList([]) -> [(chaos, andList [])]
  | AndList(f::glist) ->
    cartesian_product [formEval f ~assigns; formEval (andList glist) ~assigns]
    |> List.map ~f:(handle_tuples ~f:(fun g1 g2 -> andList [g1; g2]))
  | OrList([]) -> [(chaos, orList [])]
  | OrList(f::glist) ->
    cartesian_product [formEval f ~assigns; formEval (orList glist) ~assigns]
    |> List.map ~f:(handle_tuples ~f:(fun g1 g2 -> orList [g1; g2]))
  | Imply(ant, cons) ->
    cartesian_product [formEval ant ~assigns; formEval cons ~assigns]
    |> List.map ~f:(handle_tuples ~f:imply)
  | Chaos -> [(chaos, chaos)]
  | Miracle -> [(chaos, miracle)]

(* preCond *)
let preCond f statements =
  formEval f ~assigns:(statement_2_assigns statements)
  |> List.dedup



(********************************** Module Choose **************************************)

(* Choose a true invariant *)
module Choose = struct

  type level =
    | Tautology of formula
    | Implied of formula
    | New_inv of formula
    | Not_inv of formula

  let tautology form = Tautology form
  let implied form = Implied form
  let new_inv form = New_inv form
  let not_inv form = Not_inv form

  (* Check the level of an optional invariant *)
  let check_level ~types ~vardefs inv smv_file invs =
    if is_tautology (ToStr.Smt2.act inv ~types ~vardefs) then
      tautology inv
    else begin
      
    end

  (* Assign to formula *)
  let assign_to_form statement =
    match statement with
    | Assign(v, e) -> eqn (var v) e
    | Parallel(_) -> raise Unexhausted_flat_parallel

  let rec choose_0_dimen invs guards assigns cons =
    let assigns_on_0_dimen =
      List.filter assigns ~f:(fun (Arr(_, paramrefs), _) -> List.length paramrefs = 0)
    in
    let ants_0_dimen = 
      if assigns_on_0_dimen = [] then
        []
      else begin
        assigns_on_0_dimen
        |> List.map ~f:assign_to_form
        |> List.map ~f:neg
      end
    in



end


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