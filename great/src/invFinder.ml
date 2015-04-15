(** For find invariants and causal relations based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils
open Smt
open Smv
open Formula
open Paramecium

open Core.Std

(** Raised when parallel statements haven't been cast to assign list *)
exception Unexhausted_flat_parallel

(** Raised when circular parallel assignments detected *)
exception Circular_parallel_assign

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

(* Convert formula to concrete property *)
let form_2_concreate_prop ?(new_invs=[]) form =
  let new_inv_name_base = "inv__" in
  (* Generate names for new invariants found *)
  let next_inv_name new_invs = sprintf "%s%d" new_inv_name_base (List.length new_invs) in
  let (form', Generalize.Paraminfo(paramdefs, params)) =
    Generalize.form_act form (Generalize.paraminfo [] [])
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
let expEval exp ~assigns =
  let rec wrapper exp assigns accessed =
    match exp with
    | Const(_) -> exp
    | Param(Paramfix(_, c)) -> Const c
    | Param(Paramref _) -> raise Unexhausted_inst
    | Var(v) ->
      let accessed' = exp::accessed in
      let value = List.Assoc.find assigns v in (
        match value with
        | None -> var v
        | Some(Var v') ->
          if List.exists accessed' ~f:(fun x -> x = var v') then
            (* TODO in fact, this is circular parallel assignment
                which is illegal
            *)
            var v'
            (* raise Circular_parallel_assign *)
          else begin
            wrapper (var v') assigns accessed'
          end
        | Some(e) -> wrapper e assigns accessed'
      )
  in
  wrapper exp assigns []

(* Evaluate formula with assignments
    Result has format (condition, form)
*)
let rec formEval form ~assigns =
  match form with
  | Chaos
  | Miracle -> form
  | Eqn(e1, e2) -> eqn (expEval e1 ~assigns) (expEval e2 ~assigns)
  | Neg(f) -> neg (formEval f ~assigns)
  | AndList(fl) -> andList (List.map fl ~f:(formEval ~assigns))
  | OrList(fl) -> orList (List.map fl ~f:(formEval ~assigns))
  | Imply(ant, cons) -> imply (formEval ant ~assigns) (formEval cons ~assigns)

(* preCond *)
let preCond f statements = formEval f ~assigns:(statement_2_assigns statements)


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

  (* get type name of a concrete param *)
  let param_type_name param =
    match param with
    | (_, Paramfix(tname, _)) -> tname
    | (_, Paramref(_)) -> raise Unexhausted_inst

  (* partition the concrete params by their type, then sort by typename *)
  let sorted_partition params =
    partition_with_label params ~f:param_type_name
    |> List.sort ~cmp:(fun (x, _) (y, _) -> String.compare x y)

  (*  Given two partitioned concrete params, suppose they have same types
      Judge if the first partition has no more parameters in each type
      than the second one
  *)
  let not_more_params partition1 partition2 =
    let not_more_than (_, x) (_, y) = List.length x <= List.length y in
    List.map2_exn partition1 partition2 ~f:not_more_than
    |> reduce ~default:true ~f:(fun x y -> x && y)

  (*  If partition1 is the compatible subset of one with partition2
      Generate the compatible parameters
  *)
  let compatible_params partition1 partition2 =
    (* parameter names of eache type in partition2 *)
    let params_names_part2 = List.map partition2 ~f:(fun (_, x) -> get_names_of_params x) in
    (* parameter count of each type in partition2 *)
    let params_c_part2 = List.map partition2 ~f:(fun (_, x) -> List.length x) in
    (* get values of parameters of shortened partition1 *)
    let params_val_shorten_part1 = List.map partition1 ~f:(fun (_, x) -> x) in
    let rename_all list names = List.map list ~f:(set_names_of_params ~names) in
    (*  choose |params2[k]| params in the values of shortened partition1
        result is like [[[a;b];[b;c];[a;c]]; [[1;2];[1;3];[2;3]]]
    *)
    let choosed_comb = List.map2_exn params_val_shorten_part1 params_c_part2 ~f:combination in
    (* rename to names of partition2 *)
    List.map2_exn choosed_comb params_names_part2 ~f:rename_all
    (*  permutation,  result is like
        [[[a;b];[b;a];[b;c];[c;b];[a;c];[c;a]]; [[1;2];[2;1];...]]
    *)
    |> List.map ~f:(fun x -> List.map x ~f:(fun y -> List.concat (permutation y)))
    (* result is like [[[a;b];[1;2]]; [[a;b];[2;1]]; ...] *)
    |> cartesian_product
    (* result is like [[a;b;1;2]; [a;b;2;1]; ...] *)
    |> List.map ~f:List.concat

  (* Algorithm ParamCompatible
      This algorithm is for judge if a invariant inv1 is compatible with inv2.

      Compatible definition
      Suppose parameter type set of inv1 is types1, and types2of inv2; suppose
      |types1| = m, |types2| = n; inv1 is
      compatible with inv2 iff:
      1. types2 is subset of types1 (so n <= m), and
      2. suppose the parameter types in inv1 have parameter sets params1[i] for
        0 <= i < m, and for inv2 have paramter sets params2[j] for 0 <= j < n, then
        |params2[k]| <= |params1[k| for 0 <= k < n

      This algorithm returns the compatible params combination of inv1 for inv2
      if are compatible, else return []
  *)
  let param_compatible inv_param1 inv_param2 =
    (* Firstly, partition the parameters by their type *)
    let partition1 = sorted_partition inv_param1 in
    let partition2 = sorted_partition inv_param2 in
    (* Secondly, Judge the compatibility *)
    let types1 = String.Set.of_list (List.map partition1 ~f:(fun (x, _) -> x)) in
    let types2 = String.Set.of_list (List.map partition2 ~f:(fun (x, _) -> x)) in
    (* types2 is not subset of types1 *)
    if not (String.Set.subset types2 types1) then
      []
    else begin
      let shorten_partition1 = List.sub partition1 ~pos:0 ~len:(List.length partition2) in
      let more_params = not (not_more_params partition2 shorten_partition1) in
      (* |params2[k]| > |params1[k| for 0 <= k < n *)
      if more_params then
        []
      (* is compatible *)
      else begin
        compatible_params shorten_partition1 partition2
      end
    end

  (* Check if the new inv could be implied by old ones *)
  let inv_implied_by_old ~types ~vardefs inv invs =
    let wrapper inv old =
      let inv_vars = VarNames.of_form inv in
      let old_vars = VarNames.of_form old in
      let ConcreteProp(Prop(_, old_pd, old_gened), old_p) = form_2_concreate_prop old in
      let ConcreteProp(Prop(_, _, _), inv_p) = form_2_concreate_prop inv in
      (* If vars in old are more than vars in inv, then can't imply *)
      (* TODO is there some problems in this strategy? *)
      if String.Set.length (String.Set.diff old_vars inv_vars) > 0 then
        false
      (* If length of parameters in old is 0, then check directly *)
      else if List.length old_pd = 0 then
        is_tautology (imply old inv) ~types ~vardefs
      (* If old has more paramters, then false *)
      else if param_compatible inv_p old_p = [] then
        false
      (* Otherwise, check old with parameters of inv *)
      else begin
        let params = param_compatible inv_p old_p in
        any params ~f:(fun p -> is_tautology (apply_form old_gened p) ~types ~vardefs)
      end
    in
    any invs ~f:(fun old -> wrapper inv old)


  (* Check the level of an optional invariant *)
  let check_level ~types ~vardefs inv smv_file invs =
    if is_tautology inv ~types ~vardefs then
      tautology inv
    else if inv_implied_by_old ~types ~vardefs inv invs then
      implied inv
    else if is_inv_by_smv ~smv_file (ToStr.Smv.form_act inv) then
      new_inv inv
    else begin
      not_inv inv
    end

  (* Assign to formula *)
  let assign_to_form statement =
    match statement with
    | Assign(v, e) -> eqn (var v) e
    | Parallel(_) -> raise Unexhausted_flat_parallel

  (*let rec choose_0_dimen_var invs guards assigns cons =
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
    in*)


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
let deal_with_case_3 crule cinv obligation (invs, relations) =
  let inv' = invs in (* TODO *)
  (inv', { rule = crule;
    inv = cinv;
    relation = invHoldForRule3 (form_2_concreate_prop inv');
  }::relations)



let tabular_expans crule cinv (invs, relations) ~types ~vardefs =
  let ConcreteRule(r, p) =  crule in
  let Rule(_, _, form, statement) = apply_rule r ~p in
  let ConcreteProp(property, p) = cinv in
  let Prop(_, _, inv_inst) = apply_prop property ~p in
  (* preCond *)
  let obligation = preCond inv_inst statement in
  (* case 2 *)
  if obligation = inv_inst then
    deal_with_case_2 crule cinv (invs, relations)
  (* case 1 *)
  else if is_tautology (imply form obligation) ~types ~vardefs then
    deal_with_case_1 crule cinv (invs, relations)
  (* case 3 *)
  else begin
    deal_with_case_3 crule cinv obligation (invs, relations)
  end

(** Find invs and causal relations of a protocol

    @param protocol the protocol
    @return causal relation table
*)
