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
let rule_2_concrete r ps =
  List.map ps ~f:(fun params -> concrete_rule r params)

(* Convert concrete rule to rule instances *)
let concrete_rule_2_rule_inst cr =
  let ConcreteRule(r, p) = cr in
  apply_rule r ~p

(* Convert property to concrete property *)
let prop_2_concrete property ps =
  List.map ps ~f:(fun params -> concrete_prop property params)

(* Convert concrete property to formula *)
let concrete_prop_2_form cprop =
  let ConcreteProp(property, p) = cprop in
  let Prop(_, _, form) = apply_prop property ~p in
  form

(* Convert formula to concrete property *)
let form_2_concreate_prop ?(id=0) form =
  let new_inv_name_base = "inv__" in
  (* Generate names for new invariants found *)
  let next_inv_name id = sprintf "%s%d" new_inv_name_base id in
  let (form', Generalize.Paraminfo(paramdefs, params)) =
    Generalize.form_act form (Generalize.paraminfo [] [])
  in
  let property = prop (next_inv_name id) paramdefs form' in
  concrete_prop property params

(* Convert statements to a list of assignments *)
let rec statement_2_assigns statement =
  match statement with
  | Parallel(sl) -> List.concat (List.map sl ~f:statement_2_assigns)
  | Assign(v, e) -> [(v, e)]

(** Convert relation to a string *)
let relation_2_str relation =
  match relation with
  | InvHoldForRule1 -> "invHoldForRule1"
  | InvHoldForRule2 -> "invHoldForRule2"
  | InvHoldForRule3(cp) -> 
    let form = simplify (neg (concrete_prop_2_form cp)) in
    sprintf "invHoldForRule3-%s" (ToStr.Smv.form_act form)

(** Convert t to a string *)
let to_str {rule; inv; relation} =
  let ConcreteRule(Rule(rname, _, _, _), rps) = rule in
  let rps = List.map rps ~f:(fun (_, pr) -> ToStr.Smv.paramref_act pr) in
  let rule_str = sprintf "%s%s" rname (String.concat rps) in
  let inv_str = ToStr.Smv.form_act (simplify (concrete_prop_2_form inv)) in
  let rel_str = relation_2_str relation in
  sprintf "rule: %s; inv: %s; rel: %s" rule_str inv_str rel_str


(* Evaluate exp with assignments
    Result has format (condition, value)
*)
let expEval exp ~assigns =
  match exp with
  | Const(_) -> exp
  | Param(Paramfix(_, c)) -> Const c
  | Param(Paramref _) -> raise Unexhausted_inst
  | Var(v) ->
    let value = List.Assoc.find assigns v in (
      match value with
      | None -> var v
      | Some(e) -> e
    )

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
    | Implied of formula * formula
    | New_inv of formula
    | Not_inv

  let tautology form = Tautology form
  let implied form old = Implied(form, old)
  let new_inv form = New_inv form
  let not_inv = Not_inv

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
  let compatible_params 
  (partition1:(string * (string * paramref) list) list) 
  (partition2:(string * (string * paramref) list) list) =
    (* parameter names of eache type in partition2 *)
    let params_names_part2 = List.map partition2 ~f:(fun (_, x) -> get_names_of_params x) in
    (* parameter count of each type in partition2 *)
    let params_c_part2 = List.map partition2 ~f:(fun (_, x) -> List.length x) in
    (* get values of parameters of shortened partition1 *)
    let params_val_shorten_part1 = List.map partition1 ~f:(fun (_, x) -> x) in
    let rename_all names list = List.map list ~f:(set_names_of_params ~names) in
    (*  choose |params2[k]| params in the values of shortened partition1
        result is like [[[a;b];[b;c];[a;c]]; [[1;2];[1;3];[2;3]]]
    *)
    List.map2_exn params_val_shorten_part1 params_c_part2 ~f:combination
    (*  permutation,  result is like
        [[[a;b];[b;a];[b;c];[c;b];[a;c];[c;a]]; [[1;2];[2;1];...]]
    *)
    |> List.map ~f:(fun x -> List.concat (List.map x ~f:(fun y -> permutation y)))
    (* rename to names of partition2 *)
    |> List.map2_exn params_names_part2 ~f:rename_all
    (* result is like [[[a;b];[1;2]]; [[a;b];[2;1]]; ...] *)
    |> cartesian_product
    (* result is like [[a;b;1;2]; [a;b;2;1]; ...] *)
    |> List.map ~f:List.concat

  (* Algorithm ParamCompatible
      This algorithm is for judge if a invariant inv1 is compatible with inv2.

      Compatible definition
      Suppose parameter type set of inv1 is types1, and types2 of inv2; suppose
      |types1| = m, |types2| = n; inv1 is
      compatible with inv2 iff:
      1. types2 is subset of types1 (so n <= m), and
      2. suppose the parameter types in inv1 have parameter sets params1[i] for
        0 <= i < m, and for inv2 have paramter sets params2[j] for 0 <= j < n, then
        |params2[k]| <= |params1[k]| for 0 <= k < n

      This algorithm returns the compatible params combination of inv1 for inv2
      if are compatible, else return []
  *)
  let param_compatible 
  (inv_param1:(string * paramref) list) 
  (inv_param2:(string * paramref) list) =
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
  let inv_implied_by_old inv invs =
    let wrapper inv old =
      let inv_vars = VarNames.of_form inv in
      let old_vars = VarNames.of_form old in
      let ConcreteProp(Prop(_, old_pd, old_gened), old_p) = form_2_concreate_prop old in
      let ConcreteProp(Prop(_, _, _), inv_p) = form_2_concreate_prop inv in
      (* If vars in old are more than vars in inv, then can't imply *)
      (* TODO is there some problems in this strategy? *)
      if String.Set.length (String.Set.diff old_vars inv_vars) > 0 then
        None
      (* If length of parameters in old is 0, then check directly *)
      else if List.length old_pd = 0 then
        if is_tautology (imply old inv) then Some old
        else begin None end
      (* If old has more paramters, then false *)
      else if param_compatible inv_p old_p = [] then None
      (* Otherwise, check old with parameters of inv *)
      else if form_are_symmetric inv old then Some old
      else begin
        let params = param_compatible inv_p old_p in
        let forms = List.map params ~f:(fun p-> apply_form old_gened ~p) in
        let tautologies = List.filter forms ~f:(fun form ->
          is_tautology (imply form inv)
        ) in
        match tautologies with
        | [] -> None
        | form::_ -> Some form
      end
    in
    let implies = List.map invs ~f:(fun old -> wrapper inv old) in
    let not_none = List.filter implies ~f:(fun i -> not (i = None)) in
    match not_none with
    | [] -> None
    | form::_ -> form


  (* Check the level of an optional invariant *)
  let check_level inv smv_file invs =
    if is_tautology inv then
      tautology inv
    else begin
      let implied_by_old = inv_implied_by_old inv invs in
      match implied_by_old with
      | Some(old) -> implied inv old
      | None ->
        if is_inv_by_smv (ToStr.Smv.form_act inv) then
          new_inv inv
        else begin
          not_inv
        end
    end

  (* choose one pre in pres such that (imply pre cons) is an new inv *)
  let choose_one pres cons smv_file invs =
    let rec wrapper pres =
      match pres with
      | [] -> not_inv
      | pre::pres' ->
        let level = check_level (imply pre cons) smv_file invs in (
          match level with
          | New_inv(_)
          | Tautology(_)
          | Implied(_) -> level
          | Not_inv -> wrapper pres'
        )
    in
    wrapper pres

  (* Formulae on 0 dimension variables *)
  let form_on_0_dimen forms =
    List.filter forms ~f:(fun f ->
      let ConcreteProp(Prop(_, pd, _), _) = form_2_concreate_prop f in
      List.is_empty pd
    )

  (* choose new inv with policy 1 *)
  let choose_with_policy_1 guards cons smv_file invs =
    let ConcreteProp(Prop(_, guard_pd, _), _) = form_2_concreate_prop (andList guards) in
    let ConcreteProp(Prop(_, cons_pd, _), _) = form_2_concreate_prop cons in
    let guard_pd_names = String.Set.of_list (List.map guard_pd ~f:(fun (Paramdef(n, _)) -> n)) in
    let cons_pd_names = String.Set.of_list (List.map cons_pd ~f:(fun (Paramdef(n, _)) -> n)) in
    let inter_is_empty = String.Set.is_empty (String.Set.inter guard_pd_names cons_pd_names) in
    if not inter_is_empty then
      not_inv
    else begin
      let inv_on_0_dimen = imply (andList (form_on_0_dimen guards)) cons in
      check_level inv_on_0_dimen smv_file invs
    end

  (* choose new inv with policy 2 *)
  let choose_with_policy_2 guards ant_0_dimen cons smv_file invs =
    let enhancedGuards = List.map guards ~f:(fun g -> andList [g; ant_0_dimen]) in
    choose_one enhancedGuards cons smv_file invs

  (* get new inv by removing a component in the pres *)
  let remove_one guards cons smv_file invs =
    let rec wrapper guards necessary =
      match guards with
      | [] -> check_level (imply (andList necessary) cons) smv_file invs
      | g::guards' -> (
          let op_inv = (imply (andList (guards'@necessary)) cons) in
          match check_level op_inv smv_file invs with
          | New_inv(_)
          | Tautology(_)
          | Implied(_) -> wrapper guards' necessary
          | Not_inv -> wrapper guards' (g::necessary)
        )
    in
    wrapper guards []

  (* Assign to formula *)
  let assign_to_form (v, e) = eqn (var v) e

  (* Assignments on 0 dimension variables *)
  let assigns_on_0_dimen assigns =
    List.filter assigns ~f:(fun (Arr(_, paramrefs), _) -> List.is_empty paramrefs)
  
  (* choose new inv about 0 dimension variables *)
  let choose_with_0_dimen_var guards ants_0_dimen cons smv_file invs =
    choose_one (guards@ants_0_dimen) cons smv_file invs

  (* choose new inv *)
  let choose guards assigns cons smv_file invs =
    let dimen_0 = assigns_on_0_dimen assigns in
    let ants_0_dimen = 
      if dimen_0 = [] then
        []
      else begin
        dimen_0
        |> List.map ~f:assign_to_form
        |> List.map ~f:neg
      end
    in
    let choosed_0_dimen = 
      choose_with_0_dimen_var guards ants_0_dimen cons smv_file invs
    in
    if not (choosed_0_dimen = Not_inv) then
      choosed_0_dimen
    else begin
      let choosed_by_policy_1 = choose_with_policy_1 guards cons smv_file invs in
      if not (choosed_by_policy_1 = Not_inv) then
        choosed_by_policy_1
      else begin
        let choosed_by_policy_2 =
          match ants_0_dimen with
          | [] -> Not_inv
          | ant_0_dimen::_ ->
            choose_with_policy_2 guards ant_0_dimen cons smv_file invs
        in
        if not (choosed_by_policy_2 = Not_inv) then
          choosed_by_policy_2
        else begin
          remove_one guards cons smv_file invs
        end
      end
    end

end


(* Deal with case invHoldForRule1 *)
let deal_with_case_1 crule cinv =
  { rule = crule;
    inv = cinv;
    relation = invHoldForRule1;
  }

(* Deal with case invHoldForRule2 *)
let deal_with_case_2 crule cinv =
  { rule = crule;
    inv = cinv;
    relation = invHoldForRule2;
  }

(* Deal with case invHoldForRule3 *)
let deal_with_case_3 crule cinv cons old_invs smv_file =
  let Rule(_, _, guard, statement) = concrete_rule_2_rule_inst crule in
  let guards = flat_and_to_list guard in
  let assigns = statement_2_assigns statement in
  let level = Choose.choose guards assigns cons smv_file old_invs in
  let (new_inv, causal_inv) =
    match level with
    | Choose.Tautology(_) -> ([], chaos)
    | Choose.Implied(_, old) -> ([], old)
    | Choose.New_inv(inv) -> 
      let simplified = simplify inv in
      ([simplified], simplified)
    | Choose.Not_inv -> raise Empty_exception
  in
  (new_inv, { rule = crule;
    inv = cinv;
    relation = invHoldForRule3 (form_2_concreate_prop causal_inv);
  })

(* Find new inv and relations with concrete rule and a concrete invariant *)
let tabular_expans crule ~cinv ~old_invs ~smv_file =
  let Rule(_, _, form, statement) = concrete_rule_2_rule_inst crule in
  let inv_inst = simplify (concrete_prop_2_form cinv) in
  (* preCond *)
  let obligation =
    preCond inv_inst statement
    |> simplify
  in
  (* case 2 *)
  if form_are_symmetric obligation inv_inst then
    ([], deal_with_case_2 crule cinv)
  (* case 1 *)
  else if is_tautology (imply form (neg obligation)) then
    ([], deal_with_case_1 crule cinv)
  (* case 3 *)
  else begin
    deal_with_case_3 crule cinv (neg obligation) old_invs smv_file
  end

(* Rule instant policy *)
let rule_inst_policy r ~cinv ~types =
  let Rule(_, paramdefs, _, _) = r in
  let ps = cart_product_with_paramfix paramdefs types in
  rule_2_concrete r ps

(* Find new inv and relations with concrete rules and a concrete invariant *)
let tabular_rules_cinv rules cinv ~new_inv_id ~smv_file ~types =
  let rec wrapper cinvs new_inv_id old_invs relations =
    match cinvs with
    | [] -> (new_inv_id, old_invs, relations)
    | cinv::cinvs' ->
      let crules = List.concat (List.map rules ~f:(rule_inst_policy ~cinv ~types)) in
      let (new_invs, new_relation) =
        List.map crules ~f:(tabular_expans ~cinv ~old_invs ~smv_file)
        |> List.unzip
      in
      let real_new_invs =
        List.concat new_invs
        |> List.dedup ~compare:(fun x y -> if form_are_symmetric x y then 0 else 1)
        |> List.map ~f:(normalize ~types)
      in
      let old_invs' = List.dedup (real_new_invs@old_invs) in
      let rec invs_to_cinvs invs cinvs new_inv_id =
        match invs with
        | [] -> (cinvs, new_inv_id)
        | inv::invs' ->
          let cinv = form_2_concreate_prop ~id:new_inv_id (neg inv) in
          invs_to_cinvs invs' (cinv::cinvs) (new_inv_id + 1)
      in
      let (new_cinvs, new_inv_id') = invs_to_cinvs real_new_invs [] new_inv_id in
      let cinvs'' = List.dedup (cinvs'@new_cinvs) in
      wrapper cinvs'' new_inv_id' old_invs' (new_relation@relations)
  in
  let init_lib = [neg (concrete_prop_2_form cinv)] in
  wrapper [cinv] new_inv_id init_lib []


(** Find invs and causal relations of a protocol

    @param protocol the protocol
    @param prop_params property parameters given
    @return causal relation table
*)
let find ~protocol:{name; types; vardefs; init=_; rules; properties} ~prop_params =
  let smv_file = sprintf "%s.smv" name in
  let _smt_context = set_smt_context name (ToStr.Smt2.context_of ~types ~vardefs) in
  let _smv_context = set_smv_context name (In_channel.input_all (In_channel.create smv_file)) in
  let rec wrapper cinvs new_inv_id table =
    match cinvs with
    | [] -> table
    | cinv::cinvs' ->
      let (new_inv_id', invs_lib, relations) =
        tabular_rules_cinv rules cinv ~new_inv_id ~smv_file ~types
      in
      wrapper cinvs' new_inv_id' ((cinv, invs_lib, relations)::table)
  in
  let cinvs = List.map2_exn properties prop_params ~f:(concrete_prop) in
  wrapper cinvs 0 []
