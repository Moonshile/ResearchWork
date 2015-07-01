(** For find invariants and causal relations based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils
open Smv
open Formula
open Paramecium

open Core.Std

(** Raised when parallel statements haven't been cast to assign list *)
exception Unexhausted_flat_parallel

(** Raised when circular parallel assignments detected *)
exception Circular_parallel_assign

(** Concrete rule

    + ConcreteRule: instantiated rule, concrete param list
*)
type concrete_rule =
  | ConcreteRule of rule * paramref list

let concrete_rule r ps = ConcreteRule(r, ps)

(** Concrete property

    + ConcreteProp: property, concrete param list
*)
type concrete_prop =
  | ConcreteProp of prop * paramref list

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

let type_defs = ref []

(* Convert rule to concrete rules *)
let rule_2_concrete r ps =
  if List.length ps = 0 then
    [concrete_rule r []]
  else begin
    List.map ps ~f:(fun p -> concrete_rule (apply_rule r ~p) p)
  end

(* Convert concrete rule to rule instances *)
let concrete_rule_2_rule_inst cr =
  let ConcreteRule(r, _) = cr in
  r

(* Convert concrete property to formula *)
let concrete_prop_2_form cprop =
  let ConcreteProp(property, pfs) = cprop in
  let Prop(_, _, form) = property in
  apply_form form pfs

(* Convert formula to concrete property *)
let form_2_concreate_prop ?(id=0) form =
  let new_inv_name_base = "inv__" in
  (* Generate names for new invariants found *)
  let next_inv_name id = sprintf "%s%d" new_inv_name_base id in
  let normalized = normalize form ~types:!type_defs in
  let (pds, pfs, form') = Generalize.form_act normalized in
  let property = prop (next_inv_name id) pds form' in
  concrete_prop property pfs

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
  let rps = List.map rps ~f:ToStr.Smv.paramref_act in
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
  | Param(Paramfix(_, _, c)) -> Const c
  | Param(Paramref _) -> raise Unexhausted_inst
  | Var(v) ->
    let value = List.Assoc.find assigns v ~equal:(fun x y -> ToStr.Smv.var_act x = ToStr.Smv.var_act y) in (
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

  (* partition the concrete params by their type, then sort by typename *)
  let sorted_partition params =
    partition_with_label params ~f:typename_of_paramfix
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
  (partition1:(string * paramref list) list) 
  (partition2:(string * paramref list) list) =
    (* parameter names of eache type in partition2 *)
    let params_names_part2 = List.map partition2 ~f:(fun (_, x) -> List.map x ~f:name_of_param) in
    (* parameter count of each type in partition2 *)
    let params_c_part2 = List.map partition2 ~f:(fun (_, x) -> List.length x) in
    (* get values of parameters of shortened partition1 *)
    let params_val_shorten_part1 = List.map partition1 ~f:(fun (_, x) -> x) in
    let rename_all names ls = List.map ls ~f:(fun vs -> List.map2_exn vs names ~f:set_param_name) in
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
  (inv_param1:paramref list) 
  (inv_param2:paramref list) =
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
        if is_tautology (imply (simplify old) (simplify inv)) then Some old
        else begin None end
      (* If old has more paramters, then false *)
      else if param_compatible inv_p old_p = [] then None
      (* Otherwise, check old with parameters of inv *)
      (*else if form_are_symmetric inv old then Some old*)
      else begin
        let params = param_compatible inv_p old_p in
        let forms = List.map params ~f:(fun p-> apply_form old_gened ~p) in
        let tautologies = List.filter forms ~f:(fun form ->
          is_tautology (imply (simplify form) (simplify inv))
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
  let check_level inv invs =
    let inv = simplify inv in
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
  let choose_one pres cons invs =
    let rec wrapper pres =
      match pres with
      | [] -> not_inv
      | pre::pres' ->
        let level = check_level (imply pre cons) invs in (
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
  let choose_with_policy_1 guards cons invs =
    let ConcreteProp(Prop(_, guard_pd, _), _) = form_2_concreate_prop (andList guards) in
    let ConcreteProp(Prop(_, cons_pd, _), _) = form_2_concreate_prop cons in
    let guard_pd_names = String.Set.of_list (List.map guard_pd ~f:(fun (Paramdef(n, _)) -> n)) in
    let cons_pd_names = String.Set.of_list (List.map cons_pd ~f:(fun (Paramdef(n, _)) -> n)) in
    let inter_is_empty = String.Set.is_empty (String.Set.inter guard_pd_names cons_pd_names) in
    if not inter_is_empty then
      not_inv
    else begin
      let inv_on_0_dimen = imply (andList (form_on_0_dimen guards)) cons in
      check_level inv_on_0_dimen invs
    end

  (* choose new inv with policy 2 *)
  let choose_with_policy_2 guards ant_0_dimen cons invs =
    let enhancedGuards = List.map guards ~f:(fun g -> andList [g; ant_0_dimen]) in
    choose_one enhancedGuards cons invs

  (* Assign to formula *)
  let assign_to_form (v, e) = eqn (var v) e

  (* Assignments on 0 dimension variables *)
  let assigns_on_0_dimen assigns =
    List.filter assigns ~f:(fun (Arr(_, paramrefs), _) -> List.is_empty paramrefs)
  
  (* choose new inv about 0 dimension variables *)
  let choose_with_0_dimen_var guards ants_0_dimen cons invs =
    choose_one (guards@ants_0_dimen) cons invs

  (* choose new inv *)
  let choose guards assigns cons invs =
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
      choose_with_0_dimen_var guards ants_0_dimen cons invs
    in
    if not (choosed_0_dimen = Not_inv) then
      choosed_0_dimen
    else begin
      let choosed_by_policy_1 = choose_with_policy_1 guards cons invs in
      if not (choosed_by_policy_1 = Not_inv) then
        choosed_by_policy_1
      else begin
        let choosed_by_policy_2 =
          match ants_0_dimen with
          | [] -> Not_inv
          | ant_0_dimen::_ ->
            choose_with_policy_2 guards ant_0_dimen cons invs
        in
        if not (choosed_by_policy_2 = Not_inv) then
          choosed_by_policy_2
        else begin
          check_level (imply (andList guards) cons) invs
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
let deal_with_case_3 crule cinv cons old_invs =
  let Rule(_name, _, guard, statement) = concrete_rule_2_rule_inst crule in
  let guards = flat_and_to_list guard in
  let assigns = statement_2_assigns statement in
  let level = Choose.choose guards assigns cons old_invs in
  let (new_inv, causal_inv) =
    match level with
    | Choose.Tautology(_) -> ([], chaos)
    | Choose.Implied(_, old) -> ([], old)
    | Choose.New_inv(inv) -> 
      let simplified = simplify inv in
      let new_inv_str = ToStr.Smv.form_act simplified in
      let causal_inv_str = ToStr.Smv.form_act (simplify (concrete_prop_2_form cinv)) in
      print_endline (sprintf "rule %s, new %s, old %s" _name new_inv_str causal_inv_str);
      ([simplified], simplified)
    | Choose.Not_inv ->
      let ConcreteRule(Rule(name, _, _, _), ps) = crule in
      let cp_2_str pr =
        match pr with
        | Paramref(_) -> raise Empty_exception
        | Paramfix(n, _, _) -> sprintf "%s:%s" n (ToStr.Smv.paramref_act pr) in
      let params_str = String.concat (List.map ps ~f:cp_2_str) ~sep:", " in
      let inv_str = ToStr.Smv.form_act (concrete_prop_2_form cinv) in
      let guard_str = ToStr.Smv.form_act guard in
      Prt.error (sprintf "\n\n%s, %s\nguard: %s\n%s\n" name params_str guard_str inv_str);
      (*[], miracle*)
      raise Empty_exception
  in
  (new_inv, { rule = crule;
    inv = cinv;
    relation = invHoldForRule3 (form_2_concreate_prop causal_inv);
  })

let symmetry_form f1 f2 =
  let n1 = normalize ~types:(!type_defs) f1 in
  let n2 = normalize ~types:(!type_defs) f2 in
  match Choose.inv_implied_by_old n1 [n2], Choose.inv_implied_by_old n2 [n1] with
  | Some(_), Some(_) -> 0
  | _ -> String.compare (ToStr.Smv.form_act n1) (ToStr.Smv.form_act n2)


(* Find new inv and relations with concrete rule and a concrete invariant *)
let tabular_expans crule ~cinv ~old_invs =
  let Rule(_name, _, form, statement) = concrete_rule_2_rule_inst crule in
  let inv_inst = simplify (concrete_prop_2_form cinv) in
  (* preCond *)
  let obligation =
    preCond inv_inst statement
    |> simplify
  in
  (*Prt.warning (_name^": "^ToStr.Smv.form_act obligation^", "^ToStr.Smv.form_act inv_inst^"\n");*)
  (* case 2 *)
  if symmetry_form obligation inv_inst = 0 then
    ([], deal_with_case_2 crule cinv)
  (* case 1 *)
  else if is_tautology (imply (simplify form) (simplify (neg obligation))) then
    ([], deal_with_case_1 crule cinv)
  (* case 3 *)
  else begin
    deal_with_case_3 crule cinv (neg obligation) old_invs
  end



let inv_table = Hashtbl.create ~hashable:String.hashable ()



let minify_inv inv =
  let ls = match inv with | OrList(fl) -> fl | _ -> [inv] in
  let rec wrapper necessary parts =
    match parts with
    | [] -> necessary
    | p::parts' ->
      if Smv.is_inv_by_smv (ToStr.Smv.form_act (orList (necessary@parts'))) then
        wrapper necessary parts'
      else begin
        wrapper (p::necessary) parts'
      end
  in
  orList (wrapper [] ls)



(* Find new inv and relations with concrete rules and a concrete invariant *)
let tabular_rules_cinvs crules cinvs =
  let rec wrapper cinvs new_inv_id old_invs relations =
    match cinvs with
    | [] -> (new_inv_id, old_invs, relations)
    | cinv::cinvs' ->
      let (new_invs, new_relation) =
        List.map crules ~f:(tabular_expans ~cinv ~old_invs)
        |> List.unzip
      in
      let real_new_invs =
        List.concat new_invs
        |> List.map ~f:simplify
        |> List.dedup ~compare:symmetry_form
        |> List.map ~f:(normalize ~types:(!type_defs))
        |> List.map ~f:minify_inv
        |> List.map ~f:(normalize ~types:(!type_defs))
        |> List.dedup ~compare:symmetry_form
        |> List.filter ~f:(fun x ->
          let key = ToStr.Smv.form_act x in
          match Hashtbl.find inv_table key with 
          | None -> Hashtbl.replace inv_table ~key ~data:true; true
          | _ -> false)
        |> List.filter ~f:(fun f -> all old_invs ~f:(fun old -> not (symmetry_form f old = 0)))
      in
      if real_new_invs = [] then () else begin
        print_endline ("NewInv: "^String.concat ~sep:"\n" (
          List.map real_new_invs ~f:(fun f -> ToStr.Smv.form_act (simplify (neg f)))
        ))
      end;
      let old_invs' = real_new_invs@old_invs in
      let rec invs_to_cinvs invs cinvs new_inv_id =
        match invs with
        | [] -> (cinvs, new_inv_id)
        | inv::invs' ->
          let cinv = form_2_concreate_prop ~id:new_inv_id (simplify (neg inv)) in
          invs_to_cinvs invs' (cinv::cinvs) (new_inv_id + 1)
      in
      let (new_cinvs, new_inv_id') = invs_to_cinvs real_new_invs [] new_inv_id in
      let cinvs'' = List.dedup (cinvs'@new_cinvs) in
      wrapper cinvs'' new_inv_id' old_invs' (new_relation@relations)
  in
  let init_lib = List.map cinvs ~f:(fun cinv -> neg (concrete_prop_2_form cinv)) in
  Prt.warning ("initial invs:\n"^String.concat ~sep:"\n" (
    List.map init_lib ~f:(fun f -> ToStr.Smv.form_act (simplify (neg f)))
  ));
  wrapper cinvs 0 init_lib []

let simplify_prop property =
  let Prop(_, pds, f) = property in
  let orList_items =
    if List.length pds > 0 then
      let ps = cart_product_with_paramfix pds (!type_defs) in
      List.map ps ~f:(fun p -> simplify (neg (apply_form f ~p)))
    else begin
      [simplify (neg f)]
    end
  in
  orList_items
  |> List.map ~f:(fun form -> match form with | OrList(fl) -> fl | _ -> [form])
  |> List.concat
  |> List.filter ~f:(fun x -> match x with | Miracle -> false | _ -> true)
  |> List.dedup ~compare:symmetry_form

let result_to_str (_, invs, relations) =
  let invs_str =
    invs
    |> List.map ~f:neg
    |> List.map ~f:simplify
    |> List.map ~f:ToStr.Smv.form_act
  in
  let relations_str = List.map relations ~f:to_str in
  String.concat ~sep:"\n" ((*relations_str@*)invs_str)

(** Find invs and causal relations of a protocol

    @param protocol the protocol
    @param prop_params property parameters given
    @return causal relation table
*)
let find ~protocol ?(smv="") () =
  let {name; types; vardefs; init=_init; rules; properties} = Loach.Trans.act protocol in
  let _smt_context = Smt.set_smt_context name (ToStr.Smt2.context_of ~types ~vardefs) in
  let _smv_context =
    if smv = "" then
      Smv.set_smv_context name (Loach.ToSmv.protocol_act protocol)
    else begin
      Smv.set_smv_context name smv
    end
  in
  type_defs := types;
  let cinvs = 
    List.concat (List.map properties ~f:simplify_prop)
    |> List.map ~f:form_2_concreate_prop
  in
  let inst_rule r =
    let Paramecium.Rule(_, paramdefs, _, _) = r in
    let ps = cart_product_with_paramfix paramdefs (!type_defs) in
    rule_2_concrete r ps
    |> List.map ~f:(fun (ConcreteRule(Rule(n, pd, f, s), p)) ->
      let simplified_g = simplify f in
      match simplified_g with
      | OrList(fl) ->
        let indice = up_to (List.length fl) in
        let gen_new_name name i = sprintf "%s_part_%d" name i in
        List.map2_exn fl indice ~f:(fun g i -> concrete_rule (rule (gen_new_name n i) pd g s) p)
      | _ -> [concrete_rule (rule n pd simplified_g s) p]
    )
    |> List.concat
    |> List.filter ~f:(fun (ConcreteRule(Rule(_, _, f, _), _)) -> is_satisfiable f)
  in
  let crules = List.concat (List.map rules ~f:inst_rule) in
  let result = tabular_rules_cinvs crules cinvs in
  printf "%s\n" (result_to_str result);
