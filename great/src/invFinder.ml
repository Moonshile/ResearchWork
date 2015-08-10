(** For find invariants and causal relations based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils
open Formula
open Paramecium

open Core.Std

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

let concrete_rule r ps =
  let Rule(name, _, _, _) = r in
  ConcreteRule(name, ps)

(** Concrete property

    + ConcreteProp: property, concrete param list
*)
type concrete_prop =
  | ConcreteProp of prop * paramref list
with sexp

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
with sexp

let invHoldForRule1 = InvHoldForRule1
let invHoldForRule2 = InvHoldForRule2
let invHoldForRule3 p = InvHoldForRule3 p

(** InvFinder type, i.e., causal relation table *)
type t = {
  rule: concrete_rule;
  inv: concrete_prop;
  relation: relation;
}
with sexp

let type_defs = ref []
let protocol_name = ref ""
let rule_table = Hashtbl.create ~hashable:String.hashable ()

let simplify_inst_guard (Rule(n, pd, f, s)) =
  let gs = match simplify f with
    | OrList(fl) -> fl
    | _ as g -> [g]
  in
  let sat_gs = List.filter gs ~f:is_satisfiable in
  let indice = up_to (List.length sat_gs) in
  let gen_new_name name i = sprintf "%s__part__%d" name i in
  List.map2_exn sat_gs indice ~f:(fun g i -> rule (gen_new_name n i) pd g s)

(* Convert rule to concrete rules *)
let rule_2_concrete r ps =
  let r_insts =
    if List.length ps = 0 then [([r], [])] else List.map ps ~f:(fun p ->
      (simplify_inst_guard (apply_rule r ~p), p)
    )
  in
  let rec store_rules rs =
    match rs with
    | [] -> ()
    | (ris, _)::rs' ->
      let rec do_store ris =
        match ris with
        | [] -> ()
        | ri::ris' ->
          let Rule(name, _, _, _) = ri in
          Hashtbl.replace rule_table ~key:name ~data:ri;
          do_store ris'
      in
      do_store ris;
      store_rules rs'
  in
  store_rules r_insts;
  List.map r_insts ~f:(fun (ris, p) -> List.map ris ~f:(fun ri -> concrete_rule ri p), p)

(* Convert concrete rule to rule instances *)
let concrete_rule_2_rule_inst cr =
  let ConcreteRule(rname, _) = cr in
  match Hashtbl.find rule_table rname with
  | Some(r) -> r
  | None ->
    Prt.error (sprintf "%s not in [%s]" rname (String.concat ~sep:", " (Hashtbl.keys rule_table)));
    raise Empty_exception

(* Convert concrete property to formula *)
let concrete_prop_2_form cprop =
  let ConcreteProp(property, pfs) = cprop in
  let Prop(_, _, form) = property in
  apply_form form ~p:pfs

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
    let form = (concrete_prop_2_form cp) in
    sprintf "invHoldForRule3-%s" (ToStr.Smv.form_act form)

(** Convert t to a string *)
let to_str {rule; inv; relation} =
  let ConcreteRule(rname, _) = rule in
  let inv_str = ToStr.Smv.form_act (concrete_prop_2_form inv) in
  let rel_str = relation_2_str relation in
  sprintf "rule: %s; inv: %s; rel: %s" rname inv_str rel_str


(* Evaluate exp with assignments
    Result has format (condition, value)
*)
let expEval exp ~assigns =
  match exp with
  | Const(_) -> exp
  | Param(Paramref _) -> raise Unexhausted_inst
  | Param(_) -> exp
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



(* Minify inv by remove useless components one by one *)
let minify_inv_desc inv =
  let rec wrapper necessary parts =
    match parts with
    | [] ->
      if Smv.is_inv (ToStr.Smv.form_act (neg (andList necessary))) then
        necessary
      else begin raise Empty_exception end
    | p::parts' ->
      if Smv.is_inv (ToStr.Smv.form_act (neg (andList (necessary@parts')))) then
        wrapper necessary parts'
      else begin
        wrapper (p::necessary) parts'
      end
  in
  let ls = match inv with | AndList(fl) -> fl | _ -> [inv] in
  andList (wrapper [] ls)

(* Minify inv by add useful components gradually *)
let minify_inv_inc inv =
  let rec wrapper components =
    match components with
    | [] -> 
      Prt.error ("Not invariant: "^ToStr.Smv.form_act inv);
      raise Empty_exception
    | parts::components' ->
      let piece = normalize (andList parts) ~types:(!type_defs) in
      (*let (_, pfs, _) = Generalize.form_act piece in
      Prt.info ("parts: "^ToStr.Smv.form_act (andList parts)^
        "\nnormalized: "^ToStr.Smv.form_act piece^"Res: "^
        (if List.length pfs <= 3 then
          (if Smv.is_inv (ToStr.Smv.form_act (neg piece)) then "true" else "false")
          else begin "unknown" end
        )
      );*)
      let check_inv_res =
        let (_, pfs, _) = Generalize.form_act piece in
        (* TODO *)
        let over = List.filter pfs ~f:(fun pr ->
          match pr with
          | Paramfix(_, _, Intc(i)) -> i > 3
          | _ -> false
        ) in
        let check_with_murphi form =
          let form_str = ToStr.Smv.form_act ~lower:false (neg form) in
          let res = Murphi.is_inv form_str in
          print_endline (sprintf "Check by mu: %s, %b" form_str res); res
        in
        if List.is_empty over then
          try Smv.is_inv (ToStr.Smv.form_act (neg piece)) with
          | Client.Smv.Cannot_check -> check_with_murphi piece
          | _ -> raise Empty_exception
        else begin
          check_with_murphi piece
        end
      in
      if check_inv_res then piece
      else begin wrapper components' end
  in
  let ls = match inv with | AndList(fl) -> fl | _ -> [inv] in
  let components = combination_all ls in
  wrapper components



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

  (* Decide if formula cons could be implied by ant *)
  let can_imply ant cons =
      let ant_vars = VarNames.of_form ant in
      let cons_vars = VarNames.of_form cons in
      let ConcreteProp(Prop(_, ant_pd, ant_gened), ant_p) = form_2_concreate_prop ant in
      let ConcreteProp(Prop(_, _, _), cons_p) = form_2_concreate_prop cons in
      (* If vars in old are more than vars in inv, then can't imply *)
      (* TODO is there some problems in this strategy? *)
      if String.Set.length (String.Set.diff ant_vars cons_vars) > 0 then
        None
      (* If length of parameters in old is 0, then check directly *)
      else if List.length ant_pd = 0 then
        if is_tautology (imply (simplify ant) (simplify cons)) then Some ant
        else begin None end
      (* If old has more paramters, then false *)
      else if param_compatible cons_p ant_p = [] then None
      (* Otherwise, check old with parameters of inv *)
      (*else if form_are_symmetric inv old then Some old*)
      else begin
        let params = param_compatible cons_p ant_p in
        let rec has_implied params =
          match params with
          | [] -> None
          | p::params' ->
            let new_ant = apply_form ant_gened ~p in
            if is_tautology (imply (simplify new_ant) (simplify cons)) then
              Some new_ant
            else begin
              has_implied params'
            end
        in
        has_implied params
      end

  (* Check if the new inv could be implied by old ones *)
  let rec inv_implied_by_old inv invs =
    match invs with
    | [] -> None
    | old::invs' ->
      (* Note that invariants here are of their negation format.
          i.e. A real INV is of form inv = (neg INV), so as old = (neg OLD).
          Because there is implication OLD -> INV, thus (neg INV) -> (neg OLD),
          which means inv -> old
      *)
      let res = can_imply inv old in
      if res = None then inv_implied_by_old inv invs' else begin res end

  (* Check the level of an optional invariant *)
  let check_level ?(must_new=false) inv invs =
    let inv = simplify inv in
    if is_tautology inv then
      tautology inv
    else begin
      try 
        let inv = minify_inv_inc inv in
        let implied_by_old = inv_implied_by_old inv invs in
        match implied_by_old with
        | Some(old) -> implied inv old
        | None ->
          let normalized = normalize inv ~types:(!type_defs) in
          if must_new || Smv.is_inv (ToStr.Smv.form_act (neg normalized)) then
            new_inv inv
          else begin
            not_inv
          end
      with
      | _ -> not_inv
    end

  (* choose one pre in pres such that (imply pre cons) is an new inv *)
  let choose_one pres cons invs =
    let rec wrapper pres =
      match pres with
      | [] -> not_inv
      | pre::pres' ->
        let level = check_level (simplify (andList [pre; cons])) invs in (
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
      let inv_on_0_dimen = simplify (andList (cons::form_on_0_dimen guards)) in
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
    List.filter assigns ~f:(fun (Arr(ls), _) -> List.is_empty (
      List.concat (List.map ls ~f:(fun (_, params) -> params))
    ))
  
  (* choose new inv about 0 dimension variables *)
  let choose_with_0_dimen_var guards ants_0_dimen cons invs =
    choose_one (guards@ants_0_dimen) cons invs

  (* choose new inv *)
  let choose guards assigns cons invs =
    (* It seems that if we simplify invs increasingly, then we should check big inv directly *)
    (* let dimen_0 = assigns_on_0_dimen assigns in
    let ants_0_dimen = 
      if dimen_0 = [] then
        []
      else begin
        dimen_0
        |> List.map ~f:assign_to_form
        |> List.map ~f:neg
      end
    in *)
    check_level ~must_new:true (simplify (andList (cons::guards))) invs
    (*let choosed_0_dimen = 
      if not (ants_0_dimen = []) then
        choose_with_0_dimen_var guards ants_0_dimen cons invs
      else begin not_inv end
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
          check_level ~must_new:true (simplify (andList (cons::guards))) invs
        end
      end
    end*)

end








let get_rule_inst_name rname pfs =
  let const_act c =
    match c with
    | Intc(i) -> Int.to_string i
    | Strc(s) -> String.lowercase s
    | Boolc(b) -> String.uppercase (Bool.to_string b)
  in
  let paramref_act pr =
    match pr with
    | Paramfix(_, _, c) -> sprintf "[%s]" (const_act c)
    | Paramref(_) -> raise Unexhausted_inst
  in
  sprintf "%s%s" rname (String.concat (List.map pfs ~f:paramref_act))

let sort_pfs pds pfs = List.map pds ~f:(fun (Paramdef(name, _)) ->
  List.find_exn pfs ~f:(fun pr ->
    match pr with
    | Paramfix(n, _, _) -> n = name
    | _ -> raise Empty_exception
  )
)

module SemiPerm = struct

  let semi_table = Hashtbl.create ~hashable:String.hashable ()

  let equal_int a b m n = not ((a <= m - n) && (not (a = b)))

  let equal_list ls1 ls2 m n =
    if List.length ls1 = List.length ls2 && List.length ls1 > 0 then
      let flags = List.map (List.zip_exn ls1 ls2) ~f:(fun (x, y) -> equal_int x y m n) in
      all flags ~f:(fun flag -> flag = true)
    else begin false end

  let equal ls1 ls2 m n =
    (equal_list ls1 ls2 m n) || (equal_list (List.rev ls1) (List.rev ls2) m n)

  let rec semi ls m n =
    match ls with
    | [] -> []
    | ele::ls' ->
      let not_equal = List.filter ls' ~f:(fun x -> not (equal ele x m n)) in
      ele::(semi not_equal m n)

  let semi_perm m n =
    match (m, n) with
    | (m, 0) -> [[]]
    | (0, n) -> []
    | _ -> 
      let nums = List.map (up_to m) ~f:(fun x -> x + 1) in
      semi (combination_permutation nums n) m n

  let gen_of_a_type inv_pds rule_pds =
    match rule_pds with
    | [] -> raise Empty_exception
    | (Paramdef(_, tname))::_ ->
      let trange = name2type ~tname ~types:(!type_defs) in
      let n_inv = List.length inv_pds in
      let n_rule = List.length rule_pds in
      let semi_list = semi_perm (n_inv + n_rule) n_rule in
      let semi_consts = List.map semi_list ~f:(fun ls -> List.map ls ~f:(fun x -> intc x)) in
      let valid_consts = List.filter semi_consts ~f:(fun ls ->
        all ls ~f:(fun x -> List.exists trange ~f:(fun t -> t = x))
      ) in
      List.map valid_consts ~f:(fun ls ->
        List.map2_exn ls rule_pds ~f:(fun c (Paramdef(name, _)) -> paramfix name tname c)
      )


  let gen_paramfixes inv_pds rule_pds =
    let key = String.concat ~sep:";" (List.map (inv_pds@rule_pds) ~f:(fun (Paramdef(name, tname)) ->
      sprintf "%s:%s" name tname
    )) in
    match Hashtbl.find semi_table key with
    | None ->
      let inv_parts = partition_with_label inv_pds ~f:(fun (Paramdef(_, tname)) -> tname) in
      let rule_parts = partition_with_label rule_pds ~f:(fun (Paramdef(_, tname)) -> tname) in
      let paramfixes = 
        List.map rule_parts ~f:(fun (tname, rpds) ->
          let ipds = 
            match List.Assoc.find inv_parts tname with
            | None -> []
            | Some(ls) -> ls
          in
          gen_of_a_type ipds rpds
        )
        |> cartesian_product
        |> List.map ~f:List.concat
      in
      let pf_unsym =
        let all_pfs = cart_product_with_paramfix rule_pds (!type_defs) in
        (* TODO *)
        let has_unsym pfs = List.exists pfs ~f:(fun pr -> 
          match pr with 
          | Paramfix(_, _, c) -> c = intc 0
          | _ -> raise Empty_exception
        ) in
        List.filter all_pfs ~f:has_unsym
      in
      let res = List.map (paramfixes@pf_unsym) ~f:(sort_pfs rule_pds) in
      Hashtbl.replace semi_table ~key ~data:res; res
    | Some(res) -> res

end

let inv_table = Hashtbl.create ~hashable:String.hashable ()

(* Map rule_name[p1][p2][p3] to its actual rule instant *)
let rule_insts_table = Hashtbl.create ~hashable:String.hashable ()



  


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
      let ConcreteRule(name, ps) = crule in
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
  match Choose.can_imply n1 n2, Choose.can_imply n2 n1 with
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
  if obligation = inv_inst || symmetry_form obligation inv_inst = 0 then
    ([], deal_with_case_2 crule cinv)
  (* case 1 *)
  else if is_tautology (imply (simplify form) (simplify (neg obligation))) then
    ([], deal_with_case_1 crule cinv)
  (* case 3 *)
  else begin
    deal_with_case_3 crule cinv obligation old_invs
  end


let compute_rule_inst_names rname_paraminfo_pairs prop_pds =
  List.concat (List.map rname_paraminfo_pairs ~f:(fun (rname, rpds) ->
    match rpds with
    | [] -> [rname]
    | _ ->
      SemiPerm.gen_paramfixes prop_pds rpds
      |> List.map ~f:(fun pfs -> 
        get_rule_inst_name rname pfs
      )
  ))

let get_res_of_cinv cinv rname_paraminfo_pairs old_invs =
  let (ConcreteProp(Prop(_, prop_pds, _), _)) = cinv in
  let rule_inst_names = compute_rule_inst_names rname_paraminfo_pairs prop_pds in
  let crules = 
    List.map rule_inst_names ~f:(fun n -> 
      match Hashtbl.find rule_insts_table n with
      | None -> Prt.error n; raise Empty_exception
      | Some(cr) -> cr
    )
    |> List.concat
  in
  List.map crules ~f:(tabular_expans ~cinv ~old_invs)
  |> List.unzip

let get_real_new_invs new_invs =
  List.concat new_invs
  |> List.map ~f:simplify
  |> List.map ~f:minify_inv_inc
  |> List.dedup ~compare:symmetry_form
  |> List.map ~f:(normalize ~types:(!type_defs))

let read_res_cache cinvs =
  let cinv_file_name = sprintf "%s.cinvs.cache" (!protocol_name) in
  let all_cinv_file_name = sprintf "%s.all.cinvs.cache" (!protocol_name) in
  let invlib_file_name = sprintf "%s.inv.lib.cache" (!protocol_name) in
  let rel_file_name = sprintf "%s.relations.cache" (!protocol_name) in
  let id_file_name = sprintf "%s.id.cache" (!protocol_name) in
  let try_with_default filename convertor default =
    try
      In_channel.read_lines filename
      |> List.map ~f:Sexp.of_string
      |> List.map ~f:convertor
    with
    | Sys_error(_) -> default
    | e -> raise e
  in
  let cinvs' = try_with_default cinv_file_name (concrete_prop_of_sexp) cinvs in
  let cinvs_all = try_with_default all_cinv_file_name (concrete_prop_of_sexp) [] in
  let init_lib =
    let default = List.map cinvs ~f:(fun cinv -> concrete_prop_2_form cinv) in
    try_with_default invlib_file_name (formula_of_sexp) default
  in
  let relations = try_with_default rel_file_name (t_of_sexp) [] in
  let new_inv_id = 
    try 
      Int.of_string (In_channel.read_all id_file_name) 
    with | _ -> List.length cinvs in 
  cinvs', cinvs_all, init_lib, relations, new_inv_id

let write_res_cache cinvs cinv real_new_invs relations new_inv_id () =
  let cinv_file_name = sprintf "%s.cinvs.cache" (!protocol_name) in
  let all_cinv_file_name = sprintf "%s.all.cinvs.cache" (!protocol_name) in
  let invlib_file_name = sprintf "%s.inv.lib.cache" (!protocol_name) in
  let rel_file_name = sprintf "%s.relations.cache" (!protocol_name) in
  let id_file_name = sprintf "%s.id.cache" (!protocol_name) in
  let cinvs_sexp_str = 
    cinvs
    |> List.map ~f:sexp_of_concrete_prop
    |> List.map ~f:Sexp.to_string
  in
  let all_cinv_sexp_str = Sexp.to_string (sexp_of_concrete_prop cinv) in
  let real_new_inv_sexp_str =
    real_new_invs
    |> List.map ~f:sexp_of_formula
    |> List.map ~f:Sexp.to_string
  in
  let relations_sexp_str =
    relations
    |> List.map ~f:sexp_of_t
    |> List.map ~f:Sexp.to_string
  in
  let id_sexp_str = sprintf "%d" new_inv_id in
  let append_file fn lines () =
    let f = Out_channel.create fn ~append:true in
    (Out_channel.output_lines f lines; Out_channel.close f);
  in (
    Out_channel.write_lines cinv_file_name cinvs_sexp_str;
    append_file all_cinv_file_name [all_cinv_sexp_str] ();
    append_file invlib_file_name real_new_inv_sexp_str ();
    append_file rel_file_name relations_sexp_str ();
    Out_channel.write_all id_file_name ~data:id_sexp_str
  )


(* Find new inv and relations with concrete rules and a concrete invariant *)
let tabular_rules_cinvs rname_paraminfo_pairs cinvs =
  let rec wrapper cinvs cinvs_all new_inv_id old_invs relations =
    match cinvs with
    | [] -> (cinvs_all, relations)
    | cinv::cinvs' ->
      let (new_invs, new_relation) = get_res_of_cinv cinv rname_paraminfo_pairs old_invs in
      let real_new_invs = get_real_new_invs new_invs in
      if real_new_invs = [] then () else begin
        print_endline (String.concat ~sep:"\n" (
          List.map real_new_invs ~f:(fun f -> "NewInv: "^ToStr.Smv.form_act f)
        ))
      end;
      let rec invs_to_cinvs invs cinvs new_inv_id =
        match invs with
        | [] -> (cinvs, new_inv_id)
        | inv::invs' ->
          let cinv = form_2_concreate_prop ~id:new_inv_id (simplify inv) in
          invs_to_cinvs invs' (cinv::cinvs) (new_inv_id + 1)
      in
      let (new_cinvs, new_inv_id') = invs_to_cinvs real_new_invs [] new_inv_id in
      let cinvs'' = cinvs'@new_cinvs in
      write_res_cache cinvs'' cinv real_new_invs new_relation new_inv_id' ();
      wrapper cinvs'' (cinvs_all@[cinv]) new_inv_id' (old_invs@real_new_invs) (relations@new_relation)
  in
  let cinvs, cinvs_all, init_lib, relations, new_inv_id = read_res_cache cinvs in
  Prt.warning ("initial invs:\n"^String.concat ~sep:"\n" (
    List.map init_lib ~f:(fun f -> ToStr.Smv.form_act f)
  ));
  wrapper cinvs cinvs_all new_inv_id init_lib relations



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

let result_to_str (cinvs, relations) =
  let invs_str =
    cinvs
    |> List.map ~f:concrete_prop_2_form
    |> List.map ~f:simplify
    |> List.map ~f:ToStr.Smv.form_act
  in
  let relations_str = List.map relations ~f:to_str in
  String.concat ~sep:"\n" (relations_str@invs_str)

(** Find invs and causal relations of a protocol

    @param protocol the protocol
    @param prop_params property parameters given
    @return causal relation table
*)
let find ?(smv="") ?(smv_bmc="") ?(murphi="") protocol =
  let {name; types; vardefs; init=_init; rules; properties} = Loach.Trans.act ~loach:protocol in
  let _smt_context = Smt.set_context name (ToStr.Smt2.context_of ~types ~vardefs) in
  let _mu_context = Murphi.set_context name murphi in
  let _smv_bmc_context =
    if smv_bmc = "" then
      SmvBmc.set_context name (Loach.ToSmv.protocol_act ~limit_param:false protocol)
    else begin SmvBmc.set_context name smv_bmc end
  in
  let _smv_context =
    if smv = "" then Smv.set_context name (Loach.ToSmv.protocol_act protocol)
    else begin Smv.set_context name smv end
  in
  (type_defs := types; protocol_name := name);
  let cinvs =
    List.concat (List.map properties ~f:simplify_prop)
    |> List.map ~f:form_2_concreate_prop
  in
  let get_rulename_nparam_pair r =
    let Paramecium.Rule(rname, paramdefs, _, _) = r in
    let ps = cart_product_with_paramfix paramdefs (!type_defs) in
    let raw_insts_of_p = rule_2_concrete r ps in
    let rec store_table raw_insts_of_p =
      match raw_insts_of_p with
      | [] -> ()
      | (crs, p)::raw_insts_of_p' ->
        let key = get_rule_inst_name rname p in
      Hashtbl.replace rule_insts_table ~key ~data:crs;
      store_table raw_insts_of_p'
    in
    store_table raw_insts_of_p;
    (rname, paramdefs)
  in
  let rname_paraminfo_pairs = List.map rules ~f:get_rulename_nparam_pair in
  let result = tabular_rules_cinvs rname_paraminfo_pairs cinvs in
  printf "%s\n" (result_to_str result); result
