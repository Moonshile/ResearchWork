

open Utils
open Core.Std
open Paramecium
open Loach

exception Unsupported of string

let types_ref = ref []

let gen_tmp_vars n =
  let nums = up_to n in
  List.map nums ~f:(fun i -> sprintf "i%d" i)

let const_act c =
  match c with
  | Boolc(b) ->
    Some (sprintf "definition %b::\"scalrValueType\" where [simp]: \"%b\\<equiv> boolV %s\""
      b b (if b then "True" else "False")
    )
  | Strc(s) ->
    Some (sprintf "definition %s::\"scalrValueType\" where [simp]: \"%s\\<equiv> enum ''control'' ''%s''\""
      s s s
    )
  | Intc(i) -> None

let const_to_str c =
  match c with
  | Boolc(b) -> sprintf "%b" b
  | Strc(s) -> s
  | Intc(i) -> sprintf "%d" i

let type_act (Enum(name, consts)) =
  let const_strs = List.filter_map consts ~f:const_act in
  match const_strs with
  | [] -> None
  | _ -> Some (String.concat ~sep:"\n" const_strs)

let var_act (Arr(name, prs)) =
  let ident = sprintf "(Ident ''%s'')" name in
  match prs with
  | [] -> ident
  | _ -> List.fold prs ~init:ident ~f:(fun res x -> sprintf "(Para %s %s)" res (name_of_param x))

let paramref_to_index pr =
  match pr with
  | Paramref(name) -> name
  | Paramfix(_, _, c) -> (
    match c with
    | Intc(i) -> sprintf "%d" i
    | _ -> raise (Unsupported("Non-integer indexes are not supported yet"))
  )

let exp_act e =
  match e with
  | Const(c) -> sprintf "(Const %s)" (const_to_str c)
  | Var(v) -> sprintf "(IVar %s)" (var_act v)
  | Param(pr) -> sprintf "(Const (index %s))" (paramref_to_index pr)

let rec formula_act f =
  match f with
  | Chaos -> "chaos"
  | Miracle -> "miracle"
  | Eqn(e1, e2) -> sprintf "(eqn %s %s)" (exp_act e1) (exp_act e2)
  | Neg(g) -> sprintf "(neg %s)" (formula_act g)
  | AndList(fl) -> (
    match fl with
    | [] -> formula_act chaos
    | [g] -> formula_act g
    | f1::f2::fl' -> 
      let init = sprintf "(andForm %s %s)" (formula_act f1) (formula_act f2) in
      List.fold fl' ~init ~f:(fun res x -> sprintf "(andForm %s %s)" res (formula_act x))
  )
  | OrList(fl) -> (
    match fl with
    | [] -> formula_act miracle
    | [g] -> formula_act g
    | f1::f2::fl' -> 
      let init = sprintf "(orForm %s %s)" (formula_act f1) (formula_act f2) in
      List.fold fl' ~init ~f:(fun res x -> sprintf "(orForm %s %s)" res (formula_act x))
  )
  | Imply(f1, f2) -> sprintf "(implyForm %s %s)" (formula_act f1) (formula_act f2)
  | ForallFormula(paramdefs, form) ->
    begin
      match paramdefs with
      | [] -> raise Empty_exception
      | [Paramdef(name, tname)] ->
        let type_range = name2type ~tname ~types:(!types_ref) in
        let num = List.length (List.filter type_range ~f:(fun c -> not (c = intc 0))) in
        let form_str = formula_act form in
        sprintf "(forallForm (down %d) (\\<lambda>%s. %s))" num name form_str
      | _ -> raise (Unsupported "More than 1 paramters in forall are not supported yet")
    end
  | ExistFormula(paramdefs, form) ->
    begin
      match paramdefs with
      | [] -> raise Empty_exception
      | [Paramdef(name, tname)] ->
        let type_range = name2type ~tname ~types:(!types_ref) in
        let num = List.length (List.filter type_range ~f:(fun c -> not (c = intc 0))) in
        let form_str = formula_act form in
        sprintf "(existsForm (down %d) (\\<lambda>%s. %s))" num name form_str
      | _ -> raise (Unsupported "More than 1 paramters in exists are not supported yet")
    end

let get_var_of_balanced s =
  match s with
  | Assign(v, _)
  | IfelseStatement(_, Assign(v, _), _)
  | ForStatement(Assign(v, _), _)
  | ForStatement(IfelseStatement(_, Assign(v, _), _), _) -> v
  | _ -> raise Empty_exception

let get_vname_map_of_balanced sl =
  let rec wrapper sl m =
    match sl with
    | [] -> m
    | s::sl' ->
      let v = get_var_of_balanced s in
      let key = ToStr.Debug.var_act v in
      wrapper sl' (String.Map.add m ~key ~data:s)
  in
  wrapper sl String.Map.empty

let merge_ifelse f s1 s2 =
  match (s1, s2) with
  | (ForStatement(s, pd), ForStatement(s', _)) ->
    (* TODO need to check pds? *)
    forStatement (ifelseStatement f s s') pd
  | (_, ForStatement(s, pd)) -> forStatement (ifelseStatement f s1 s) pd
  | (ForStatement(s, pd), _) -> forStatement (ifelseStatement f s s2) pd
  | (Assign(_), Assign(_))
  | (Assign(_), IfelseStatement(_))
  | (IfelseStatement(_), Assign(_))
  | (IfelseStatement(_), IfelseStatement(_)) -> ifelseStatement f s1 s2
  | _ -> raise Empty_exception

let rec balance_ifstatement statement =
  match statement with
  | Assign(_) -> [statement]
  | Parallel(sl) -> List.concat (List.map sl ~f:balance_ifstatement)
  | IfStatement(f, s) -> balance_ifstatement (ifelseStatement f s (parallel []))
  | IfelseStatement(f, s1, s2) ->
    let bs1 = balance_ifstatement s1 in
    let bs2 = balance_ifstatement s2 in
    let names1 = get_vname_map_of_balanced bs1 in
    let names2 = get_vname_map_of_balanced bs2 in
    let rec wrapper pair res =
      match pair with
      | ([], []) -> res
      | ([], b2::bs2') ->
        let v = get_var_of_balanced b2 in
        let next_s = assign v (var v) in
        wrapper ([], bs2') (merge_ifelse f next_s b2::res)
      | (b1::bs1', bs2') ->
        let v = get_var_of_balanced b1 in
        let key = ToStr.Debug.var_act v in
        let next_s =
          match String.Map.find names2 key with
          | None -> assign v (var v)
          | Some(s) -> s
        in
        wrapper (bs1', bs2') (merge_ifelse f b1 next_s::res)
    in
    wrapper (bs1, bs2) []
  | ForStatement(s, pd) ->
    let bs = balance_ifstatement s in
    List.map bs ~f:(fun s' -> forStatement s' pd)

let statement_act statement =
  let balanced = balance_ifstatement statement in
  let trans bs =
    match bs with
    | Assign(v, e) -> sprintf "(assign %s %s)" (var_act v) (exp_act e)
    | IfelseStatement(f, Assign(v, e1), Assign(_, e2)) ->
      sprintf "(assign %s (iteForm %s %s %s))" (var_act v) (formula_act f) (exp_act e1) (exp_act e2)
    | ForStatement(Assign(v, e), pd) ->
      begin
        match pd with
        | [] -> raise Empty_exception
        | [Paramdef(name, tname)] ->
          let type_range = name2type ~tname ~types:(!types_ref) in
          let s_str = sprintf "(assign %s %s)" (var_act v) (exp_act e) in
          sprintf "(forallSent (down N) (\\<lambda>%s. %s))" name s_str
        | _ -> raise (Unsupported "More than 1 paramters in exists are not supported yet")
      end
    | ForStatement(IfelseStatement(f, Assign(v, e1), Assign(_, e2)), pd) ->
      begin
        match pd with
        | [] -> raise Empty_exception
        | [Paramdef(name, tname)] ->
          let type_range = name2type ~tname ~types:(!types_ref) in
          let s_str = sprintf "(assign %s (iteForm %s %s %s))"
            (var_act v) (formula_act f) (exp_act e1) (exp_act e2) in
          sprintf "(forallSent (down N) (\\<lambda>%s. %s))" name s_str
        | _ -> raise (Unsupported "More than 1 paramters in exists are not supported yet")
      end
    | _ -> raise Empty_exception
  in
  sprintf "(parallelList [%s])" (String.concat ~sep:", " (List.map balanced ~f:trans))

let rule_act r =
  let Rule(name, pd, f, s) = r in
  let pd_count_t = List.map pd ~f:(fun _ -> "nat") in
  let pd_str = String.concat ~sep:" \\<Rightarrow> " pd_count_t in
  let rule_type = sprintf "%s \\<Rightarrow> rule" pd_str in
  let pd_names = String.concat ~sep:" " (List.map pd ~f:(fun (Paramdef(n, _)) -> n)) in
  let guard = formula_act f in
  let statements = statement_act s in
  sprintf "definition %s::\"%s\" where [simp]:
\"%s %s\\<equiv>\nlet g = %s in\nlet s = %s in\nguard g s\""
    name rule_type name pd_names guard statements

let rules_act rs =
  let rstrs = String.concat ~sep:"\n\n" (List.map rs ~f:rule_act) in
  let r_insts_str = String.concat ~sep:" \\<or>\n" (
    List.map rs ~f:(fun (Rule(name, pd, _, _)) ->
      let n = List.length pd in
      let tmp_vars = String.concat (List.map (gen_tmp_vars n) ~f:(fun t -> sprintf "%%%s." t)) in
      sprintf "ex%dP N (%% %s. r=%s %s)" n tmp_vars name tmp_vars
    )
  ) in
  sprintf "%s\n\ndefinition rules::\"nat \\<Rightarrow> rule set\" where [simp]:
\"rules N \\<equiv> {r.\n%s\n}\"" rstrs r_insts_str

let rec paramecium_form_to_loach form =
  match form with
  | Paramecium.Chaos -> chaos
  | Paramecium.Miracle -> miracle
  | Paramecium.Eqn(e1, e2) -> eqn e1 e2
  | Paramecium.Neg(f) -> neg (paramecium_form_to_loach f)
  | Paramecium.AndList(fl) -> andList (List.map fl ~f:paramecium_form_to_loach)
  | Paramecium.OrList(fl) -> orList (List.map fl ~f:paramecium_form_to_loach)
  | Paramecium.Imply(f1, f2) -> imply (paramecium_form_to_loach f1) (paramecium_form_to_loach f2)

let inv_act inv i =
  let (pds, pfs, gened_inv) = Generalize.form_act inv in
  let gened_inv' = paramecium_form_to_loach gened_inv in
  let has_not_sym = List.exists pfs ~f:(fun (Paramfix(_, _, c)) -> c = intc 0) in
  let pds' =
    if has_not_sym then
      let Paramfix(name, _, _) = List.find_exn pfs ~f:(fun (Paramfix(_, _, c)) -> c = intc 0) in
      List.filter pds ~f:(fun (Paramdef(n, _)) -> not (n = name))
    else begin pds end
  in
  let gened_inv'' =
    if has_not_sym then
      let not_sym_pf = List.find_exn pfs ~f:(fun (Paramfix(_, _, c)) -> c = intc 0) in
      apply_form gened_inv' ~p:[not_sym_pf]
    else begin gened_inv' end
  in
  let pd_count_t = List.map pds' ~f:(fun _ -> "nat") in
  let pd_str = String.concat ~sep:" \\<Rightarrow> " pd_count_t in
  let inv_type = sprintf "%s \\<Rightarrow> formula" pd_str in
  let pd_names = String.concat ~sep:" " (List.map pds' ~f:(fun (Paramdef(n, _)) -> n)) in
  List.length pds', sprintf "definition inv%d::\"%s\" where [simp]:
\"inv%d %s \\<equiv>\n%s\"" i inv_type i pd_names (formula_act gened_inv'')

let invs_act invs =
  let invs_no = up_to (List.length invs) in
  let invs_with_pd_count = List.map2_exn invs invs_no ~f:inv_act in
  let inv_strs = String.concat ~sep:"\n\n" (List.map invs_with_pd_count ~f:(fun (_, s) -> s)) in
  let inv_insts_str = String.concat ~sep:" \\<or>\n" (
    List.map2_exn invs_no invs_with_pd_count ~f:(fun i (n, _) ->
      let tmp_vars = String.concat (List.map (gen_tmp_vars n) ~f:(fun t -> sprintf "%%%s." t)) in
      sprintf "ex%dP N (%% %s. f=inv%d %s)" i tmp_vars i tmp_vars
    )
  ) in
  sprintf "%s\n\ndefinition invariants::\"nat \\<Rightarrow> formula set\" where [simp]:
\"invariants N \\<equiv> {f.\n%s\n}\"" inv_strs inv_insts_str

let init_act statement i =
  let quant, body =
    match statement with
    | Assign(v, e) -> "", sprintf "(eqn %s %s)" (exp_act (var v)) (exp_act e)
    | IfelseStatement(f, Assign(v, e1), Assign(_, e2)) ->
      "", sprintf "(eqn %s (iteForm %s %s %s))" (exp_act (var v)) (formula_act f) (exp_act e1) (exp_act e2)
    | ForStatement(Assign(v, e), pd) ->
      begin
        match pd with
        | [] -> raise Empty_exception
        | [Paramdef(name, tname)] ->
          let type_range = name2type ~tname ~types:(!types_ref) in
          let s_str = sprintf "(eqn %s %s)" (exp_act (var v)) (exp_act e) in
          "N", sprintf "(forallForm (down N) (%% %s . %s))" name s_str
        | _ -> raise (Unsupported "More than 1 paramters in exists are not supported yet")
      end
    | ForStatement(IfelseStatement(f, Assign(v, e1), Assign(_, e2)), pd) ->
      begin
        match pd with
        | [] -> raise Empty_exception
        | [Paramdef(name, tname)] ->
          let type_range = name2type ~tname ~types:(!types_ref) in
          let s_str = sprintf "(eqn %s (iteForm %s %s %s))"
            (exp_act (var v)) (formula_act f) (exp_act e1) (exp_act e2) in
          "N", sprintf "(forallForm (down N) (%% %s . %s))" name s_str
        | _ -> raise (Unsupported "More than 1 paramters in exists are not supported yet")
      end
    | _ -> raise Empty_exception
  in
  let init_type_str = if quant = "" then "formula" else begin "nat \\<Rightarrow> formula" end in
  quant, sprintf "definition initSpec%d::\"%s\" where [simp]:
\"initSpec%d %s \\<equiv> %s\"" i init_type_str i quant body

let inits_act statements =
  let balanced = balance_ifstatement statements in
  let init_no = up_to (List.length balanced) in
  let init_strs_with_quant = List.map2_exn balanced init_no ~f:init_act in
  let init_strs = String.concat ~sep:"\n\n" (List.map init_strs_with_quant ~f:(fun (_, s) -> s)) in
  let init_insts_str = String.concat ~sep:",\n" (
    List.map2_exn init_no (List.map init_strs_with_quant ~f:(fun (q, _) -> q)) ~f:(fun i q ->
      sprintf "(initSpec%d %s)" i q
    )
  ) in
  sprintf "%s\n\ndefinition allInitSpecs::\"nat \\<Rightarrow> formula list\" where [simp]:
\"allInitSpecs N \\<equiv> [\n%s\n]\"" init_strs init_insts_str




let protocol_act {name; types; vardefs; init; rules; properties} invs =
  types_ref := types;
  let types_str = String.concat ~sep:"\n" (List.filter_map types ~f:type_act) in
  let rules_str = rules_act rules in
  let invs_str = invs_act invs in
  let inits_str = inits_act init in
  sprintf "\
theory %s imports localesDef\n\
begin\n\
section{*Main definitions*}\n\
%s\n\
%s\n\
%s\n\
%s\n\
end\n" name types_str rules_str invs_str inits_str
