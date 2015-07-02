(** Language for cache coherence protocols in support of
    parameterization and local variables

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils
open Core.Std

open Paramecium

(** Unexhausted instantiation
    This exception should never be raised. Once raised, There should be a bug in this tool.
*)
exception Unexhausted_inst

(** Global variable *)
let global name = arr name []

(** Record definition *)
let record_def name paramdefs vardefs =
    List.map vardefs ~f:(fun (Arrdef(n, pds, t)) ->
      arrdef (sprintf "%s.%s" name n) (List.concat [paramdefs; pds]) t
    )

(** Record *)
let record vars =
  let named_vars = List.map vars ~f:(fun (Arr(name, prs)) -> (name, prs)) in
  let names, prs = List.unzip named_vars in
  arr (String.concat names ~sep:".") (List.concat prs)

type formula =
  | Chaos
  | Miracle
  | Eqn of exp * exp
  | Neg of formula
  | AndList of formula list
  | OrList of formula list
  | Imply of formula * formula
  | ForallFormula of typedef list * paramdef list * formula
  | ExistFormula of typedef list * paramdef list * formula

let chaos = Chaos
let miracle = Miracle
let eqn e1 e2 = Eqn(e1, e2)
let neg f = Neg f
let andList fs = AndList fs
let orList fs = OrList fs
let imply f1 f2 = Imply(f1, f2)
let forallFormula ~types paramdefs form = ForallFormula(types, paramdefs, form)
let existFormula ~types paramdefs form = ExistFormula(types, paramdefs, form)

(** Assignment statements *)
type statement =
  | Assign of var * exp
  | Parallel of statement list
  | IfStatement of formula * statement
  | IfelseStatement of formula * statement * statement
  | ForStatement of statement * paramdef list

let assign v e = Assign(v, e)
let parallel statements = Parallel statements
let ifStatement form statement = IfStatement(form, statement)
let ifelseStatement form s1 s2 = IfelseStatement(form, s1, s2)
let forStatement s paramdefs = ForStatement(s, paramdefs)

(** Represents rules which consists of guard and assignments
    + Rule: name, parameters, guard, assignments
*)
type rule = 
  | Rule of string * paramdef list * formula * statement

let rule name paramdef f s = Rule(name, paramdef, f, s)

type prop =
  | Prop of string * paramdef list * formula

let prop name paramdef f = Prop(name, paramdef, f)

(** Represents the whole protocol *)
type protocol = {
  name: string;
  types: typedef list;
  vardefs: vardef list;
  init: statement;
  rules: rule list;
  properties: prop list;
}

let rec apply_form f ~p =
  match f with
  | Chaos
  | Miracle -> f
  | Eqn(e1, e2) -> eqn (apply_exp e1 ~p) (apply_exp e2 ~p)
  | Neg(form) -> neg (apply_form form ~p)
  | AndList(fl) -> andList (List.map fl ~f:(apply_form ~p))
  | OrList(fl) -> orList (List.map fl ~f:(apply_form ~p))
  | Imply(f1, f2) -> imply (apply_form f1 ~p) (apply_form f2 ~p)
  | ForallFormula(types, paramdefs, form) -> forallFormula ~types paramdefs (apply_form form ~p)
  | ExistFormula(types, paramdefs, form) -> existFormula ~types paramdefs (apply_form form ~p)

let rec apply_statement statement ~p ~types =
  match statement with
  | Assign(v, e) -> assign (apply_array v ~p) (apply_exp e ~p)
  | Parallel(sl) -> parallel (List.map sl ~f:(apply_statement ~p ~types))
  | IfStatement(f, s) -> ifStatement (apply_form f ~p) (apply_statement s ~p ~types)
  | IfelseStatement(f, s1, s2) ->
    ifelseStatement (apply_form f ~p) (apply_statement s1 ~p ~types) (apply_statement s2 ~p ~types)
  | ForStatement(s, pd) ->
    let s' = apply_statement s ~p ~types in
    let pfs = cart_product_with_paramfix pd types in
    parallel (List.map pfs ~f:(fun p -> apply_statement s' ~p ~types))

let rec eliminate_for statement ~types =
  match statement with
  | Assign(_) -> statement
  | Parallel(sl) -> parallel (List.map sl ~f:(eliminate_for ~types))
  | IfStatement(f, s) -> ifStatement f (eliminate_for s ~types)
  | IfelseStatement(f, s1, s2) ->
    ifelseStatement f (eliminate_for s1 ~types) (eliminate_for s2 ~types)
  | ForStatement(s, pd) ->
    let pfs = cart_product_with_paramfix pd types in
    parallel (List.map pfs ~f:(fun p -> apply_statement s ~p ~types))

let apply_rule r ~p ~types =
  let Rule(n, paramdefs, f, s) = r in
  let name =
    if p = [] then n
    else begin
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
      sprintf "%s%s" n (String.concat (List.map p ~f:paramref_act))
    end
  in
  rule name [] (apply_form f ~p) (apply_statement s ~p ~types)

let apply_prop property ~p =
  let Prop(name, paramdefs, f) = property in
  prop name [] (apply_form f ~p)

let rule_to_insts r ~types =
  let Rule(n, pd, f, s) = r in
  let ps = cart_product_with_paramfix pd types in
  if pd = [] then
    [rule n pd f (eliminate_for s ~types)]
  else begin
    List.map ps ~f:(fun p -> apply_rule r ~p ~types)
  end


let analyze_if statement guard ~types =
  let nofor = eliminate_for statement ~types in
  let rec wrapper statement ~m ~g =
    match statement with
    | Assign(v, e) ->
      let key = ToStr.Debug.var_act v in 
      let data = (
        match String.Map.find m key with
        | None -> (v, [(g, e)])
        | Some(v, exps) -> (v, (g, e)::exps)
      ) in
      String.Map.add m ~key ~data
    | Parallel(sl) ->
      let rec wrap_parallel sl m =
        match sl with
        | [] -> m
        | s::sl' -> wrap_parallel sl' (wrapper s ~m ~g)
      in
      wrap_parallel sl m
    | IfStatement(f, s) -> wrapper s ~m ~g:(andList [f; g])
    | IfelseStatement(f, s1, s2) ->
      let if_part = wrapper s1 ~m ~g:(andList [f; g]) in
      wrapper s2 ~m:if_part ~g:(andList [neg f; g])
    | ForStatement(_) -> raise Empty_exception
  in
  let m = wrapper nofor ~m:String.Map.empty ~g:guard in
  let keys = String.Map.keys m in
  List.map keys ~f:(fun k -> String.Map.find_exn m k)



















(*----------------------------- Translate module ---------------------------------*)

(** Translate language of this level to the next lower level *)
module Trans = struct

  exception Unexhausted_flatten

  (* Translate data structures from Loach to Paramecium *)

  let rec trans_formula form =
    match form with
    | Chaos -> Paramecium.chaos
    | Miracle -> Paramecium.miracle
    | Eqn(e1, e2) -> Paramecium.eqn e1 e2
    | Neg(f) -> Paramecium.neg (trans_formula f)
    | AndList(fl) -> Paramecium.andList (List.map fl ~f:trans_formula)
    | OrList(fl) -> Paramecium.orList (List.map fl ~f:trans_formula)
    | Imply(f1, f2) -> Paramecium.imply (trans_formula f1) (trans_formula f2)
    | ForallFormula(types, paramdefs, f) -> 
      let ps = cart_product_with_paramfix paramdefs types in
      let f' = trans_formula f in
      Paramecium.andList (List.map ps ~f:(fun p -> Paramecium.apply_form ~p f'))
    | ExistFormula(types, paramdefs, f) -> 
      let ps = cart_product_with_paramfix paramdefs types in
      let f' = trans_formula f in
      Paramecium.orList (List.map ps ~f:(fun p -> Paramecium.apply_form ~p f'))

  let rec trans_statement ~types statement =
    match eliminate_for statement ~types with
    | Assign(v, e) -> [(Paramecium.chaos, Paramecium.assign v e)]
    | Parallel(slist) -> 
      cartesian_product (List.map slist ~f:(trans_statement ~types))
      |> List.map ~f:(fun x ->
        let gs, ss = List.unzip x in
        (Paramecium.andList gs, Paramecium.parallel ss)
      )
    | IfStatement(f, s) ->
        let translated = trans_statement ~types s in
        let translated_f = trans_formula f in
        let res1 = List.map translated ~f:(fun (f', s') -> 
          (Paramecium.andList [translated_f; f'], s')) in
        let res2 = List.map translated ~f:(fun _ -> 
          (Paramecium.neg translated_f, Paramecium.parallel [])) in
        List.concat [res1; res2]
    | IfelseStatement(f, s1, s2) ->
        let translated1 = trans_statement ~types s1 in
        let translated_f = trans_formula f in
        let res1 = List.map translated1 ~f:(fun (f', s') -> 
          (Paramecium.andList [translated_f; f'], s')) in
        let translated2 = trans_statement ~types s2 in
        let res2 = List.map translated2 ~f:(fun (f', s') -> 
          (Paramecium.andList [Paramecium.neg translated_f; f'], s')) in
        List.concat [res1; res2]
    | ForStatement(_) -> raise Empty_exception

  let flatten_statement s =
    let rec wrapper s =
      match s with
      | Paramecium.Assign(_) -> [s]
      | Paramecium.Parallel(ss) -> List.concat (List.map ss ~f:wrapper)
    in
    Paramecium.parallel (wrapper s)

  (* remove duplicated assignments to a var, the last assignment will be reserved *)
  let remove_dup_assigns s =
    let flattened = flatten_statement s in
    let do_work s m =
      match s with
      | Paramecium.Assign(v, _) -> String.Map.add m ~key:(ToStr.Debug.var_act v) ~data:s
      | _ -> raise Unexhausted_flatten
    in
    match flattened with
    | Paramecium.Assign(_) -> raise Unexhausted_flatten
    | Paramecium.Parallel(ss) ->
      let rec wrapper ss m =
        match ss with
        | [] -> m
        | s::ss' -> wrapper ss' (do_work s m)
      in
      let m = wrapper ss String.Map.empty in
      Paramecium.parallel (List.map (String.Map.keys m) ~f:(fun k -> String.Map.find_exn m k))

  let trans_prop property =
    let Prop(n, p, f) = property in
    Paramecium.prop n p (trans_formula f)

  let trans_rule ~types r =
    match r with
    | Rule(n, p, f, s) ->
      let guarded_s = 
        trans_statement ~types s
        |> List.map ~f:(fun (g, s) -> g, remove_dup_assigns s)
        |> List.filter ~f:(fun (_, s) -> 
          match s with | Paramecium.Parallel([]) -> false | _ -> true)
      in
      let indice = up_to (List.length guarded_s) in
      List.map2_exn guarded_s indice ~f:(fun (g, s) i -> 
        Paramecium.rule (sprintf "%s_%d" n i) p (Paramecium.andList [trans_formula f; g]) s)

  (** Translate language of Loach to Paramecium

      @param loach cache coherence protocol written in Loach
      @return the protocol in Paramecium
  *)
  let act ~loach:{name; types; vardefs; init; rules; properties} =
    let new_init = 
      let _, ss = List.unzip (trans_statement ~types init) in
      Paramecium.parallel ss
    in
    let new_prop = List.map properties ~f:trans_prop in
    let new_rules = List.concat (List.map rules ~f:(trans_rule ~types)) in
    Prt.info "Done\n";
    { Paramecium.name = name;
      types = types;
      vardefs = vardefs;
      init = new_init;
      rules = new_rules;
      properties = new_prop;
    }

end















(*********************************** Module Variable Names, with Param values *****************)

(** Get variable names in the components *)
module VarNamesWithParam = struct
  
  open String.Set

  let of_var_ref = ref (fun _x -> of_list [])

  (** Names of exp *)
  let of_exp e =
    match e with
    | Const(_)
    | Param(_) -> of_list []
    | Var(v) -> (!of_var_ref) v

  (** Names of formula *)
  let rec of_form f =
    match f with
    | Paramecium.Chaos
    | Miracle -> of_list []
    | Eqn(e1, e2) -> union_list [of_exp e1; of_exp e2]
    | Neg(form) -> of_form form
    | AndList(fl)
    | OrList(fl) -> union_list (List.map fl ~f:(of_form))
    | Imply(f1, f2) -> union_list [of_form f1; of_form f2]


  let rec of_statement s =
    match s with
    | Assign(v, e) -> union_list [(!of_var_ref) v; of_exp e]
    | Parallel(slist) -> union_list (List.map slist ~f:(of_statement))
    | IfStatement(f, s) -> union_list [of_form (Trans.trans_formula f); of_statement s]
    | IfelseStatement(f, s1, s2) -> union_list [of_form (Trans.trans_formula f); 
      of_statement s1; of_statement s2]
    | ForStatement(_) -> raise Empty_exception

  let of_rule ~of_var r = 
    of_var_ref := of_var;
    match r with
    | Rule(_, _, f, s) ->
      let f' = Trans.trans_formula f in
      union_list [of_form f'; of_statement s]
end













module ToSmv = struct
  open ToStr.Smv
  open Formula

  let types_ref = ref []

  let statement_act ?(is_init=false) statement guard =
    let analyzed = analyze_if statement guard ~types:(!types_ref) in
    let trans_assigns v guarded_exps =
      let rec wrapper guarded_exps cur_str =
        match guarded_exps with
        | [] -> cur_str
        | (g, e)::guarded_exps' ->
          let gstr = form_act (simplify (Trans.trans_formula g)) in
          let estr = exp_act e in
          let cur_str' =
            if gstr = "FALSE" then
              cur_str
            else begin
              sprintf "%s%s : %s;\n" cur_str gstr estr
            end
          in
          if gstr = "TRUE" then
            cur_str'
          else begin
            wrapper guarded_exps' cur_str'
          end
      in
      let vstr = var_act v in
      let conditions = wrapper guarded_exps "" in
      if is_init then
        sprintf "init(%s) := case\n%sesac;" vstr conditions
      else begin
        sprintf "next(%s) := case\n%sTRUE : %s;\nesac;" vstr conditions vstr
      end
    in
    List.map analyzed ~f:(fun (v, guarded_exps) -> trans_assigns v guarded_exps)
    |> String.concat ~sep:"\n"

  let init_act statement =
    statement_act statement ~is_init:true chaos

  let rule_act r =
    let escape n =
      String.substr_replace_all n ~pattern:"[" ~with_:"__"
      |> String.substr_replace_all ~pattern:"]" ~with_:""
      |> String.substr_replace_all ~pattern:"." ~with_:"__"
    in
    let vars = String.Set.to_list (VarNamesWithParam.of_rule r ~of_var:(fun v ->
      String.Set.of_list [var_act v]
    )) in
    let vars_str = String.concat vars ~sep:", " in
    (* rule process instance *)
    let Rule(n, _, f, s) = r in
    let name = escape n
    in
    let rule_proc_inst = sprintf "%s : process Proc__%s(%s);" name name vars_str in
    (* rule process *)
    let statement_str = escape (statement_act s f) in
    let rule_proc = 
      sprintf "MODULE Proc__%s(%s)\nASSIGN\n%s" name (escape vars_str) statement_str
    in
    (* result *)
    (rule_proc_inst, rule_proc)

  let prop_act property =
    let Prop(_, _, f) = property in
    sprintf "SPEC\n  AG (!%s)" (form_act (simplify (Trans.trans_formula f)))

  let protocol_act {name=_; types; vardefs; init; rules; properties} =
    types_ref := types;
    let property_strs = [""] (* List.map properties ~f:prop_act *) in
    let rule_insts = List.concat (List.map rules ~f:(rule_to_insts ~types)) in
    let rule_proc_insts, rule_procs = List.unzip (List.map rule_insts ~f:(rule_act)) in
    let vardef_str =
      sprintf "VAR\n%s" (String.concat ~sep:"\n" (List.map vardefs ~f:(vardef_act ~types)))
    in
    let rule_proc_insts_str = String.concat ~sep:"\n\n" rule_proc_insts in
    let init_str = sprintf "ASSIGN\n%s" (init_act (eliminate_for init ~types)) in
    let prop_str = String.concat ~sep:"\n\n" property_strs in
    let rule_procs_str = String.concat ~sep:"\n\n---------\n\n" rule_procs in
    let strs = [vardef_str; rule_proc_insts_str; init_str; prop_str] in
    let main_module = 
      sprintf "MODULE main\n%s" (String.concat ~sep:"\n\n--------------------\n\n" strs)
    in
    sprintf "%s\n\n--------------------\n\n%s" main_module rule_procs_str


end
