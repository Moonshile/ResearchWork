(** Translate Paramecium to string of other languages

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils
open Paramecium

open Core.Std


(*----------------------------- Module To Debug String ----------------------------------*)

(** Translate to Debug string *)
module Debug = struct

  (* Translate a const to Debug const *)
  let const_act c =
    match c with
    | Intc(i) -> Int.to_string i
    | Strc(s) -> s
    | Boolc(b) -> String.uppercase (Bool.to_string b)

  let paramdef_act (Paramdef(vn, tn)) = sprintf "[def-%s:%s]" vn tn

  (** Translate a paramref to Debug string *)
  let paramref_act pr =
    match pr with
    | Paramfix(vname, tname, c) -> sprintf "[%s:%s:%s]" (const_act c) tname vname
    | Paramref(name) -> sprintf "[%s:__ref__]" name

  (* Translate a variable to Debug variable *)
  let var_act v =
    let Arr(name, params) = v in
    if params = [] then
      name
    else begin
      let actual_ps = List.map params ~f:paramref_act in
      sprintf "%s%s" name (String.concat actual_ps)
    end

  (* Translate an exp to Debug exp *)
  let exp_act exp =
    match exp with
    | Const(c) -> const_act c
    | Var(v) -> var_act v
    | Param(pr) -> paramref_act pr

  (** Translate formula to Debug string

      @param form the formula to be translated
      @return the Debug string
  *)
  let rec form_act form =
    match form with
    | Chaos -> "TRUE"
    | Miracle -> "FALSE"
    | Eqn(e1, e2) -> sprintf "(%s = %s)" (exp_act e1) (exp_act e2)
    | Neg(form) -> sprintf "(!%s)" (form_act form)
    | AndList(fl) ->
      List.map fl ~f:form_act
      |> reduce ~default:"TRUE" ~f:(fun res x -> sprintf "%s & %s" res x)
      |> sprintf "(%s)"
    | OrList(fl) ->
      List.map fl ~f:form_act
      |> reduce ~default:"FALSE" ~f:(fun res x -> sprintf "%s | %s" res x)
      |> sprintf "(%s)"
    | Imply(f1, f2) -> sprintf "(%s -> %s)" (form_act f1) (form_act f2)

end



















(*----------------------------- Module To SMT String ----------------------------------*)

(** Translate to smt2 string *)
module Smt2 = struct

  let const_is_strc c = 
    match c with
    | Strc(_) -> true
    | Intc(_) | Boolc(_) -> false

  let const_is_intc c = 
    match c with
    | Intc(_) -> true
    | Strc(_) | Boolc(_) -> false

  let const_is_boolc c = 
    match c with
    | Boolc(_) -> true
    | Intc(_) | Strc(_) -> false

  (* Translate a type definition to smt2 type definition *)
  let type_act t =
    let Enum(name, values) = t in
    let is_strc = all values ~f:const_is_strc in
    if is_strc then
      let strs = List.map values ~f:(fun c ->
        match c with
        | Strc(s) -> s
        | Intc(_) | Boolc(_) -> raise Empty_exception
      ) in
      sprintf "(declare-datatypes () ((%s %s)))" name (String.concat ~sep:" " strs)
    else
      ""
  
  (* Translate a variable definition to smt2 function definition *)
  let vardef_act vd ~types =
    let Arrdef(name, paramdefs, tname) = vd in
    let type_name tname =
      let consts = name2type ~tname ~types in
      if all consts ~f:const_is_strc then
        tname
      else if all consts ~f:const_is_intc then
        "Int"
      else
        "Bool"
    in
    let param_ts = List.map paramdefs ~f:(fun (Paramdef(_, t)) -> type_name t) in
    sprintf "(declare-fun %s (%s) %s)" name (String.concat ~sep:" " param_ts) (type_name tname)

  (* Translate a const to smt const *)
  let const_act c =
    match c with
    | Intc(i) -> Int.to_string i
    | Strc(s) -> s
    | Boolc(b) -> Bool.to_string b

  (* Translate a variable to smt2 function call *)
  let var_act v =
    let Arr(name, params) = v in
    if params = [] then
      name
    else begin
      let actual_ps = List.map params ~f:(fun p ->
        match p with
        | Paramfix(_, _, c) -> const_act c
        | Paramref(_) -> raise Unexhausted_inst
      ) in
      sprintf "(%s %s)" name (String.concat ~sep:" " actual_ps)
    end

  (* Translate an exp to smt2 exp *)
  let exp_act exp =
    match exp with
    | Const(c) -> const_act c
    | Var(v) -> var_act v
    | Param(Paramfix(_, _, c)) -> const_act c
    | Param(Paramref _) -> raise Unexhausted_inst

  (* Translate formula to smt2 string *)
  let rec form_act form =
    match form with
    | Chaos -> "true"
    | Miracle -> "false"
    | Eqn(e1, e2) -> sprintf "(= %s %s)" (exp_act e1) (exp_act e2)
    | Neg(form) -> sprintf "(not %s)" (form_act form)
    | AndList(fl) ->
      List.map fl ~f:form_act
      |> reduce ~default:"true" ~f:(fun res x -> sprintf "(and %s %s)" res x)
    | OrList(fl) ->
      List.map fl ~f:form_act
      |> reduce ~default:"false" ~f:(fun res x -> sprintf "(or %s %s)" res x)
    | Imply(f1, f2) -> sprintf "(=> %s %s)" (form_act f1) (form_act f2)

  let context_of ~types ~vardefs =
    let type_str =
      List.map types ~f:type_act
      |> List.filter ~f:(fun x -> not (x = ""))
      |> String.concat ~sep:"\n"
    in
    let vardef_str =
      List.map vardefs ~f:(vardef_act ~types)
      |> String.concat ~sep:"\n"
    in
    sprintf "%s%s" type_str vardef_str

  let form_of form =
    sprintf "(assert %s)" (form_act form)

  (** Translate to smt2 string

      @param types the type definitions of the protocol
      @param vardefs the variable definitions of the protocol
      @param form the formula to be translated
      @return the smt2 string
  *)
  let act ~types ~vardefs form =
    sprintf "%s\n%s\n(check-sat)" (context_of ~types ~vardefs) (form_of form)

end








(*----------------------------- Module To SMV String ----------------------------------*)

(** Translate to smv string *)
module Smv = struct

  (* Translate a const to smv const *)
  let const_act c =
    match c with
    | Intc(i) -> Int.to_string i
    | Strc(s) -> String.lowercase s
    | Boolc(b) -> String.uppercase (Bool.to_string b)

  let type_act ~types name =
    let Enum(_, consts) = List.find_exn types ~f:(fun (Enum(n, _)) -> n = name) in
    let range =
      List.map consts ~f:const_act
      |> String.concat ~sep:", "
    in
    if range = "TRUE, FALSE" || range = "FALSE, TRUE" then "boolean"
    else begin sprintf "{%s}" range end

  (** Translate a paramref to smv string *)
  let paramref_act pr =
    match pr with
    | Paramfix(_, _, c) -> sprintf "[%s]" (const_act c)
    | Paramref(_) -> raise Unexhausted_inst

  let vardef_act ~types vd =
    let Arrdef(name, pds, t) = vd in
    let type_str = type_act ~types t in
    if pds = [] then
      sprintf "%s : %s;" name type_str
    else begin
      let ps = cart_product_with_paramfix pds types in
      let const_strs = List.map ps ~f:(fun group -> 
        List.map group ~f:(fun pr -> paramref_act pr)
        |> String.concat
      ) in
      List.map const_strs ~f:(fun cstr -> sprintf "%s%s : %s;" name cstr type_str)
      |> String.concat ~sep:"\n"
    end

  (* Translate a variable to smv variable *)
  let var_act v =
    let Arr(name, params) = v in
    if params = [] then
      name
    else begin
      let actual_ps = List.map params ~f:paramref_act in
      sprintf "%s%s" name (String.concat actual_ps)
    end

  (* Translate an exp to smv exp *)
  let exp_act exp =
    match exp with
    | Const(c) -> const_act c
    | Var(v) -> var_act v
    | Param(pr) -> (
      match pr with
      | Paramfix(_, _, c) -> sprintf "%s" (const_act c)
      | Paramref(_) -> raise Unexhausted_inst
    )

  (** Translate formula to smv string

      @param form the formula to be translated
      @return the smv string
  *)
  let rec form_act form =
    match form with
    | Chaos -> "TRUE"
    | Miracle -> "FALSE"
    | Eqn(e1, e2) -> sprintf "(%s = %s)" (exp_act e1) (exp_act e2)
    | Neg(form) -> sprintf "(!%s)" (form_act form)
    | AndList(fl) ->
      List.map fl ~f:form_act
      |> reduce ~default:"TRUE" ~f:(fun res x -> sprintf "%s & %s" res x)
      |> sprintf "(%s)"
    | OrList(fl) ->
      List.map fl ~f:form_act
      |> reduce ~default:"FALSE" ~f:(fun res x -> sprintf "%s | %s" res x)
      |> sprintf "(%s)"
    | Imply(f1, f2) -> sprintf "(%s -> %s)" (form_act f1) (form_act f2)

  let rec statement_act ?(is_init=false) s fstr =
    match s with
    | Assign(v, e) ->
      let var_str = var_act v in
      let exp_str = exp_act e in
      if is_init then
        sprintf "init(%s) := case\nTRUE : %s;\nesac;" var_str exp_str
      else begin
        sprintf "next(%s) := case\n%s : %s;\nTRUE : %s;\nesac;" var_str fstr exp_str var_str
      end
    | Parallel(ss) ->
      if ss = [] then "" else begin
        List.map ss ~f:(fun s' -> statement_act ~is_init s' fstr)
        |> String.concat ~sep:"\n"
      end

  (*let rec init_act s =
    match s with
    | Assign(v, e) ->
      let var_str = var_act v in
      let exp_str = exp_act e in
      sprintf "init(%s) := %s;" var_str exp_str
    | Parallel(ss) ->
      if ss = [] then "" else begin
        List.map ss ~f:init_act
        |> String.concat ~sep:"\n"
      end*)
  let init_act s = statement_act ~is_init:true s "TRUE"

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
    let form_str = form_act f in
    let statement_str = escape (statement_act s form_str) in
    let rule_proc = 
      sprintf "MODULE Proc__%s(%s)\nASSIGN\n%s" name (escape vars_str) statement_str
    in
    (* result *)
    (rule_proc_inst, rule_proc)

  let prop_act property =
    let Prop(_, _, f) = property in
    sprintf "SPEC\n  AG (!%s)" (form_act f)

  let protocol_act {name=_; types; vardefs; init; rules; properties} =
    let property_strs = List.map properties ~f:prop_act in
    let rule_proc_insts, rule_procs =List.unzip (List.map rules ~f:rule_act) in
    let vardef_str = 
      sprintf "VAR\n%s" (String.concat ~sep:"\n" (List.map vardefs ~f:(vardef_act ~types)))
    in
    let rule_proc_insts_str = String.concat ~sep:"\n\n" rule_proc_insts in
    let init_str = sprintf "ASSIGN\n%s" (init_act init) in
    let prop_str = String.concat ~sep:"\n\n" property_strs in
    let rule_procs_str = String.concat ~sep:"\n\n---------\n\n" rule_procs in
    let strs = [vardef_str; rule_proc_insts_str; init_str; prop_str] in
    let main_module = 
      sprintf "MODULE main\n%s" (String.concat ~sep:"\n\n--------------------\n\n" strs)
    in
    sprintf "%s\n\n--------------------\n\n%s" main_module rule_procs_str

end
