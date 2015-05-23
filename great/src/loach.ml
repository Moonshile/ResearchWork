(** Language for cache coherence protocols in support of
    parameterization and local variables

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils
open Core.Std

open Paramecium

(** Global variable *)
let global name = arr name []

(** Record definition *)
(*val record_def : (string * vardef list) list -> vardef list*)
let record_def named_vardefs =
  let naming (name, vd) =
    List.map vd ~f:(fun (Arrdef(n, pds, t)) -> arrdef (sprintf "%s.%s" name n) pds t)
  in
  List.concat (List.map named_vardefs ~f:naming)

(** Record *)
let record vars =
  let named_vars = List.map vars ~f:(fun (Arr(name, prs)) -> (name, prs)) in
  let names, prs = List.unzip named_vars in
  arr (String.concat names ~sep:".") (List.concat prs)

(** Forall formula *)
let forallFormula ~types paramdefs form =
  let ps = cart_product_with_paramfix paramdefs types in
  andList (List.map ps ~f:(apply_form form))

(** Exist formula *)
let existFormula ~types paramdefs form =
  let ps = cart_product_with_paramfix paramdefs types in
  orList (List.map ps ~f:(apply_form form))

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

(*----------------------------- Translate module ---------------------------------*)

(** Translate language of this level to the next lower level *)
module Trans = struct

  (** Unexhausted instantiation
      This exception should never be raised. Once raised, There should be a bug in this tool.
  *)
  exception Unexhausted_inst

  (* Translate data structures from Loach to Paramecium *)

  let rec trans_statement ~types statement =
    match statement with
    | Assign(v, e) -> [(chaos, Paramecium.assign (trans_var v) (trans_exp e))]
    | Parallel(slist) -> [(chaos, Paramecium.parallel (List.map ~f:trans_statement ~types slist))]
    | IfStatement(f, s) ->
        let translated = trans_statement ~types s in
        List.map translated ~f:(fun (f', s') -> (andList [f; f'], s'))
    | IfelseStatement(f, s1, s2) ->
        let translated1 = trans_statement ~types s1 in
        let res1 = List.map translated1 ~f:(fun (f', s') -> (andList [f; f'], s')) in
        let translated2 = trans_statement ~types s2 in
        let res2 = List.map translated2 ~f:(fun (f', s') -> (andList [neg f; f'], s')) in
        List.concat [res1; res2]
    | ForStatement(s, paramdefs) ->
        cart_product_with_paramfix paramdefs types
        |> List.map ~f:(apply_statement s)
        |> List.map ~f:(trans_statement ~types)
        |> List.concat

  let trans_rule ~types r =
    match r with
    | Rule(n, p, f, s) ->
      let guarded_s = trans_statement ~types s in
      (* TODO generate a new rule name??? *)
      List.map guarded_s ~f:(fun (g, s) -> Paramecium.rule n p (andList [f; g]) s)

  (** Translate language of Loach to Paramecium

      @param loach cache coherence protocol written in Loach
      @return the protocol in Paramecium
  *)
  let act ~loach:{name; types; vardefs; init; rules; properties} =
    let new_init = 
      match trans_statement ~types init with
      | [(_, s)] -> s
      | _ -> raise Empty_exception
    in
    let new_rules = List.concat (List.map rules ~f:(trans_rule ~types)) in
    Prt.info "Done\n";
    { Paramecium.name = name;
      types = types;
      vardefs = vardefs;
      init = new_init;
      rules = new_rules;
      properties = properties;
    }

end
