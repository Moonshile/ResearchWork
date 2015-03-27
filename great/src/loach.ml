(** Language for cache coherence protocols in support of
    parameterization and local variables

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)
open Utils

open Core.Std

(*------------------------------ Types ---------------------------------*)

(** Basic types available, including integers and enumerations.
    Types are defined by their names and range.
*)
type typedef =
  | IntEnum of string * int list
  | StrEnum of string * string list

(** Variable definitions
    + Var: each with its name and name of its type
    + Array var: each with its name, names list of its array-types and name of its type
*)
type vardef =
  | Singledef of string * string
  | Arraydef of string * string list * string

(** Constants *)
type const =
  | Intc of int
  | Strc of string

(** Variables
    + Global variables referenced by name
    + Array variables referenced by name and actual parameters
    + Parameters
*)
type var =
  | Global of string
  | Array of string * exp list
  | Param of string
(** Represents expressions, including
    + Constans of basic_types
    + Global variables of basic_types, with their own names
    + Condition expressions
*)
and exp =
  | Const of const
  | Var of var
  | Cond of formula * exp * exp
(** Boolean expressions, including
    + Boolean constants
    + Equation expression
    + Other basic logical operations, including negation,
      conjuction, disjuction, and implication
    + Abstract formula, which is parameterized its definition with parameters
*)
and formula =
  | True
  | False
  | Eqn of exp * exp
  | Neg of formula
  | And of formula list
  | Or of formula list
  | Imply of formula * formula
  | AbsForm of formula * vardef list

(** Assignment statements, including
    + Single assignment
    + Parallel assignment
    + Abstract statement, which is parameterized its definition with parameters
*)
type statement =
  | Assign of var * exp
  | Parallel of statement list
  | AbsStatement of statement * vardef list

(** Represents rules which consists of guard and assignments
    + Rule with its name, guard and assigments
    + Abstract rule, which is parameterized its definition with parameters
*)
type rule = 
  | Rule of string * formula * statement
  | AbsRule of rule * vardef list

(** Represents properties
    + Property with its name and formula
    + Abstract property, which is parameterized its definition with parameters
*)
type prop =
  | Prop of string * formula
  | AbsProp of prop * vardef list

(** Represents the whole protocol *)
type protocol = {
  types: typedef list;
  vars: vardef list;
  init: statement;
  rules: rule list;
  properties: prop list;
}

(*----------------------------- Exceptions ----------------------------------*)

exception Wrong_parameter
exception Wrong_function_call

(*----------------------------- Functions ---------------------------------*)

(* Convert a typedef to a list of tupels, which contain name and consts
    e.g., `IntEnum("a", [1;2])` is to `("a", [Intc 1; Intc 2])`
*)
let type_range_to_const typedef =
  match typedef with
  | IntEnum(name, ints) -> (name, List.map ~f:(fun x -> Intc x) ints)
  | StrEnum(name, strs) -> (name, List.map ~f:(fun x -> Strc x) strs)

(* Associate name of `vardef`s with their types *)
let assoc_vardef_with_type_exn vardefs types =
  let t_consts = List.map ~f:type_range_to_const types in
  let find_t tname = List.Assoc.find_exn t_consts tname in
  List.map vardefs ~f:(fun vardef ->
    match vardef with
    | Singledef(n, t) | Arraydef(n, _, t) -> (n, find_t t)
  )

(* Generate combination of all possible values of a `vardef` set *)
let combine_params_exn vardefs types =
  (* Firstly, check if every vardef in `vardefs` is constructed by `Singledef` *)
  let is_vardef var =
    match var with
    | Singledef(_) -> true
    | Arraydef(_) -> false
  in
  let all_vardef =
    vardefs
    |> List.map ~f:is_vardef
    |> List.fold ~init:true ~f:(fun res x -> res && x)
  in
  match all_vardef with
  | false -> raise Wrong_parameter
  | true -> 
  (* Secondly, generate the combination *)
    assoc_vardef_with_type_exn vardefs types
    |> List.map ~f:(fun (n, ts) -> List.map ~f:(fun t -> (n, t)) ts)
    |> combination

(* Apply a combination of parameters to an array, generating a global var.
    Need instantiate rules first.
*)
let apply_array_exn arr =
  match arr with
  | Array(name, ilist) ->
    let rec attach str i =
      match i with
      | Const(Intc(c)) -> sprintf "%s[%d]" str c
      | Const(Strc(c)) -> sprintf "%s[%s]" str c
      | _ -> raise Wrong_parameter
    in
    Global(List.fold ~init:name ~f:attach ilist)
  | Global(x) -> Global(x)
  | Param(_) -> raise Wrong_function_call

(* Translate arraydefs to a set of vardefs *)
let trans_vardef vardefs types =
  let t_consts = List.map ~f:type_range_to_const types in
  let rec attach str i =
    match i with
    | Intc(c) -> sprintf "%s[%d]" str c
    | Strc(c) -> sprintf "%s[%s]" str c
  in
  let apply_arraydef name params = List.fold ~init:name ~f:attach params in
  let apply_vardef vardef =
    match vardef with
    | Singledef(name, t) -> [Singledef(name, t)]
    | Arraydef(name, index, t) ->
      index 
      |> List.map ~f:(fun x -> List.Assoc.find_exn t_consts x)
      |> combination
      |> List.map ~f:(apply_arraydef name)
      |> List.map ~f:(fun x -> Singledef(x, t))
  in
  List.concat (List.map ~f:apply_vardef vardefs)

(* Apply a combination of parameters to a exp *)
let rec apply_exp exp ~param =
  match exp with
  | Cond(form, exp1, exp2) -> Cond(form, apply_exp exp1 ~param, apply_exp exp2 ~param)
  | Var(Array(name, exps)) -> Var(Array(name, List.map ~f:(apply_exp ~param) exps))
  | Var(Param(name)) -> Const(List.Assoc.find_exn param name)
  | Var(Global(name)) -> Var(Global(name))
  | Const(x) -> Const(x)

(* Apply a combination of parameters to a fomula *)
let rec apply_form formula ~param ~types =
  match formula with
  | True -> True
  | False -> False
  | Eqn(exp1, exp2) -> Eqn(apply_exp exp1 ~param, apply_exp exp2 ~param)
  | Neg(x) -> Neg(apply_form x ~param ~types)
  | And(x) -> And(List.map ~f:(apply_form ~param ~types) x)
  | Or(x) -> Or(List.map ~f:(apply_form ~param ~types) x)
  | Imply(x, y) -> Imply(apply_form x ~param ~types, apply_form y ~param ~types)
  | AbsForm(x, vardefs) -> instantiate_form x vardefs ~types
(* Instantiate an abstract formula *)
and instantiate_form formula vardefs ~types =
  let actual_params = combine_params_exn vardefs types in
  And(List.map ~f:(fun p -> apply_form formula ~param:p ~types) actual_params)

(* Apply a combination of parameters to a statement *)
let rec apply_statement statement ~param ~types =
  match statement with
  | Assign(var, exp) -> Assign(var, apply_exp exp ~param)
  | Parallel(statements) -> Parallel(List.map ~f:(apply_statement ~param ~types) statements)
  | AbsStatement(statement, vardefs) -> instantiate_statement statement vardefs ~types
(* Instantiate an abstract statement *)
and instantiate_statement statement vardefs ~types =
  let actual_params = combine_params_exn vardefs types in
  Parallel(List.map ~f:(fun p -> apply_statement statement ~param:p ~types) actual_params)

(* Apply a combination of parameters to a rule *)
let rec apply_rule rule ~param ~types =
  match rule with
  | Rule(name, form, s) -> [Rule(name, apply_form form ~param ~types, apply_statement s ~param ~types)]
  | AbsRule(rule, vardefs) -> instantiate_rule rule vardefs ~types
(* Instantiate an abstract rule *)
and instantiate_rule rule vardefs ~types =
  let actual_params = combine_params_exn vardefs types in
  List.concat (List.map ~f:(fun p -> apply_rule rule ~param:p ~types) actual_params)

(* Apply a combination of parameters to a property *)
let rec apply_property prop ~param ~types =
  match prop with
  | Prop(name, form) -> [Prop(name, apply_form form ~param ~types)]
  | AbsProp(p, vardefs) -> instantiate_property p vardefs ~types
(* Instantiate an abstract property *)
and instantiate_property prop vardefs ~types =
  let actual_params = combine_params_exn vardefs types in
  List.concat (List.map ~f:(fun p -> apply_property prop ~param:p ~types) actual_params)

(** Translate language of Loach to Paramecium

    @param loach cache coherence protocol written in Loach
    @return the protocol in Paramecium
*)
let translate ~loach:{types; vars; init; rules; properties} =
  let new_vars = trans_vardef vars types in
  let new_init = apply_statement init ~param:[] ~types in
  let new_rules = List.concat (List.map ~f:(apply_rule ~param:[] ~types) rules) in
  let new_properties = List.concat (List.map ~f:(apply_property ~param:[] ~types) properties) in
  let state_type = Paramecium.StrEnum("state", ["I"; "T"; "C"; "E"]) in
  { Paramecium.types = [state_type];
    vars = [];
    init = Assign(Global("x"), Const(Strc("I")));
    rules = [];
    properties = [];
  }
