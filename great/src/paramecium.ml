(** The most fundamental language for this tool

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils

open Core.Std

(*------------------------------ Types ---------------------------------*)

(** Constants *)
type const =
  | Intc of int
  | Strc of string
  | Boolc of bool

let intc i = Intc i
let strc s = Strc s
let boolc b = Boolc b

(** Basic types available, including integers and enumerations.
    Types are defined by their names and range.
*)
type typedef =
  | Enum of string * const list

let enum name letues = Enum(name, letues)

(** Parameter definitions
    + Paramdef, name and typename
*)
type paramdef =
  | Paramdef of string * string

let paramdef name typename = Paramdef(name, typename)

(** Parameter references
    + Paramref, name
    + Paramfix, letue
*)
type paramref =
  | Paramref of string
  | Paramfix of const

let paramref name = Paramref name
let paramfix letue = Paramfix letue

(** Variable definitions, each with its name and name of its type
    + Array var: name, param definitions, type name
*)
type vardef =
  | Arrdef of string * paramdef list * string

let arrdef name paramdef typename = Arrdef(name, paramdef, typename)

(** Variable reference *)
type var =
  | Arr of string * paramref list

let arr name paramref = Arr(name, paramref)

(** Represents expressions, including
    + Constants
    + Variable references
    + Condition expressions
    + Parameter
*)
type exp =
  | Const of const
  | Var of var
  | Cond of formula * exp * exp
  | Param of paramref
(** Boolean expressions, including
    + Boolean constants, Chaos as True, Miracle as false
    + Equation expression
    + Other basic logical operations, including negation,
      conjuction, disjuction, and implication
*)
and formula =
  | Chaos
  | Miracle
  | Eqn of exp * exp
  | Neg of formula
  | AndList of formula list
  | OrList of formula list
  | Imply of formula * formula

let const c = Const c
let var v = Var v
let cond f e1 e2 = Cond(f, e1, e2)
let param paramref = Param(paramref)

let chaos = Chaos
let miracle = Miracle
let eqn e1 e2 = Eqn(e1, e2)
let neg f = Neg f
let andList fs = AndList fs
let orList fs = OrList fs
let imply f1 f2 = Imply(f1, f2)

(** Assignment statements, including
    + Single assignment
    + Parallel assignment
*)
type statement =
  | Assign of var * exp
  | Parallel of statement list

let assign v e = Assign(v, e)
let parallel statements = Parallel statements

(** Represents rules which consists of guard and assignments
    + Rule: name, parameters, guard, assignments
*)
type rule = 
  | Rule of string * paramdef list * formula * statement

let rule name paramdef f s = Rule(name, paramdef, f, s)

(** Represents properties
    + Prop: name, parameters, formula
*)
type prop =
  | Prop of string * paramdef list * formula

let prop name paramdef f = Prop(name, paramdef, f)

(** Represents the whole protocol *)
type protocol = {
  types: typedef list;
  vardefs: vardef list;
  init: statement;
  rules: rule list;
  properties: prop list;
}

(*----------------------------- Exceptions ----------------------------------*)

(** The actual parameters can't match with their definitions *)
exception Unmatched_parameters

(** Unexhausted instantiation
    This exception should never be raised. Once raised, There should be a bug in this tool.
*)
exception Unexhausted_inst

(*----------------------------- Functions ----------------------------------*)

(** Find the letues range of a type by its name
*)
let name2type ~tname ~types =
  let Enum(_, consts) = List.find_exn types ~f:(fun (Enum(n, _)) -> n = tname) in
  consts

(* Generate Cartesian production of all possible values of a `paramdef` set
    Result is like [[Boolc true; Intc 1]; [Boolc false; Intc 1]]
*)
let cart_product paramdefs types =
  paramdefs
  |> List.map ~f:(fun (Paramdef(_, tname)) -> name2type ~tname ~types)
  |> combination

(* Generate Cartesian production of all possible values of a `paramdef` set
    Each value in each set with its index name
    Result is like [[("x", Boolc true); ("n", Intc 1)]; [("x", Boolc false); ("n", Intc 1)]]
*)
let cart_product_with_name paramdefs types =
  paramdefs
  |> List.map ~f:(fun (Paramdef(n, tname)) -> (n, name2type ~tname ~types))
  |> List.map ~f:(fun (n, t) -> List.map t ~f:(fun x -> (n, x)))
  |> combination

(* attach const i to string name *)
let attach name i =
  match i with
  | Strc(x) -> sprintf "%s[%s]" name x
  | Intc(x) -> sprintf "%s[%d]" name x
  | Boolc(x) -> sprintf "%s[%b]" name x

(** attach consts i to string name *)
let attach_list name i_list =
  List.fold i_list ~init:name ~f:attach

(** Apply a paramref with param, i.e., cast it to consts *)
let apply_paramref pr ~param =
  match pr with
  | Paramref(s) -> paramfix (List.Assoc.find_exn param s)
  | Paramfix(_) -> pr

(** Apply array with param *)
let apply_array (Arr(name, params)) ~param =
  arr name (List.map params ~f:(apply_paramref ~param))

(** Apply exp with param *)
let rec apply_exp exp ~param =
  match exp with
  | Var(x) -> var (apply_array x ~param)
  | Cond(f, e1, e2) -> 
    cond (apply_form f ~param) (apply_exp e1 ~param) (apply_exp e2 ~param)
  | Param(p) -> (
    match (apply_paramref p ~param) with
    | Paramfix(c) -> const c
    | Paramref(_) -> raise Empty_exception
  )
  | Const(_) -> exp
(** Apply formula with param *)
and apply_form f ~param =
  match f with
  | Eqn(e1, e2) -> eqn (apply_exp e1 ~param) (apply_exp e2 ~param)
  | Neg(form) -> neg (apply_form form ~param)
  | AndList(fl) -> andList (List.map fl ~f:(apply_form ~param))
  | OrList(fl) -> orList (List.map fl ~f:(apply_form ~param))
  | Imply(f1, f2) -> imply (apply_form f1 ~param) (apply_form f2 ~param)
  | Chaos
  | Miracle -> f

(** Apply statement with param *)
let rec apply_statement statement ~param =
  match statement with
  | Assign(v, e) -> assign (apply_array v ~param) (apply_exp e ~param)
  | Parallel(sl) -> parallel (List.map sl ~f:(apply_statement ~param))

(* Check if a given parameter matches with the paramdef *)
let name_match params defs =
  if List.length params = List.length defs then
    let same_name (n1,_) (Paramdef(n2, _)) = n1 = n2 in
    List.map2_exn params defs ~f:same_name
    |> List.fold ~init:true ~f:(fun res x -> res && x)
  else
    false

(** Apply rule with param *)
let apply_rule r ~param =
  let Rule(name, paramdefs, f, s) = r in
  if name_match param paramdefs then
    rule name [] (apply_form f ~param) (apply_statement s ~param)
  else
    raise Unmatched_parameters

(** Apply property with param *)
let apply_prop property ~param =
  let Prop(name, paramdefs, f) = property in
  if name_match param paramdefs then
    prop name [] (apply_form f ~param)
  else
    raise Unmatched_parameters
