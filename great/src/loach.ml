(** Language for cache coherence protocols in support of
    parameterization and local variables

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Core.Std

open Utils

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
  | Vardef of string * string
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
  | AbsAssign of statement * vardef list

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
  vars: var list;
  init: statement list;
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
  let find_t tname =
    match List.Assoc.find t_consts tname with
    | None -> raise Wrong_parameter
    | Some x -> x
  in
  List.map vardefs ~f:(fun vardef ->
    match vardef with
    | Vardef(n, t) | Arraydef(n, _, t) -> (n, find_t t)
  )

(* Generate combination of all possible values of a `vardef` set *)
let combine_params_exn vardefs types =
  (* Firstly, check if every vardef in `vardefs` is constructed by `Vardef` *)
  let is_vardef var =
    match var with
    | Vardef(_) -> true
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

(* Translate array to global var.
    Need instantiate rules first.
*)
let trans_array_exn array =
  match array with
  | Array(name, ilist) ->
    let attach str i =
      match i with
      | Const(Intc(c)) -> sprintf "%s_%d" str c
      | Const(Strc(c)) -> str ^ "_" ^ c
      | _ -> raise Wrong_parameter
    in
    Global(List.fold ~init:name ~f:attach ilist)
  | Global(x) -> Global(x)
  | Param(_) -> raise Wrong_function_call

(** Translate language of Loach to Paramecium

    @param loach cache coherence protocol written in Loach
    @return the protocol in Paramecium
*)
let translate ~loach:{types; vars; init; rules; properties} =
  let state_type = Paramecium.StrEnum("state", ["I"; "T"; "C"; "E"]) in
  { Paramecium.types = [state_type];
    vars = [];
    init = [Assign(Global("x"), Const(Strc("I")))];
    rules = [];
    properties = [];
  }
