(** Language for cache coherence protocols in support of
    parameterization and local variables

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
    + paramdef, name and typename
*)
type paramdef =
  | Paramdef of string * string

let paramdef name typename = Paramdef(name, typename)

(** Parameter references
    + Paramref, name
*)
type paramref =
  | Paramref of string

let paramref name = Paramref name

(** Index definitions, for abstract components
    + Indexdef: name, type name
*)
type indexdef =
  | Indexdef of string * string

let indexdef name tname = Indexdef(name, tname)

(** Index reference
    + Indexref: name
*)
type indexref =
  | Indexref of string

let indexref name = Indexref name

(** Variable definitions, each with its name and name of its type
    + Arraydef: name, param definitions, index definitions, type name
    + Singledef: name, type name
*)
type vardef =
  | Arraydef of string * paramdef list * indexdef list * string
  | Singledef of string * string

let arraydef name paramdef indexdef typename = Arraydef(name, paramdef, indexdef, typename)
let singledef name typename = Singledef(name, typename)

(** Variable reference
    + Array: name, paramref, indexref
    + Single: name
*)
type var =
  | Array of string * paramref list * indexref list
  | Single of string

let array name paramref indexref = Array(name, paramref, indexref)
let single name = Single name

(** Represents expressions, including
    + Constants
    + Variable references
    + Condition expressions
    + Parameter
    + Index
*)
type exp =
  | Const of const
  | Var of var
  | Cond of formula * exp * exp
  | Param of paramref
  | Index of indexref
(** Boolean expressions, including
    + Boolean constants, Chaos as True, Miracle as false
    + Equation expression
    + Other basic logical operations, including negation,
      conjuction, disjuction, and implication
    + Abstract formulae, a set of formulae which has conjunction relation
*)
and formula =
  | Chaos
  | Miracle
  | Eqn of exp * exp
  | Neg of formula
  | AndList of formula list
  | OrList of formula list
  | Imply of formula * formula
  | AbsForm of formula * indexdef list

let const c = Const c
let var v = Var v
let cond f e1 e2 = Cond(f, e1, e2)
let param paramref = Param paramref
let index indexref = Index indexref

let chaos = Chaos
let miracle = Miracle
let eqn e1 e2 = Eqn(e1, e2)
let neg f = Neg f
let andList fs = AndList fs
let orList fs = OrList fs
let imply f1 f2 = Imply(f1, f2)
let absForm f indexdefs = AbsForm(f, indexdefs)

(** Assignment statements, including
    + Single assignment
    + Parallel assignment
    + AbsStatement: a set of statements which has Parallel relation
*)
type statement =
  | Assign of var * exp
  | Parallel of statement list
  | AbsStatement of statement * indexdef list

let assign v e = Assign(v, e)
let parallel statements = Parallel statements
let absStatement s indexdefs = AbsStatement(s, indexdefs)

(** Represents rules which consists of guard and assignments
    + Rule: name, parameters, guard, assignments
    + AbsRule: a list of rules
*)
type rule = 
  | Rule of string * paramdef list * formula * statement
  | AbsRule of rule * indexdef list

let rule name paramdef f s = Rule(name, paramdef, f, s)
let absRule r indexdefs = AbsRule(r, indexdefs)

(** Represents properties
    + Prop: name, parameters, formula
    + AbsProp: a list of properties
*)
type prop =
  | Prop of string * paramdef list * formula
  | AbsProp of prop * indexdef list

let prop name paramdef f = Prop(name, paramdef, f)
let absProp p indexdefs = AbsProp(p, indexdefs)

(** Represents the whole protocol *)
type protocol = {
  types: typedef list;
  vardefs: vardef list;
  init: statement;
  rules: rule list;
  properties: prop list;
}

(*----------------------------- Exceptions ----------------------------------*)


(*----------------------------- Functions ----------------------------------*)

(** Convert a int list to const list *)
let int_consts ints = List.map ints ~f:intc
(** Convert a string list to const list *)
let str_consts strs = List.map strs ~f:strc
(** Convert a boolean list to const list *)
let bool_consts bools = List.map bools ~f:boolc

(** Find the letues range of a type by its name

    @param name: name of the type
    @param types: list of type definitions
    @return the concrete type, i.e., const list
*)
let name2type ~tname ~types =
  let Enum(_, consts) = List.find_exn types ~f:(fun (Enum(n, _)) -> n = tname) in
  consts

(* Generate combination of all possible values of a `indexdef` set
    Result is like [[Boolc true; Intc 1]; [Boolc false; Intc 1]]
*)
let combine_params indexdefs types =
  indexdefs
  |> List.map ~f:(fun (Indexdef(_, tname)) -> name2type ~tname ~types)
  |> combination

(* Generate combination of all possible values of a `indexdef` set
    Each value in each set with its index name
    Result is like [[("x", Boolc true); ("n", Intc 1)]; [("x", Boolc false); ("n", Intc 1)]]
*)
let combine_params_with_name indexdefs types =
  indexdefs
  |> List.map ~f:(fun (Indexdef(n, tname)) -> (n, name2type ~tname ~types))
  |> List.map ~f:(fun (n, t) -> List.map t ~f:(fun x -> (n, x)))
  |> combination

(* attach const i to string name *)
let attach name i =
  match i with
  | Strc(x) -> sprintf "%s[%s]" name x
  | Intc(x) -> sprintf "%s[%d]" name x
  | Boolc(x) -> sprintf "%s[%b]" name x

(* attach consts i to string name *)
let attach_list name i_list =
  List.fold i_list ~init:name ~f:attach

(* Instantiate vardef *)
let inst_vardef vardef ~types =
  match vardef with
  | Singledef(n, t) -> [singledef n t]
  | Arraydef(n, p, i, t) ->
    combine_params i types
    |> List.map ~f:(fun x -> arraydef (attach_list n x) p [] t)

(* Apply indexref with index *)
let apply_indexref (Indexref x) ~index =
  List.Assoc.find_exn index x

(* Apply the indexref to var *)
let apply_var var ~index =
  match var with
  | Single(s) -> Single(s)
  | Array(s, params, indexrefs) ->
    let i_list = List.map indexrefs ~f:(apply_indexref ~index) in
    array (attach_list s i_list) params []

(* Apply exp with index *)
let rec apply_exp exp ~index ~types =
  match exp with
  | Var(x) -> var (apply_var x ~index)
  | Cond(f, exp1, exp2) ->
    cond (apply_form f ~index ~types) (apply_exp exp1 ~index ~types) (apply_exp exp2 ~index ~types)
  | Index(x) -> const (apply_indexref x ~index)
  | Const(_)
  | Param(_) -> exp
(* Apply formula with index *)
and apply_form form ~index ~types =
  match form with
  | Eqn(exp1, exp2) -> eqn (apply_exp exp1 ~index ~types) (apply_exp exp2 ~index ~types)
  | Neg(x) -> neg (apply_form x ~index ~types)
  | AndList(x) -> andList (List.map x ~f:(apply_form ~index ~types))
  | OrList(x) -> orList (List.map x ~f:(apply_form ~index ~types))
  | Imply(f1, f2) -> imply (apply_form f1 ~index ~types) (apply_form f2 ~index ~types)
  | AbsForm(x, indexdefs) ->
    inst_form x indexdefs ~types
  | Chaos
  | Miracle -> form
(* Instantiate formulae *)
and inst_form form indexdefs ~types =
  let actual_index = combine_params_with_name indexdefs types in
  andList (List.map actual_index ~f:(fun index -> apply_form form ~index ~types))

(* Apply statement with index *)
let rec apply_statement statement ~index ~types =
  match statement with
  | Assign(v, e) -> assign (apply_var v ~index) (apply_exp e ~index ~types)
  | Parallel(x) -> parallel (List.map x ~f:(apply_statement ~index ~types))
  | AbsStatement(s, indexdefs) -> inst_statement s indexdefs ~types
(* Instantiate statement *)
and inst_statement statement indexdefs ~types =
  let actual_index = combine_params_with_name indexdefs types in
  parallel (List.map actual_index ~f:(fun index -> apply_statement statement ~index ~types))

(* Apply rule with index *)
let rec apply_rule r ~index ~types =
  match r with
  | Rule(n, p, f, s) ->
    [rule n p (apply_form f ~index ~types) (apply_statement s ~index ~types)]
  | AbsRule(x, indexdefs) -> inst_rule x indexdefs ~types
(* Instantiate rule *)
and inst_rule r indexdefs ~types =
  let actual_index = combine_params_with_name indexdefs types in
  List.concat (List.map actual_index ~f:(fun index -> apply_rule r ~index ~types))

(* Apply property with index *)
let rec apply_prop p ~index ~types =
  match p with
  | Prop(n, p, f) -> [prop n p (apply_form f ~index ~types)]
  | AbsProp(x, indexdefs) -> inst_prop x indexdefs ~types
(* Instantiate property *)
and inst_prop p indexdefs ~types =
  let actual_index = combine_params_with_name indexdefs types in
  List.concat (List.map actual_index ~f:(fun index -> apply_prop p ~index ~types))
