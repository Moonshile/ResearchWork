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

(** Index definitions, for abstract components
    + Indexdef: name, type name
*)
type indexdef =
  | Indexdef of string * string

let indexdef name tname = Indexdef(name, tname)

(** Index reference
    + Indexref: name
    + Indexfix: const value
*)
type indexref =
  | Indexref of string
  | Indexfix of const

let indexref name = Indexref name
let indexfix value = Indexfix value

(** Parameter definitions
    + paramdef, name and typename
*)
type paramdef =
  | Paramdef of string * string

let paramdef name typename = Paramdef(name, typename)

(** Parameter references
    + Paramref, name
    + Paramfix, type name, value
    + Paramindex, type name of parameter (not index), indexed param
*)
type paramref =
  | Paramref of string
  | Paramfix of string * const
  | Paramindex of string * indexref

let paramref name = Paramref name
let paramfix tname value = Paramfix(tname, value)
let paramindex tname indexed = Paramindex(tname, indexed)

(** Variable definitions, each with its name and name of its type
    + Arraydef: name, index definitions, param definitions, type name
    + Singledef: name, type name
*)
type vardef =
  | Arrdef of string * indexdef list * paramdef list * string
  | Singledef of string * string

let arrdef name ?(indexdef=[]) paramdef typename = Arrdef(name, indexdef, paramdef, typename)
let singledef name typename = Singledef(name, typename)

(** Variable reference
    + Array: name, indexref, paramref
    + Single: name
*)
type var =
  | Arr of string * indexref list * paramref list
  | Single of string

let arr name ?(indexdef=[]) paramref = Arr(name, indexdef, paramref)
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

(* Generate Cartesian production of all possible values of a `indexdef` set
    Result is like [[Boolc true; Intc 1]; [Boolc false; Intc 1]]
*)
let cart_product indexdefs types =
  indexdefs
  |> List.map ~f:(fun (Indexdef(_, tname)) -> name2type ~tname ~types)
  |> cartesian_product

(* Generate Cartesian production of all possible values of a `indexdef` set
    Each value in each set with its index name
    Result is like [[("x", Boolc true); ("n", Intc 1)]; [("x", Boolc false); ("n", Intc 1)]]
*)
let cart_product_with_name indexdefs types =
  indexdefs
  |> List.map ~f:(fun (Indexdef(n, tname)) -> (n, name2type ~tname ~types))
  |> List.map ~f:(fun (n, t) -> List.map t ~f:(fun x -> (n, x)))
  |> cartesian_product

(* Apply indexref with index *)
let apply_indexref i ~index =
  match i with
  | Indexref(x) -> List.Assoc.find_exn index x
  | Indexfix(c) -> c

(* Apply param with index *)
let apply_param param ~index =
  match param with
  | Paramindex(tname, i) -> paramfix tname (apply_indexref i ~index)
  | Paramref(_)
  | Paramfix(_) -> param

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
  | Singledef(n, t) -> [arrdef n [] t]
  | Arrdef(n, i, p, t) ->
    if i = [] then
      [vardef]
    else
      cart_product i types
      |> List.map ~f:(fun x -> arrdef (attach_list n x) p t)

(* Apply the indexref to var *)
let apply_var var ~index =
  match var with
  | Single(s) -> arr s []
  | Arr(s, indexrefs, params) ->
    let i_list = List.map indexrefs ~f:(apply_indexref ~index) in
    arr (attach_list s i_list) (List.map params ~f:(apply_param ~index))

(* Apply exp with index *)
let rec apply_exp exp ~index ~types =
  match exp with
  | Var(x) -> var (apply_var x ~index)
  | Cond(f, exp1, exp2) ->
    cond (apply_form f ~index ~types) (apply_exp exp1 ~index ~types) (apply_exp exp2 ~index ~types)
  | Index(x) -> const (apply_indexref x ~index)
  | Const(_) -> exp
  | Param(p) -> param (apply_param p ~index)
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
  if indexdefs = [] then
    form
  else
    let actual_index = cart_product_with_name indexdefs types in
    andList (List.map actual_index ~f:(fun index -> apply_form form ~index ~types))

(* Apply statement with index *)
let rec apply_statement statement ~index ~types =
  match statement with
  | Assign(v, e) -> assign (apply_var v ~index) (apply_exp e ~index ~types)
  | Parallel(x) -> parallel (List.map x ~f:(apply_statement ~index ~types))
  | AbsStatement(s, indexdefs) -> inst_statement s indexdefs ~types
(* Instantiate statement *)
and inst_statement statement indexdefs ~types =
  if indexdefs = [] then
    statement
  else
    let actual_index = cart_product_with_name indexdefs types in
    parallel (List.map actual_index ~f:(fun index -> apply_statement statement ~index ~types))

(* Apply rule with index *)
let rec apply_rule r ~index ~types =
  match r with
  | Rule(n, pd, f, s) ->
    let new_f = apply_form f ~index ~types in
    let new_s = apply_statement s ~index ~types in
    [rule n pd new_f new_s]
  | AbsRule(x, indexdefs) -> inst_rule x indexdefs ~types
(* Instantiate rule *)
and inst_rule r indexdefs ~types =
  if indexdefs = [] then
    [r]
  else
    let actual_index = cart_product_with_name indexdefs types in
    List.concat (List.map actual_index ~f:(fun index -> apply_rule r ~index ~types))

(* Apply property with index *)
let rec apply_prop property ~index ~types =
  match property with
  | Prop(n, pd, f) ->
    [prop n pd (apply_form f ~index ~types)]
  | AbsProp(x, indexdefs) -> inst_prop x indexdefs ~types
(* Instantiate property *)
and inst_prop property indexdefs ~types =
  if indexdefs = [] then
    [property]
  else
    let actual_index = cart_product_with_name indexdefs types in
    List.concat (List.map actual_index ~f:(fun index -> apply_prop property ~index ~types))


(*----------------------------- Translate module ---------------------------------*)

(** Translate language of this level to the next lower level *)
module Trans = struct

  (** Unexhausted instantiation
      This exception should never be raised. Once raised, There should be a bug in this tool.
  *)
  exception Unexhausted_inst

  (* Translate data structures from Loach to Paramecium *)

  let trans_const const =
    match const with
    | Intc(i) -> Paramecium.intc i
    | Strc(s) -> Paramecium.strc s
    | Boolc(b) -> Paramecium.boolc b

  let trans_typedef typedef =
    match typedef with
    | Enum(s, l) -> Paramecium.enum s (List.map l ~f:trans_const)

  let trans_paramdef pd =
    match pd with
    | Paramdef(n, t) -> Paramecium.paramdef n t

  let trans_paramref pr =
    match pr with
    | Paramref(n) -> Paramecium.paramref n
    | Paramfix(tname, c) -> Paramecium.paramfix tname (trans_const c)
    | Paramindex(_) -> raise Unexhausted_inst

  let trans_indexdef _ =
    raise Unexhausted_inst

  let trans_indexref _ =
    raise Unexhausted_inst

  let trans_vardef vardef =
    match vardef with
     | Arrdef(n, i, p, t) ->
       let _ = (List.map i ~f:trans_indexdef) in
       Paramecium.arrdef n (List.map p ~f:trans_paramdef) t
     | Singledef(_) -> raise Unexhausted_inst

  let trans_var var =
    match var with
    | Arr(n, i, p) ->
      let _ = (List.map i ~f:trans_indexref) in
      Paramecium.arr n (List.map p ~f:trans_paramref)
    | Single(_) -> raise Unexhausted_inst

  let rec trans_exp exp =
    match exp with
    | Const(c) -> Paramecium.const (trans_const c)
    | Var(v) -> Paramecium.var (trans_var v)
    | Cond(f, e1, e2) -> Paramecium.cond (trans_formula f) (trans_exp e1) (trans_exp e2)
    | Param(x) -> Paramecium.param (trans_paramref x)
    | Index(_) -> raise Unexhausted_inst
  and trans_formula formula =
    match formula with
    | Chaos -> Paramecium.Chaos
    | Miracle -> Paramecium.Miracle
    | Eqn(e1, e2) -> Paramecium.eqn (trans_exp e1) (trans_exp e2)
    | Neg(f) -> Paramecium.neg (trans_formula f)
    | AndList(flist) -> Paramecium.andList (List.map ~f:trans_formula flist)
    | OrList(flist) -> Paramecium.orList (List.map ~f:trans_formula flist)
    | Imply(f1, f2) -> Paramecium.imply (trans_formula f1) (trans_formula f2)
    | AbsForm(_) -> raise Unexhausted_inst

  let rec trans_statement statement =
    match statement with
    | Assign(v, e) -> Paramecium.assign (trans_var v) (trans_exp e)
    | Parallel(slist) -> Paramecium.parallel (List.map ~f:trans_statement slist)
    | AbsStatement(_) -> raise Unexhausted_inst

  let trans_rule r =
    match r with
    | Rule(n, p, f, s) -> 
      Paramecium.rule n (List.map p ~f:trans_paramdef) (trans_formula f) (trans_statement s)
    | AbsRule(_) -> raise Unexhausted_inst

  let trans_prop p =
    match p with
    | Prop(n, p, f) -> Paramecium.prop n (List.map p ~f:trans_paramdef) (trans_formula f)
    | AbsProp(_) -> raise Unexhausted_inst

  (** Translate language of Loach to Paramecium

      @param loach cache coherence protocol written in Loach
      @return the protocol in Paramecium
  *)
  let act ~loach:{types; vardefs; init; rules; properties} =
    Prt.info "Start to translate from Loach to Paramecium...\n";
    let new_vardefs = List.concat (List.map vardefs ~f:(inst_vardef ~types)) in
    let new_init = apply_statement init ~index:[] ~types in
    let new_rules = List.concat (List.map rules ~f:(apply_rule ~index:[] ~types)) in
    let new_properties = List.concat (List.map properties ~f:(apply_prop ~index:[] ~types)) in
    Prt.info "Done\n";
    { Paramecium.types = List.map ~f:trans_typedef types;
      vardefs = List.map ~f:trans_vardef new_vardefs;
      init = trans_statement new_init;
      rules = List.map ~f:trans_rule new_rules;
      properties = List.map ~f:trans_prop new_properties;
    }

end
