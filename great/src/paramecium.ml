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

(* attach consts i to string name *)
let attach_list name i_list =
  List.fold i_list ~init:name ~f:attach

(* Apply a paramref with param, i.e., cast it to consts *)
let apply_paramref paramref ~param =
  match paramref with
  | Paramref(s) -> List.Assoc.find_exn param s
  | Paramfix(c) -> c

(* Apply array with param *)
let apply_array (Arr(name, params)) ~param =
  let param_consts = List.map params ~f:(apply_paramref ~param) in
  arr (attach_list name param_consts) []

(* Apply exp with param *)
let rec apply_exp exp ~param =
  match exp with
  | Var(x) -> var (apply_array x ~param)
  | Cond(f, e1, e2) -> 
    cond (apply_form f ~param) (apply_exp e1 ~param) (apply_exp e2 ~param)
  | Param(p) -> const (apply_paramref p ~param)
  | Const(_) -> exp
(* Apply formula with param *)
and apply_form f ~param =
  match f with
  | Eqn(e1, e2) -> eqn (apply_exp e1 ~param) (apply_exp e2 ~param)
  | Neg(form) -> neg (apply_form form ~param)
  | AndList(fl) -> andList (List.map fl ~f:(apply_form ~param))
  | OrList(fl) -> orList (List.map fl ~f:(apply_form ~param))
  | Imply(f1, f2) -> imply (apply_form f1 ~param) (apply_form f2 ~param)
  | Chaos
  | Miracle -> f

(* Apply statement with param *)
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

(* Apply rule with param *)
let apply_rule r ~param =
  let Rule(name, paramdefs, f, s) = r in
  if name_match param paramdefs then
    rule name [] (apply_form f ~param) (apply_statement s ~param)
  else
    raise Unmatched_parameters

(* Apply property with param *)
let apply_prop property ~param =
  let Prop(name, paramdefs, f) = property in
  if name_match param paramdefs then
    prop name [] (apply_form f ~param)
  else
    raise Unmatched_parameters

(*----------------------------- Module InvFinder ----------------------------------*)

(** Module for find invariants and causal relations *)
module InvFinder = struct

  (** Unexhausted instantiation
      This exception should never be raised. Once raised, There should be a bug in this tool.
  *)
  exception Unexhausted_inst

  (* This exception is for stop warnings. It will never be raised. *)
  exception Empty_exception

  (** Concrete parameter

      + ConcreteParam: name, value
  *)
  type concrete_param =
    | ConcreteParam of string * const

  let concrete_param name value = ConcreteParam(name, value)

  (** Concrete rule

      + ConcreteRule: rule, concrete param list
  *)
  type concrete_rule =
    | ConcreteRule of rule * concrete_param list

  let concrete_rule r ps = ConcreteRule(r, ps)

  (** Concrete property

      + ConcreteProp: property, concrete param list
  *)
  type concrete_prop =
    | ConcreteProp of prop * concrete_param list

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

  let invHoldForRule1 = InvHoldForRule1
  let invHoldForRule2 = InvHoldForRule2
  let invHoldForRule3 p = InvHoldForRule3 p

  (** InvFinder type, i.e., causal relation table *)
  type t = {
    rule: concrete_rule;
    inv: concrete_prop;
    relation: relation;
  }

  (* Convert statements to a list of assignments *)
  let rec statement_2_assigns statement =
    match statement with
    | Parallel(sl) -> List.concat (List.map sl ~f:statement_2_assigns)
    | Assign(v, e) -> [(v, e)]

  (* Strengthen the guard of evaluated exps with formula f *)
  let strengthen ~form evaled_exp =
    List.map evaled_exp ~f:(fun (h, e) -> (andList [form; h], e))

  (* Evaluate exp with assignments
      Result has format (condition, value)
  *)
  let rec expEval exp ~assigns =
    match exp with
    | Const(_) -> [(chaos, exp)]
    | Var(v) ->
      let value = List.Assoc.find_exn assigns v in
      (match value with
      | Cond(f, e1, e2) -> [(f, e1); (neg f, e2)]
      | Const(_)
      | Var(_) -> [(chaos, value)]
      | Param(_) -> raise Unexhausted_inst)
    | Cond(form, e1, e2) ->
      let evaled_e1 = expEval e1 ~assigns in
      let evaled_e2 = expEval e2 ~assigns in
      let evaled_form = formEval form ~assigns in
      (* strengthen value condition with the True components in evaled_form *)
      let evaled_T =
        List.filter evaled_form ~f:(fun (_, f'') -> f'' = Chaos)
        |> List.map ~f:(fun (form, _) -> strengthen ~form evaled_e1)
        |> List.concat
      in
      (* strengthen value condition with the False components in evaled_form *)
      let evaled_F = 
        List.filter evaled_form ~f:(fun (_, f'') -> f'' = Miracle)
        |> List.map ~f:(fun (form, _) -> strengthen ~form evaled_e2)
        |> List.concat
      in
      evaled_T@evaled_F
    | Param(_) -> raise Unexhausted_inst
  (* Evaluate formula with assignments
      Result has format (condition, form)
  *)
  and formEval form ~assigns =
    let handle_tuples ~f tuples =
      match tuples with
      | [(f1, g1); (f2, g2)] -> (andList [f1; f2], f g1 g2)
      | _ -> raise Empty_exception
    in
    match form with
    | Eqn(e1, e2) ->
      combination [expEval e1 ~assigns; expEval e2 ~assigns]
      |> List.map ~f:(handle_tuples ~f:eqn)
    | Neg(f) ->
      formEval f ~assigns
      |> List.map ~f:(fun (g, f') -> (g, neg f'))
    | AndList([]) -> [(chaos, andList [])]
    | AndList(f::glist) ->
      combination [formEval f ~assigns; formEval (andList glist) ~assigns]
      |> List.map ~f:(handle_tuples ~f:(fun g1 g2 -> andList [g1; g2]))
    | OrList([]) -> [(chaos, orList [])]
    | OrList(f::glist) ->
      combination [formEval f ~assigns; formEval (orList glist) ~assigns]
      |> List.map ~f:(handle_tuples ~f:(fun g1 g2 -> orList [g1; g2]))
    | Imply(ant, cons) ->
      combination [formEval ant ~assigns; formEval cons ~assigns]
      |> List.map ~f:(handle_tuples ~f:imply)
    | Chaos -> [(chaos, chaos)]
    | Miracle -> [(chaos, miracle)]

  (* preCond *)
  let preCond f statements =
    formEval f ~assigns:(statement_2_assigns statements)

  (** Find invs and causal relations of a protocol

      @param protocol the protocol
      @return causal relation table
  *)

end
