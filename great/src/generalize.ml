(** Generalize a concrete formula based on Paramecium to parameterized format

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils
open Paramecium

open Core.Std

type t =
  | Paraminfo of paramdef list * (string * paramref) list

let paraminfo paramdefs params = Paraminfo(paramdefs, params)

let paramdef_name_base = "p__"

(** Generate paramdef names *)
let next_name new_paramdefs = sprintf "%s%d" paramdef_name_base (List.length new_paramdefs)

(** Convert paramref *)
let paramref_act pr param_info =
  let Paraminfo(paramdefs, params) = param_info in
  match pr with
  | Paramref(_) -> raise Unexhausted_inst
  | Paramfix(tname, _) ->
    let name = next_name paramdefs in
    let new_def = paramdef name tname in
    let new_param = (name, pr) in
    (paramref name, paraminfo (new_def::paramdefs) (new_param::params))

(** Convert a list of components *)
let components_act components param_info f =
  let rec wrapper components gened_comp param_info =
    match components with
    | [] -> (gened_comp, param_info)
    | c::components' ->
      let (c', param_info') = f c param_info in
      wrapper components' (c'::gened_comp) param_info'
  in
  wrapper components [] param_info

(** Convert var *)
let var_act v param_info =
  let Arr(name, prs) = v in
  let (prs', param_info') = components_act prs param_info paramref_act in
  (arr name prs', param_info')

(** Convert exp *)
let exp_act e param_info =
  match e with
  | Const(_) -> (e, param_info)
  | Var(v) ->
    let (v', param_info') = var_act v param_info in
    (var v', param_info')
  | Param(pr) ->
    let (pr', param_info') = paramref_act pr param_info in
    (param pr', param_info')

(** Convert formula *)
let rec form_act f param_info =
  match f with
  | Chaos
  | Miracle -> (f, param_info)
  | Eqn(e1, e2) ->
    let (e1', param_info1) = exp_act e1 param_info in
    let (e2', param_info2) = exp_act e2 param_info1 in
    (eqn e1' e2', param_info2)
  | Neg(f) ->
    let (f', param_info') = form_act f param_info in
    (neg f', param_info')
  | AndList(fl) ->
    let (fl', param_info') = components_act fl param_info form_act in
    (andList fl', param_info')
  | OrList(fl) ->
    let (fl', param_info') = components_act fl param_info form_act in
    (orList fl', param_info')
  | Imply(f1, f2) ->
    let (f1', param_info1) = form_act f1 param_info in
    let (f2', param_info2) = form_act f2 param_info1 in
    (imply f1' f2', param_info2)
