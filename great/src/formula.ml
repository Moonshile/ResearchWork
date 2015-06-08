(** Operations of formula based on Paramecium

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils
open Paramecium
open Core.Std

(** Judge if a formula is tautology
    If negation of the formula is not satisfiable, then the formula is tautology

    @param filename is the temp file to store smt2 formula, default is "inv.smt2"
    @param quiet true (default) to prevent to print output of smt solver to screen
    @param types type definitions
    @param vardefs variable definitions
*)
let is_tautology ?(quiet=true) form =
  not (Smt.is_satisfiable ~quiet (ToStr.Smt2.form_of (neg form)))

(** Judge if a formula is satisfiable *)
let is_satisfiable ?(quiet=true) form =
  Smt.is_satisfiable ~quiet (ToStr.Smt2.form_of form)

let rec eliminate_imply_neg form =
  match form with
  | Chaos
  | Miracle
  | Eqn(_) -> form
  | Neg(Chaos) -> miracle
  | Neg(Miracle) -> chaos
  | Neg(Eqn(_)) -> form
  | Neg(Neg(f)) -> eliminate_imply_neg f
  | Neg(AndList(fl)) -> eliminate_imply_neg (orList (List.map fl ~f:neg))
  | Neg(OrList(fl)) -> eliminate_imply_neg (andList (List.map fl ~f:neg))
  | Neg(Imply(f1, f2)) -> eliminate_imply_neg (andList [f1; neg f2])
  | AndList(fl) -> andList (List.map fl ~f:eliminate_imply_neg)
  | OrList(fl) -> orList (List.map fl ~f:eliminate_imply_neg)
  | Imply(f1, f2) -> eliminate_imply_neg (orList [neg f1; f2])

(** Cast a formula to a list of formulae with and relation between them *)
let flat_and_to_list form =
  let no_imply_neg = eliminate_imply_neg form in
  let rec wrapper form =
    match form with
    | Chaos
    | Miracle
    | Eqn(_)
    | Neg(Eqn(_)) -> [form]
    | AndList(fl) -> List.concat (List.map fl ~f:wrapper)
    | OrList(fl) -> [form]
    | Neg(_)
    | Imply(_) -> raise Empty_exception
  in
  wrapper no_imply_neg

(** For andList, flat its all components,
    for others, flat to a single list
*)
let flat_to_andList form =
  andList (flat_and_to_list form)

(** Cast a formula to a list of formulae with or relation between them *)
let flat_or_to_list form =
  let no_imply_neg = eliminate_imply_neg form in
  let rec wrapper form =
    match form with
    | Chaos
    | Miracle
    | Eqn(_)
    | Neg(Eqn(_))
    | AndList(_) -> [form]
    | OrList(fl) -> List.concat (List.map fl ~f:wrapper)
    | Neg(_)
    | Imply(_) -> raise Empty_exception
  in
  wrapper no_imply_neg

(** For orList, flat its all components,
    for others, flat to a single list
*)
let flat_to_orList form =
  orList (flat_or_to_list form)

(** Judge if tow formulae are symmetric *)
(* TODO what if variables of the formulae have chaos sequence? *)
let form_are_symmetric f1 f2 =
  let param_info  = Generalize.paraminfo [] [] in
  let (f1', _) = Generalize.form_act f1 param_info in
  let (f2', _) = Generalize.form_act f2 param_info in
  f1' = f2'

(** Simplify a formula *)
let simplify form =
  let no_imply_neg = eliminate_imply_neg form in
  let rec wrapper form =
    match form with
    | Chaos -> chaos
    | Miracle -> miracle
    | Eqn(_)
    | Neg(Eqn(_)) ->
      if is_tautology form then chaos
      else if not (is_satisfiable form) then miracle
      else begin form end
    | AndList(fl) ->
      let simplified = List.map fl ~f:wrapper in
      if List.exists simplified ~f:(fun x -> x = Miracle) then miracle
      else begin
        let not_chaos = List.dedup (List.filter simplified ~f:(fun x -> not (x = Chaos))) in
        match not_chaos with
        | [] -> chaos
        | [one] -> one
        | _ -> andList not_chaos
      end
    | OrList(fl) ->
      let simplified = List.map fl ~f:(wrapper) in
      if List.exists simplified ~f:(fun x -> x = Chaos) then chaos
      else begin
        let not_miracle = List.dedup (List.filter simplified ~f:(fun x -> not (x = Miracle))) in
        match not_miracle with
        | [] -> miracle
        | [one] -> one
        | _ -> orList not_miracle
      end
    | Neg(_)
    | Imply(_) -> Prt.error (ToStr.Smv.form_act form); raise Empty_exception
  in
  wrapper no_imply_neg

(** Raises when there are many parameter references more than range of its type *)
exception Too_many_parameters_of_same_type

(** Normalize a parameterized formula *)
let normalize form ~types =
  let param_info  = Generalize.paraminfo [] [] in
  let (f, Generalize.Paraminfo(pds, _)) = Generalize.form_act form param_info in
  let pd_tnames = List.dedup (List.map pds ~f:(fun (Paramdef(_, tname)) -> tname)) in
  let pd_tranges = List.map pd_tnames ~f:(fun tname -> (tname, name2type ~tname ~types)) in
  let pd_tmap = Map.of_alist_exn ~comparator:String.comparator pd_tranges in
  let rec norm_params res pds pd_tmap =
    match pds with
    | [] -> res
    | (Paramdef(n, tname))::pds' ->
      let trange = Map.find pd_tmap tname in
      match trange with
      | None ->
        let form_str = ToStr.Smv.form_act form in
        raise (Cannot_find (sprintf "type %s while normalizing %s" tname form_str))
      | Some [] -> raise Too_many_parameters_of_same_type
      | Some(c::trange') ->
        let res' = (n, paramfix tname c)::res in
        let pd_tmap' = Map.add pd_tmap ~key:tname ~data:trange' in
        norm_params res' pds' pd_tmap'
  in
  apply_form f ~p:(norm_params [] pds pd_tmap)

