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
let is_tautology ?(filename="inv.smt2") ?(quiet=true) ~types ~vardefs form =
  not (Smt.is_satisfiable ~filename ~quiet (ToStr.Smt2.act ~types ~vardefs (neg form)))

(** Judge if a formula is satisfiable *)
let is_satisfiable ?(filename="inv.smt2") ?(quiet=true) ~types ~vardefs form =
  Smt.is_satisfiable ~filename ~quiet (ToStr.Smt2.act ~types ~vardefs form)

(** Cast a formula to a list of formulae with and relation between them *)
let rec flat_and_to_list form =
  match form with
  | AndList(fl) -> List.concat (List.map fl ~f:flat_and_to_list)
  | Neg(OrList(fl)) -> flat_and_to_list (andList (List.map fl ~f:neg))
  | Chaos
  | Miracle
  | Eqn(_)
  | Neg(_)
  | OrList(_)
  | Imply(_) -> [form]

(** For andList, flat its all components,
    for others, flat to a single list
*)
let flat_to_andList form =
  andList (flat_and_to_list form)

(** Cast a formula to a list of formulae with or relation between them *)
let rec flat_or_to_list form =
  match form with
  | OrList(fl) -> List.concat (List.map fl ~f:flat_or_to_list)
  | Neg(AndList(fl)) -> flat_or_to_list (orList (List.map fl ~f:neg))
  | Chaos
  | Miracle
  | Eqn(_)
  | Neg(_)
  | AndList(_)
  | Imply(_) -> [form]

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
let rec simplify ~types ~vardefs form = 
  match form with
  | Chaos -> chaos
  | Miracle -> miracle
  | Eqn(_) ->
    if is_tautology ~types ~vardefs form then chaos
    else if not (is_satisfiable ~types ~vardefs form) then miracle
    else begin form end
  | Neg(Neg(f)) -> simplify ~types ~vardefs f
  | Neg(AndList(_)) -> simplify ~types ~vardefs (flat_to_orList form)
  | Neg(OrList(_)) -> simplify ~types ~vardefs (flat_to_andList form)
  | Neg(f) ->(
      let simplified = simplify ~types ~vardefs f in
      match simplified with
      | Chaos -> miracle
      | Miracle -> chaos
      | _ -> neg simplified
    )
  | AndList(fl) ->
    let simplified = List.map fl ~f:(simplify ~types ~vardefs) in
    if List.exists simplified ~f:(fun x -> x = Miracle) then miracle
    else begin
      let not_chaos = List.dedup (List.filter simplified ~f:(fun x -> not (x = Chaos))) in
      match not_chaos with
      | [] -> chaos
      | [one] -> one
      | _ -> andList not_chaos
    end
  | OrList(fl) ->
    let simplified = List.map fl ~f:(simplify ~types ~vardefs) in
    if List.exists simplified ~f:(fun x -> x = Chaos) then chaos
    else begin
      let not_miracle = List.dedup (List.filter simplified ~f:(fun x -> not (x = Miracle))) in
      match not_miracle with
      | [] -> miracle
      | [one] -> one
      | _ -> orList not_miracle
    end
  | Imply(f1, f2) -> simplify ~types ~vardefs (orList [neg f1; f2])

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

