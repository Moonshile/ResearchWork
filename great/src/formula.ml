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

(** Judge if f2 could be implied by f1 *)
let could_imply f1 f2 =
  is_tautology (imply f1 f2)

(** Judge if tow formulae are symmetric *)
(* TODO what if variables of the formulae have chaos sequence? *)
let form_are_symmetric f1 f2 =
  ToStr.Smv.form_act f1 = ToStr.Smv.form_act f2

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

let rec remove_inner_andList form =
  match form with
  | Chaos
  | Miracle
  | Eqn(_)
  | Neg(Eqn(_)) -> [form]
  | AndList(fl) -> List.concat (List.map fl ~f:remove_inner_andList)
  | OrList(fl) -> [orList (List.concat (List.map fl ~f:remove_inner_orList))]
  | Neg(_)
  | Imply(_) -> Prt.error (ToStr.Smv.form_act form); raise Empty_exception
and remove_inner_orList form =
  match form with
  | Chaos
  | Miracle
  | Eqn(_)
  | Neg(Eqn(_)) -> [form]
  | AndList(fl) -> [andList (List.concat (List.map fl ~f:remove_inner_andList))]
  | OrList(fl) -> List.concat (List.map fl ~f:remove_inner_orList)
  | Neg(_)
  | Imply(_) -> Prt.error (ToStr.Smv.form_act form); raise Empty_exception

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
    | AndList(_) ->
      let simplified = List.map (remove_inner_andList form) ~f:wrapper in
      if List.exists simplified ~f:(fun x -> x = Miracle) then miracle
      else begin
        let not_chaos = List.dedup (List.filter simplified ~f:(fun x -> not (x = Chaos))) in
        match not_chaos with
        | [] -> chaos
        | [one] -> one
        | _ ->
          not_chaos
          |> List.map ~f:(fun x -> match x with | OrList(fl) -> fl | _ -> [x])
          |> cartesian_product
          |> List.map ~f:(fun x -> andList x)
          |> (fun fl -> match fl with | [andf] -> andf | _ -> orList fl)
      end
    | OrList(_) ->
      let simplified = List.map (remove_inner_orList form) ~f:(wrapper) in
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
  let (_, pfs, form') = Generalize.form_act form in
  let pf_groups = partition pfs ~f:typename_of_paramfix in
  (*Prt.warning (String.concat ~sep:";" (
    List.map pf_groups ~f:(fun x -> String.concat (List.map x ~f:ToStr.Debug.paramref_act))
  )^", "^String.concat ~sep:";" (
    List.map [pfs] ~f:(fun x -> String.concat (List.map x ~f:ToStr.Debug.paramref_act))
  )^"\n");*)
  (* e.g. cast [3;1;2;1] to [a;b;c;b] then to [1;2;3;2] *)
  let normalize_type pfs =
    let rec gen_map pfs range m =
      match pfs with
      | [] -> m
      | pf::pfs' ->
        let key = ToStr.Smv.paramref_act pf in (
          match Map.find m key with
          | None -> (
              let vname = name_of_param pf in
              let tname = typename_of_paramfix pf in
              match range with
              | [] -> (
                  match name2type ~tname ~types with
                  | [] -> raise Empty_exception
                  | v::range' -> gen_map pfs' range' (Map.add m ~key ~data:(paramfix vname tname v))
                )
              | v::range' -> gen_map pfs' range' (Map.add m ~key ~data:(paramfix vname tname v))
            )
          | Some(_) -> gen_map pfs' range m
        )
    in
    let m = gen_map pfs [] (String.Map.empty) in
    List.map pfs ~f:(fun pf -> Map.find_exn m (ToStr.Smv.paramref_act pf))
  in
  let p = List.concat (List.map pf_groups ~f:normalize_type) in
  (*Prt.warning ("Normalize: "^ToStr.Smv.form_act form^", "
    ^ToStr.Debug.form_act (apply_form form' ~p)^", "^
    (String.concat (List.map p ~f:ToStr.Debug.paramref_act))^"\n"
  );*)
  apply_form form' ~p

