(** Generalize a concrete formula based on Paramecium to parameterized format

    @author Yongjian Li <lyj238@gmail.com>
    @author Kaiqiang Duan <duankq@ios.ac.cn>
*)

open Utils
open Paramecium

open Core.Std

let table = Hashtbl.create ~hashable:String.hashable ()

let base_index = ref 0

let next_name () = 
  let res = sprintf "__invFinder_p_%d" (!base_index) in
  incr base_index; res

(** Convert paramref *)
let paramref_act pr pds pfs =
  match pr with
  | Paramref(_) -> Prt.error (ToStr.Debug.paramref_act pr^"\n"); raise Unexhausted_inst
  | Paramfix(vname, tname, c) -> (
    let key = tname^ToStr.Debug.const_act c in
    match Hashtbl.find table key with
    | None ->
      let new_name = 
        if String.length vname > 14 && String.sub vname ~pos:0 ~len:14 = "__invFinder_p_" then
          vname
        else begin next_name () end in
      let new_pd = paramdef new_name tname in
      Hashtbl.replace table ~key ~data:new_pd;
      (new_pd::pds, paramfix new_name tname c::pfs, paramref new_name)
    | Some(Paramdef(vn, _)) -> (pds, pfs, paramref vn)
  )

(** Convert a list of components *)
let components_act components pds pfs ~f =
  let rec wrapper components gened_comp pds pfs =
    match components with
    | [] -> (pds, pfs, gened_comp)
    | c::components' ->
      let (pds', pfs', c') = f c pds pfs in
      wrapper components' (c'::gened_comp) pds' pfs'
  in
  wrapper components [] pds pfs

(** Convert var *)
let var_act (Arr(name, prs)) pds pfs =
  let (pds', pfs', prs') = components_act prs pds pfs ~f:paramref_act in
  (pds', pfs', arr name prs')

(** Convert exp *)
let exp_act e pds pfs =
  match e with
  | Const(_) -> (pds, pfs, e)
  | Var(v) ->
    let (pds', pfs', v') = var_act v pds pfs in
    (pds', pfs', var v')
  | Param(pr) ->
    let (pds', pfs', pr') = paramref_act pr pds pfs in
    (pds', pfs', param pr')

(** Convert formula *)
let form_act f =
  (*Prt.info (ToStr.Debug.form_act f^", ");*)
  let rec wrapper f pds pfs =
    match f with
    | Chaos
    | Miracle -> (pds, pfs, f)
    | Eqn(e1, e2) ->
      let (pds1, pfs1, e1') = exp_act e1 pds pfs in
      let (pds2, pfs2, e2') = exp_act e2 pds1 pfs1 in
      (pds2, pfs2, eqn e1' e2')
    | Neg(f) ->
      let (pds', pfs', f') = wrapper f pds pfs in
      (pds', pfs', neg f')
    | AndList(fl) ->
      let (pds', pfs', fl') = components_act fl pds pfs ~f:wrapper in
      (pds', pfs', andList fl')
    | OrList(fl) ->
      let (pds', pfs', fl') = components_act fl pds pfs ~f:wrapper in
      (pds', pfs', orList fl')
    | Imply(f1, f2) ->
      let (pds1, pfs1, f1') = wrapper f1 pds pfs in
      let (pds2, pfs2, f2') = wrapper f2 pds1 pfs1 in
      (pds2, pfs2, imply f1' f2')
  in
  let (pds, pfs, f') = Hashtbl.clear table; wrapper f [] [] in
  let sorted_pds =
    List.sort ~cmp:(fun (Paramdef(n1, _)) (Paramdef(n2, _)) -> String.compare n1 n2) pds
    |> List.sort ~cmp:(fun (Paramdef(_, tn1)) (Paramdef(_, tn2)) -> String.compare tn1 tn2)
  in
  let sorted_pfs =
    List.sort ~cmp:(fun pf1 pf2 -> String.compare (name_of_param pf1) (name_of_param pf2)) pfs
    |> List.sort ~cmp:(fun pf1 pf2 ->
      let typenameof pf =
        match pf with
        | Paramref(_) -> raise Unexhausted_inst
        | Paramfix(_, tn, _) -> tn
      in
      String.compare (typenameof pf1) (typenameof pf2)
    )
  in
  (*Prt.info (ToStr.Debug.form_act f'^", "^
    (String.concat (List.map sorted_pfs ~f:ToStr.Debug.paramref_act))^"\n");*)
  (sorted_pds, sorted_pfs, f')

