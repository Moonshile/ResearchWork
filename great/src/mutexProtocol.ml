
(* This program is translated from its corresponding murphi version *)

open Core.Std
open Utils
open Paramecium
open Loach
open Formula
open InvFinder

let _True = boolc true
let _False = boolc false

let types = [
  enum "client" (int_consts [1; 2; 3; 4]);
  enum "boolean" [_True; _False];
]



let vardefs = List.concat [
  [arrdef "turn" [] "client"];
  [arrdef "want" [paramdef "i0" "client"] "boolean"];
  [arrdef "crit" [paramdef "i1" "client"] "boolean"]
]

let init = (parallel [(forStatement (parallel [(assign (arr "want" [paramref "i"]) (const (boolc false))); (assign (arr "crit" [paramref "i"]) (const (boolc false)))]) [paramdef "i" "client"]); (assign (global "turn") (const (intc 1)))])

let n_rule_req =
  let name = "n_rule_req" in
  let params = [paramdef "c1" "client"] in
  let formula = (eqn (var (arr "want" [paramref "c1"])) (const (boolc false))) in
  let statement = (assign (arr "want" [paramref "c1"]) (const (boolc true))) in
  rule name params formula statement

let n_rule_enter =
  let name = "n_rule_enter" in
  let params = [paramdef "c1" "client"] in
  let formula = (andList [(andList [(eqn (var (arr "want" [paramref "c1"])) (const (boolc true))); (eqn (var (arr "crit" [paramref "c1"])) (const (boolc false)))]); (eqn (var (global "turn")) (param (paramref "c1")))]) in
  let statement = (assign (arr "crit" [paramref "c1"]) (const (boolc true))) in
  rule name params formula statement

let n_rule_exit =
  let name = "n_rule_exit" in
  let params = [paramdef "c1" "client"; paramdef "c2" "client"] in
  let formula = (eqn (var (arr "crit" [paramref "c1"])) (const (boolc true))) in
  let statement = (parallel [(assign (global "turn") (param (paramref "c2"))); (assign (arr "crit" [paramref "c1"]) (const (boolc false))); (assign (arr "want" [paramref "c1"]) (const (boolc false)))]) in
  rule name params formula statement

let rules = [n_rule_req; n_rule_enter; n_rule_exit]

let n_coherence =
  let name = "n_coherence" in
  let params = [] in
  let formula = (andList [(eqn (var (arr "crit" [paramfix "client" (intc 1)])) (const (boolc true))); (eqn (var (arr "crit" [paramfix "client" (intc 2)])) (const (boolc true)))]) in
  prop name params formula

let properties = [n_coherence]


let protocol = Trans.act {
  name = "n_mutex";
  types;
  vardefs;
  init;
  rules;
  properties;
};;

find ~protocol ();;
