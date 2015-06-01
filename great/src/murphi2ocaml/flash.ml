
(* This program is translated from its corresponding murphi version *)

open Core.Std
open Utils
open Paramecium
open Loach
open Formula
open InvFinder

let _CACHE_I = strc "CACHE_I"
let _CACHE_S = strc "CACHE_S"
let _CACHE_E = strc "CACHE_E"
let _NODE_None = strc "NODE_None"
let _NODE_Get = strc "NODE_Get"
let _NODE_GetX = strc "NODE_GetX"
let _UNI_None = strc "UNI_None"
let _UNI_Get = strc "UNI_Get"
let _UNI_GetX = strc "UNI_GetX"
let _UNI_Put = strc "UNI_Put"
let _UNI_PutX = strc "UNI_PutX"
let _UNI_Nak = strc "UNI_Nak"
let _INV_None = strc "INV_None"
let _INV_Inv = strc "INV_Inv"
let _INV_InvAck = strc "INV_InvAck"
let _RP_None = strc "RP_None"
let _RP_Replace = strc "RP_Replace"
let _WB_None = strc "WB_None"
let _WB_Wb = strc "WB_Wb"
let _SHWB_None = strc "SHWB_None"
let _SHWB_ShWb = strc "SHWB_ShWb"
let _SHWB_FAck = strc "SHWB_FAck"
let _NAKC_None = strc "NAKC_None"
let _NAKC_Nakc = strc "NAKC_Nakc"
let _True = boolc true
let _False = boolc false

let types = [
  enum "CACHE_STATE" [_CACHE_I; _CACHE_S; _CACHE_E];
  enum "NODE_CMD" [_NODE_None; _NODE_Get; _NODE_GetX];
  enum "UNI_CMD" [_UNI_None; _UNI_Get; _UNI_GetX; _UNI_Put; _UNI_PutX; _UNI_Nak];
  enum "INV_CMD" [_INV_None; _INV_Inv; _INV_InvAck];
  enum "RP_CMD" [_RP_None; _RP_Replace];
  enum "WB_CMD" [_WB_None; _WB_Wb];
  enum "SHWB_CMD" [_SHWB_None; _SHWB_ShWb; _SHWB_FAck];
  enum "NAKC_CMD" [_NAKC_None; _NAKC_Nakc];
  enum "NODE" (int_consts [1; 2; 3]);
  enum "DATA" (int_consts [1; 2]);
  enum "boolean" [_True; _False];
]

let _NODE_STATE = List.concat [
  [arrdef "ProcCmd" [] "NODE_CMD"];
  [arrdef "InvMarked" [] "boolean"];
  [arrdef "CacheState" [] "CACHE_STATE"];
  [arrdef "CacheData" [] "DATA"]
]

let _DIR_STATE = List.concat [
  [arrdef "Pending" [] "boolean"];
  [arrdef "Local" [] "boolean"];
  [arrdef "Dirty" [] "boolean"];
  [arrdef "HeadVld" [] "boolean"];
  [arrdef "HeadPtr" [] "NODE"];
  [arrdef "ShrVld" [] "boolean"];
  [arrdef "ShrSet" [paramdef "i0" "NODE"] "boolean"];
  [arrdef "InvSet" [paramdef "i1" "NODE"] "boolean"]
]

let _UNI_MSG = List.concat [
  [arrdef "Cmd" [] "UNI_CMD"];
  [arrdef "Proc" [] "NODE"];
  [arrdef "Data" [] "DATA"]
]

let _INV_MSG = List.concat [
  [arrdef "Cmd" [] "INV_CMD"]
]

let _RP_MSG = List.concat [
  [arrdef "Cmd" [] "RP_CMD"]
]

let _WB_MSG = List.concat [
  [arrdef "Cmd" [] "WB_CMD"];
  [arrdef "Proc" [] "NODE"];
  [arrdef "Data" [] "DATA"]
]

let _SHWB_MSG = List.concat [
  [arrdef "Cmd" [] "SHWB_CMD"];
  [arrdef "Proc" [] "NODE"];
  [arrdef "Data" [] "DATA"]
]

let _NAKC_MSG = List.concat [
  [arrdef "Cmd" [] "NAKC_CMD"]
]

let _STATE = List.concat [
  record_def "Proc" [paramdef "i2" "NODE"] _NODE_STATE;
  record_def "Dir" [] _DIR_STATE;
  [arrdef "MemData" [] "DATA"];
  record_def "UniMsg" [paramdef "i3" "NODE"] _UNI_MSG;
  record_def "InvMsg" [paramdef "i4" "NODE"] _INV_MSG;
  record_def "RpMsg" [paramdef "i5" "NODE"] _RP_MSG;
  record_def "WbMsg" [] _WB_MSG;
  record_def "ShWbMsg" [] _SHWB_MSG;
  record_def "NakcMsg" [] _NAKC_MSG;
  [arrdef "CurrData" [] "DATA"];
  [arrdef "PrevData" [] "DATA"];
  [arrdef "LastWrVld" [] "boolean"];
  [arrdef "LastWrPtr" [] "NODE"];
  [arrdef "Requester" [] "NODE"];
  [arrdef "Collecting" [] "boolean"];
  [arrdef "FwdCmd" [] "UNI_CMD"];
  [arrdef "FwdSrc" [] "NODE"];
  [arrdef "LastInvAck" [] "NODE"];
  [arrdef "LastOtherInvAck" [] "NODE"]
]

let vardefs = List.concat [
  [arrdef "Home" [] "NODE"];
  record_def "Sta" [] _STATE
]

let init = (parallel [(assign (global "Home") (const (intc 1))); (assign (record [global "Sta"; global "MemData"]) (const (intc 1))); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Sta"; global "WbMsg"; global "Cmd"]) (const _WB_None)); (assign (record [global "Sta"; global "ShWbMsg"; global "Cmd"]) (const _SHWB_None)); (assign (record [global "Sta"; global "NakcMsg"; global "Cmd"]) (const _NAKC_None)); (forStatement (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "p"]; global "ProcCmd"]) (const _NODE_None)); (assign (record [global "Sta"; arr "Proc" [paramref "p"]; global "InvMarked"]) (const (boolc false))); (assign (record [global "Sta"; arr "Proc" [paramref "p"]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; global "Dir"; arr "ShrSet" [paramref "p"]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr "InvSet" [paramref "p"]]) (const (boolc false))); (assign (record [global "Sta"; arr "UniMsg" [paramref "p"]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Sta"; arr "InvMsg" [paramref "p"]; global "Cmd"]) (const _INV_None)); (assign (record [global "Sta"; arr "RpMsg" [paramref "p"]; global "Cmd"]) (const _RP_None))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; global "CurrData"]) (const (intc 1))); (assign (record [global "Sta"; global "PrevData"]) (const (intc 1))); (assign (record [global "Sta"; global "LastWrVld"]) (const (boolc false))); (assign (record [global "Sta"; global "Collecting"]) (const (boolc false))); (assign (record [global "Sta"; global "FwdCmd"]) (const _UNI_None))])

let n_Store =
  let name = "n_Store" in
  let params = [paramdef "src" "NODE"; paramdef "data" "DATA"] in
  let formula = (eqn (var (record [global "Sta"; arr "Proc" [paramref "src"]; global "CacheState"])) (const _CACHE_E)) in
  let statement = (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "src"]; global "CacheData"]) (param (paramref "data"))); (assign (record [global "Sta"; global "CurrData"]) (param (paramref "data"))); (assign (record [global "Sta"; global "LastWrVld"]) (const (boolc true))); (assign (record [global "Sta"; global "LastWrPtr"]) (param (paramref "src")))]) in
  rule name params formula statement

let n_PI_Remote_Get =
  let name = "n_PI_Remote_Get" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "src")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "Proc" [paramref "src"]; global "ProcCmd"])) (const _NODE_None))]); (eqn (var (record [global "Sta"; arr "Proc" [paramref "src"]; global "CacheState"])) (const _CACHE_I))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "src"]; global "ProcCmd"]) (const _NODE_Get)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Get)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_PI_Remote_GetX =
  let name = "n_PI_Remote_GetX" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "src")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "Proc" [paramref "src"]; global "ProcCmd"])) (const _NODE_None))]); (eqn (var (record [global "Sta"; arr "Proc" [paramref "src"]; global "CacheState"])) (const _CACHE_I))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "src"]; global "ProcCmd"]) (const _NODE_GetX)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_GetX)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_PI_Remote_PutX =
  let name = "n_PI_Remote_PutX" in
  let params = [paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "dst")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "ProcCmd"])) (const _NODE_None))]); (eqn (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; global "WbMsg"; global "Cmd"]) (const _WB_Wb)); (assign (record [global "Sta"; global "WbMsg"; global "Proc"]) (param (paramref "dst"))); (assign (record [global "Sta"; global "WbMsg"; global "Data"]) (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheData"])))]) in
  rule name params formula statement

let n_PI_Remote_Replace =
  let name = "n_PI_Remote_Replace" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "src")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "Proc" [paramref "src"]; global "ProcCmd"])) (const _NODE_None))]); (eqn (var (record [global "Sta"; arr "Proc" [paramref "src"]; global "CacheState"])) (const _CACHE_S))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "src"]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; arr "RpMsg" [paramref "src"]; global "Cmd"]) (const _RP_Replace))]) in
  rule name params formula statement

let n_NI_Nak =
  let name = "n_NI_Nak" in
  let params = [paramdef "dst" "NODE"] in
  let formula = (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "dst"]; global "Cmd"])) (const _UNI_Nak)) in
  let statement = (parallel [(assign (record [global "Sta"; arr "UniMsg" [paramref "dst"]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "ProcCmd"]) (const _NODE_None)); (assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "InvMarked"]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_Local_Get_Nak =
  let name = "n_NI_Local_Get_Nak" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(neg (eqn (param (paramref "src")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (neg (eqn (var (record [global "Sta"; arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_Get_Get =
  let name = "n_NI_Local_Get_Get" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(neg (eqn (param (paramref "src")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (neg (eqn (var (record [global "Sta"; arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Get)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (var (record [global "Sta"; global "Dir"; global "HeadPtr"]))); (ifStatement (neg (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (var (global "Home")))) (assign (record [global "Sta"; global "FwdCmd"]) (const _UNI_Get))); (assign (record [global "Sta"; global "Requester"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Collecting"]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_Local_Get_Put =
  let name = "n_NI_Local_Get_Put" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(neg (eqn (param (paramref "src")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (neg (eqn (var (record [global "Sta"; arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)))]) in
  let statement = (ifelseStatement (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True)) (parallel [(assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "MemData"]) (var (record [global "Sta"; arr "Proc" [paramref "Home"]; global "CacheData"]))); (assign (record [global "Sta"; arr "Proc" [paramref "Home"]; global "CacheState"]) (const _CACHE_S)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Put)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Data"]) (var (record [global "Sta"; arr "Proc" [paramref "Home"]; global "CacheData"])))]) (parallel [(ifelseStatement (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True)) (parallel [(assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; arr "ShrSet" [paramref "src"]]) (const (boolc true))); (forStatement (assign (record [global "Sta"; global "Dir"; arr "InvSet" [paramref "p"]]) (var (record [global "(p = src) | Sta"; global "Dir"; arr "ShrSet" [paramref "p"]]))) [paramdef "p" "NODE"])]) (parallel [(assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src")))])); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Put)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Data"]) (var (record [global "Sta"; global "MemData"])))])) in
  rule name params formula statement

let n_NI_Remote_Get_Nak =
  let name = "n_NI_Remote_Get_Nak" in
  let params = [paramdef "src" "NODE"; paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (neg (eqn (param (paramref "dst")) (var (global "Home"))))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"])) (param (paramref "dst")))]); (neg (eqn (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"])) (const _CACHE_E)))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "Sta"; global "NakcMsg"; global "Cmd"]) (const _NAKC_Nakc)); (assign (record [global "Sta"; global "FwdCmd"]) (const _UNI_None)); (assign (record [global "Sta"; global "FwdSrc"]) (param (paramref "src")))]) in
  rule name params formula statement

let n_NI_Remote_Get_Put =
  let name = "n_NI_Remote_Get_Put" in
  let params = [paramdef "src" "NODE"; paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (neg (eqn (param (paramref "dst")) (var (global "Home"))))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"])) (param (paramref "dst")))]); (eqn (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"]) (const _CACHE_S)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Put)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Data"]) (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheData"]))); (ifStatement (neg (eqn (param (paramref "src")) (var (global "Home")))) (parallel [(assign (record [global "Sta"; global "ShWbMsg"; global "Cmd"]) (const _SHWB_ShWb)); (assign (record [global "Sta"; global "ShWbMsg"; global "Proc"]) (param (paramref "src"))); (assign (record [global "Sta"; global "ShWbMsg"; global "Data"]) (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheData"])))])); (assign (record [global "Sta"; global "FwdCmd"]) (const _UNI_None)); (assign (record [global "Sta"; global "FwdSrc"]) (param (paramref "src")))]) in
  rule name params formula statement

let n_NI_Local_GetX_Nak =
  let name = "n_NI_Local_GetX_Nak" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "src")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_GetX_GetX =
  let name = "n_NI_Local_GetX_GetX" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "src")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_GetX)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (var (record [global "Sta"; global "Dir"; global "HeadPtr"]))); (ifStatement (neg (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (var (global "Home")))) (assign (record [global "Sta"; global "FwdCmd"]) (const _UNI_GetX))); (assign (record [global "Sta"; global "Requester"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Collecting"]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX =
  let name = "n_NI_Local_GetX_PutX" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "src")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]) in
  let statement = (ifelseStatement (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True)) (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Sta"; global "Dir"; arr "ShrSet" [paramref "p"]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr "InvSet" [paramref "p"]]) (const (boolc false)))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Data"]) (var (record [global "Sta"; arr "Proc" [paramref "Home"]; global "CacheData"]))); (assign (record [global "Sta"; arr "Proc" [paramref "Home"]; global "CacheState"]) (const _CACHE_I))]) (ifelseStatement (imply (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True)) (andList [(eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src"))); (forallFormula ~types [paramdef "p" "NODE"] (imply (neg (eqn (param (paramref "p")) (param (paramref "src")))) (eqn (var (record [global "Sta"; global "Dir"; arr "ShrSet" [paramref "p"]])) (const _False))))])) (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Sta"; global "Dir"; arr "ShrSet" [paramref "p"]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr "InvSet" [paramref "p"]]) (const (boolc false)))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Data"]) (var (record [global "Sta"; global "MemData"]))); (assign (record [global "Sta"; arr "Proc" [paramref "Home"]; global "CacheState"]) (const _CACHE_I)); (ifStatement (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True)) (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "Home"]; global "CacheState"]) (const _CACHE_I)); (ifStatement (eqn (var (record [global "Sta"; arr "Proc" [paramref "Home"]; global "ProcCmd"])) (const _NODE_Get)) (assign (record [global "Sta"; arr "Proc" [paramref "Home"]; global "InvMarked"]) (const (boolc true))))]))]) (parallel [(assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Sta"; global "Dir"; arr "ShrSet" [paramref "p"]]) (const (boolc false))); (ifelseStatement (andList [(andList [(neg (eqn (param (paramref "p")) (var (global "Home")))); (neg (eqn (param (paramref "p")) (param (paramref "src"))))]); (orList [(andList [(eqn (var (record [global "Sta"; global "Dir"; global "ShrVld"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; arr "ShrSet" [paramref "p"]])) (const _True))]); (andList [(eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "p")))])])]) (parallel [(assign (record [global "Sta"; global "Dir"; arr "InvSet" [paramref "p"]]) (const (boolc true))); (assign (record [global "Sta"; arr "InvMsg" [paramref "p"]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr "InvSet" [paramref "p"]]) (const (boolc false))); (assign (record [global "Sta"; arr "InvMsg" [paramref "p"]; global "Cmd"]) (const _INV_None))]))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Data"]) (var (record [global "Sta"; global "MemData"]))); (ifStatement (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True)) (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "Home"]; global "CacheState"]) (const _CACHE_I)); (ifStatement (eqn (var (record [global "Sta"; arr "Proc" [paramref "Home"]; global "ProcCmd"])) (const _NODE_Get)) (assign (record [global "Sta"; arr "Proc" [paramref "Home"]; global "InvMarked"]) (const (boolc true))))])); (assign (record [global "Sta"; global "Requester"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Collecting"]) (const (boolc true))); (assign (record [global "Sta"; global "PrevData"]) (var (record [global "Sta"; global "CurrData"]))); (ifelseStatement (neg (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src")))) (assign (record [global "Sta"; global "LastOtherInvAck"]) (var (record [global "Sta"; global "Dir"; global "HeadPtr"]))) (forStatement (ifStatement (andList [(neg (eqn (param (paramref "p")) (param (paramref "src")))); (eqn (var (record [global "Sta"; global "Dir"; arr "ShrSet" [paramref "p"]])) (const _True))]) (assign (record [global "Sta"; global "LastOtherInvAck"]) (param (paramref "p")))) [paramdef "p" "NODE"]))]))) in
  rule name params formula statement

let n_NI_Remote_GetX_Nak =
  let name = "n_NI_Remote_GetX_Nak" in
  let params = [paramdef "src" "NODE"; paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (neg (eqn (param (paramref "dst")) (var (global "Home"))))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"])) (param (paramref "dst")))]); (neg (eqn (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"])) (const _CACHE_E)))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "Sta"; global "NakcMsg"; global "Cmd"]) (const _NAKC_Nakc)); (assign (record [global "Sta"; global "FwdCmd"]) (const _UNI_None)); (assign (record [global "Sta"; global "FwdSrc"]) (param (paramref "src")))]) in
  rule name params formula statement

let n_NI_Remote_GetX_PutX =
  let name = "n_NI_Remote_GetX_PutX" in
  let params = [paramdef "src" "NODE"; paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (neg (eqn (param (paramref "dst")) (var (global "Home"))))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"])) (param (paramref "dst")))]); (eqn (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "Sta"; arr "UniMsg" [paramref "src"]; global "Data"]) (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheData"]))); (ifStatement (neg (eqn (param (paramref "src")) (var (global "Home")))) (parallel [(assign (record [global "Sta"; global "ShWbMsg"; global "Cmd"]) (const _SHWB_FAck)); (assign (record [global "Sta"; global "ShWbMsg"; global "Proc"]) (param (paramref "src")))])); (assign (record [global "Sta"; global "FwdCmd"]) (const _UNI_None)); (assign (record [global "Sta"; global "FwdSrc"]) (param (paramref "src")))]) in
  rule name params formula statement

let n_NI_Remote_Put =
  let name = "n_NI_Remote_Put" in
  let params = [paramdef "dst" "NODE"] in
  let formula = (andList [(neg (eqn (param (paramref "dst")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "dst"]; global "Cmd"])) (const _UNI_Put))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "UniMsg" [paramref "dst"]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "ProcCmd"]) (const _NODE_None)); (ifelseStatement (eqn (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "InvMarked"])) (const _True)) (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "InvMarked"]) (const (boolc false))); (assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"]) (const _CACHE_I))]) (parallel [(assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"]) (const _CACHE_S)); (assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheData"]) (var (record [global "Sta"; arr "UniMsg" [paramref "dst"]; global "Data"])))]))]) in
  rule name params formula statement

let n_NI_Remote_PutX =
  let name = "n_NI_Remote_PutX" in
  let params = [paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "dst")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "dst"]; global "Cmd"])) (const _UNI_PutX))]); (eqn (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "ProcCmd"])) (const _NODE_GetX))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "UniMsg" [paramref "dst"]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "ProcCmd"]) (const _NODE_None)); (assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "InvMarked"]) (const (boolc false))); (assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"]) (const _CACHE_E)); (assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheData"]) (var (record [global "Sta"; arr "UniMsg" [paramref "dst"]; global "Data"])))]) in
  rule name params formula statement

let n_NI_Inv =
  let name = "n_NI_Inv" in
  let params = [paramdef "dst" "NODE"] in
  let formula = (andList [(neg (eqn (param (paramref "dst")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "InvMsg" [paramref "dst"]; global "Cmd"])) (const _INV_Inv))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "InvMsg" [paramref "dst"]; global "Cmd"]) (const _INV_InvAck)); (assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "CacheState"]) (const _CACHE_I)); (ifStatement (eqn (var (record [global "Sta"; arr "Proc" [paramref "dst"]; global "ProcCmd"])) (const _NODE_Get)) (assign (record [global "Sta"; arr "Proc" [paramref "dst"]; global "InvMarked"]) (const (boolc true))))]) in
  rule name params formula statement

let n_NI_InvAck =
  let name = "n_NI_InvAck" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(neg (eqn (param (paramref "src")) (var (global "Home")))); (eqn (var (record [global "Sta"; arr "InvMsg" [paramref "src"]; global "Cmd"])) (const _INV_InvAck))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr "InvMsg" [paramref "src"]; global "Cmd"]) (const _INV_None)); (assign (record [global "Sta"; global "Dir"; arr "InvSet" [paramref "src"]]) (const (boolc false))); (ifelseStatement (existFormula ~types [paramdef "p" "NODE"] (andList [(neg (eqn (param (paramref "p")) (param (paramref "src")))); (eqn (var (record [global "Sta"; global "Dir"; arr "InvSet" [paramref "p"]])) (const _True))])) (parallel [(assign (record [global "Sta"; global "LastInvAck"]) (param (paramref "src"))); (forStatement (ifStatement (andList [(neg (eqn (param (paramref "p")) (param (paramref "src")))); (eqn (var (record [global "Sta"; global "Dir"; arr "InvSet" [paramref "p"]])) (const _True))]) (assign (record [global "Sta"; global "LastOtherInvAck"]) (param (paramref "p")))) [paramdef "p" "NODE"])]) (parallel [(assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc false))); (ifStatement (andList [(eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]) (assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false)))); (assign (record [global "Sta"; global "Collecting"]) (const (boolc false))); (assign (record [global "Sta"; global "LastInvAck"]) (param (paramref "src")))]))]) in
  rule name params formula statement

let n_NI_Replace =
  let name = "n_NI_Replace" in
  let params = [paramdef "src" "NODE"] in
  let formula = (eqn (var (record [global "Sta"; arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)) in
  let statement = (parallel [(assign (record [global "Sta"; arr "RpMsg" [paramref "src"]; global "Cmd"]) (const _RP_None)); (ifStatement (eqn (var (record [global "Sta"; global "Dir"; global "ShrVld"])) (const _True)) (parallel [(assign (record [global "Sta"; global "Dir"; arr "ShrSet" [paramref "src"]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr "InvSet" [paramref "src"]]) (const (boolc false)))]))]) in
  rule name params formula statement

let rules = [n_Store; n_PI_Remote_Get; n_PI_Remote_GetX; n_PI_Remote_PutX; n_PI_Remote_Replace; n_NI_Nak; n_NI_Local_Get_Nak; n_NI_Local_Get_Get; n_NI_Local_Get_Put; n_NI_Remote_Get_Nak; n_NI_Remote_Get_Put; n_NI_Local_GetX_Nak; n_NI_Local_GetX_GetX; n_NI_Local_GetX_PutX; n_NI_Remote_GetX_Nak; n_NI_Remote_GetX_PutX; n_NI_Remote_Put; n_NI_Remote_PutX; n_NI_Inv; n_NI_InvAck; n_NI_Replace]

let n_CacheStateProp =
  let name = "n_CacheStateProp" in
  let params = [] in
  let formula = (forallFormula ~types [paramdef "p" "NODE"] (forallFormula ~types [paramdef "q" "NODE"] (imply (neg (eqn (param (paramref "p")) (param (paramref "q")))) (neg (andList [(eqn (var (record [global "Sta"; arr "Proc" [paramref "p"]; global "CacheState"])) (const _CACHE_E)); (eqn (var (record [global "Sta"; arr "Proc" [paramref "q"]; global "CacheState"])) (const _CACHE_E))]))))) in
  prop name params formula

let n_CacheDataProp =
  let name = "n_CacheDataProp" in
  let params = [] in
  let formula = (forallFormula ~types [paramdef "p" "NODE"] (andList [(imply (eqn (var (record [global "Sta"; arr "Proc" [paramref "p"]; global "CacheState"])) (const _CACHE_E)) (eqn (var (record [global "Sta"; arr "Proc" [paramref "p"]; global "CacheData"])) (var (record [global "Sta"; global "CurrData"])))); (imply (eqn (var (record [global "Sta"; arr "Proc" [paramref "p"]; global "CacheState"])) (const _CACHE_S)) (andList [(imply (eqn (var (record [global "Sta"; global "Collecting"])) (const _True)) (eqn (var (record [global "Sta"; arr "Proc" [paramref "p"]; global "CacheData"])) (var (record [global "Sta"; global "PrevData"])))); (imply (eqn (var (record [global "Sta"; global "Collecting"])) (const _False)) (eqn (var (record [global "Sta"; arr "Proc" [paramref "p"]; global "CacheData"])) (var (record [global "Sta"; global "CurrData"]))))]))])) in
  prop name params formula

let n_MemDataProp =
  let name = "n_MemDataProp" in
  let params = [] in
  let formula = (imply (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False)) (eqn (var (record [global "Sta"; global "MemData"])) (var (record [global "Sta"; global "CurrData"])))) in
  prop name params formula

let n_lemma1 =
  let name = "n_lemma1" in
  let params = [] in
  let formula = (forallFormula ~types [paramdef "h" "NODE"] (forallFormula ~types [paramdef "i" "NODE"] (imply (andList [(eqn (param (paramref "h")) (var (global "Home"))); (eqn (var (record [global "Sta"; arr "Proc" [paramref "i"]; global "CacheState"])) (const _CACHE_E))]) (andList [(andList [(andList [(andList [(andList [(eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True)); (neg (eqn (var (record [global "Sta"; global "WbMsg"; global "Cmd"])) (const _WB_Wb)))]); (neg (eqn (var (record [global "Sta"; global "ShWbMsg"; global "Cmd"])) (const _SHWB_ShWb)))]); (neg (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "h"]; global "Cmd"])) (const _UNI_Put)))]); (forallFormula ~types [paramdef "j" "NODE"] (neg (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "j"]; global "Cmd"])) (const _UNI_PutX))))]); (forallFormula ~types [paramdef "j" "NODE"] (imply (neg (eqn (param (paramref "j")) (param (paramref "i")))) (neg (eqn (var (record [global "Sta"; arr "Proc" [paramref "j"]; global "CacheState"])) (const _CACHE_E)))))])))) in
  prop name params formula

let n_lemma2 =
  let name = "n_lemma2" in
  let params = [] in
  let formula = (forallFormula ~types [paramdef "h" "NODE"] (forallFormula ~types [paramdef "i" "NODE"] (forallFormula ~types [paramdef "j" "NODE"] (imply (andList [(andList [(andList [(andList [(eqn (param (paramref "h")) (var (global "Home"))); (neg (eqn (param (paramref "i")) (param (paramref "j"))))]); (neg (eqn (param (paramref "j")) (param (paramref "h"))))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "i"]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "i"]; global "Proc"])) (param (paramref "j")))]) (eqn (var (record [global "Sta"; global "Dir"; global "P"])) (const _True)))))) in
  prop name params formula

let n_lemma3 =
  let name = "n_lemma3" in
  let params = [] in
  let formula = (forallFormula ~types [paramdef "h" "NODE"] (forallFormula ~types [paramdef "i" "NODE"] (forallFormula ~types [paramdef "j" "NODE"] (imply (andList [(andList [(andList [(andList [(eqn (param (paramref "h")) (var (global "Home"))); (neg (eqn (param (paramref "i")) (param (paramref "j"))))]); (neg (eqn (param (paramref "j")) (param (paramref "h"))))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "i"]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr "UniMsg" [paramref "i"]; global "Proc"])) (param (paramref "j")))]) (eqn (var (record [global "Sta"; global "Dir"; global "P"])) (const _True)))))) in
  prop name params formula

let n_lemma4 =
  let name = "n_lemma4" in
  let params = [] in
  let formula = (forallFormula ~types [paramdef "h" "NODE"] (forallFormula ~types [paramdef "i" "NODE"] (imply (andList [(andList [(eqn (param (paramref "h")) (var (global "Home"))); (neg (eqn (param (paramref "i")) (param (paramref "h"))))]); (eqn (var (record [global "Sta"; arr "InvMsg" [paramref "i"]; global "Cmd"])) (const _INV_InvAck))]) (eqn (var (record [global "Sta"; global "Dir"; global "P"])) (const _True))))) in
  prop name params formula

let n_lemma5 =
  let name = "n_lemma5" in
  let params = [] in
  let formula = (forallFormula ~types [paramdef "i" "NODE"] (imply (eqn (var (record [global "Sta"; arr "Proc" [paramref "i"]; global "CacheState"])) (const _CACHE_E)) (eqn (var (record [global "Sta"; arr "Proc" [paramref "i"]; global "CacheData"])) (var (record [global "Sta"; global "CurrData"]))))) in
  prop name params formula

let properties = [n_CacheStateProp; n_CacheDataProp; n_MemDataProp; n_lemma1; n_lemma2; n_lemma3; n_lemma4; n_lemma5]


let protocol = Trans.act {
  name = "n_flash";
  types;
  vardefs;
  init;
  rules;
  properties;
};;
