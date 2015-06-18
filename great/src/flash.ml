
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
  enum "node_id" (int_consts [0; 1; 2; 3]);
  enum "data_id" (int_consts [1; 2]);
  enum "boolean" [_True; _False];
]

let _DIR_STATE = List.concat [
  [arrdef "Pending" [] "boolean"];
  [arrdef "Local" [] "boolean"];
  [arrdef "Dirty" [] "boolean"];
  [arrdef "HeadVld" [] "boolean"];
  [arrdef "HeadPtr" [] "node_id"];
  [arrdef "ShrVld" [] "boolean"];
  [arrdef "ShrSet" [paramdef "i0" "node_id"] "boolean"];
  [arrdef "InvSet" [paramdef "i1" "node_id"] "boolean"]
]

let _UNI_MSG = List.concat [
  [arrdef "Cmd" [] "UNI_CMD"];
  [arrdef "Proc" [] "node_id"];
  [arrdef "Data" [] "data_id"]
]

let _INV_MSG = List.concat [
  [arrdef "Cmd" [] "INV_CMD"]
]

let _RP_MSG = List.concat [
  [arrdef "Cmd" [] "RP_CMD"]
]

let _WB_MSG = List.concat [
  [arrdef "Cmd" [] "WB_CMD"];
  [arrdef "Proc" [] "node_id"];
  [arrdef "Data" [] "data_id"]
]

let _SHWB_MSG = List.concat [
  [arrdef "Cmd" [] "SHWB_CMD"];
  [arrdef "Proc" [] "node_id"];
  [arrdef "Data" [] "data_id"]
]

let _NAKC_MSG = List.concat [
  [arrdef "Cmd" [] "NAKC_CMD"]
]

let vardefs = List.concat [
  [arrdef "Home" [] "node_id"];
  record_def "Dir" [] _DIR_STATE;
  [arrdef "procCmd" [paramdef "i2" "node_id"] "NODE_CMD"];
  [arrdef "InvMarked" [paramdef "i3" "node_id"] "boolean"];
  [arrdef "CacheState" [paramdef "i4" "node_id"] "CACHE_STATE"];
  [arrdef "CacheData" [paramdef "i5" "node_id"] "data_id"];
  [arrdef "MemData" [] "data_id"];
  record_def "UniMsg" [paramdef "i6" "node_id"] _UNI_MSG;
  record_def "InvMsg" [paramdef "i7" "node_id"] _INV_MSG;
  record_def "RpMsg" [paramdef "i8" "node_id"] _RP_MSG;
  record_def "WbMsg" [] _WB_MSG;
  record_def "ShWbMsg" [] _SHWB_MSG;
  record_def "NakcMsg" [] _NAKC_MSG
]

let init = (parallel [(assign (global "Home") (const (intc 0))); (assign (global "MemData") (const (intc 1))); (assign (record [global "Dir"; global "Pending"]) (const (boolc false))); (assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc false))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; global "HeadPtr"]) (const (intc 1))); (assign (record [global "WbMsg"; global "Cmd"]) (const _WB_None)); (assign (record [global "ShWbMsg"; global "Cmd"]) (const _SHWB_None)); (assign (record [global "NakcMsg"; global "Cmd"]) (const _NAKC_None)); (forStatement (parallel [(assign (arr "procCmd" [paramref "p"]) (const _NODE_None)); (assign (arr "InvMarked" [paramref "p"]) (const (boolc false))); (assign (arr "CacheState" [paramref "p"]) (const _CACHE_I)); (assign (record [global "Dir"; arr "ShrSet" [paramref "p"]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramref "p"]]) (const (boolc false))); (assign (record [arr "UniMsg" [paramref "p"]; global "Cmd"]) (const _UNI_None)); (assign (record [arr "InvMsg" [paramref "p"]; global "Cmd"]) (const _INV_None)); (assign (record [arr "RpMsg" [paramref "p"]; global "Cmd"]) (const _RP_None))]) [paramdef "p" "node_id"])])

let n_NI_ReplaceHome =
  let name = "n_NI_ReplaceHome" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(eqn (var (record [arr "RpMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"])) (const _RP_Replace)); (eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc false)))]) in
  let statement = (assign (record [arr "RpMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _RP_None)) in
  rule name params formula statement

let n_NI_Replace =
  let name = "n_NI_Replace" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(eqn (var (record [arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)); (eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc false)))]) in
  let statement = (assign (record [arr "RpMsg" [paramref "src"]; global "Cmd"]) (const _RP_None)) in
  rule name params formula statement

let n_NI_ReplaceHome =
  let name = "n_NI_ReplaceHome" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(eqn (var (record [arr "RpMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"])) (const _RP_Replace)); (eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc false)))]) in
  let statement = (assign (record [arr "RpMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _RP_None)) in
  rule name params formula statement

let n_NI_ReplaceShrVld =
  let name = "n_NI_ReplaceShrVld" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(eqn (var (record [arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)); (eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc true)))]) in
  let statement = (parallel [(assign (record [arr "RpMsg" [paramref "src"]; global "Cmd"]) (const _RP_None)); (assign (record [global "Dir"; arr "ShrSet" [paramref "src"]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramref "src"]]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_ShWb =
  let name = "n_NI_ShWb" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (record [global "ShWbMsg"; global "Cmd"])) (const _SHWB_ShWb)) in
  let statement = (parallel [(assign (record [global "ShWbMsg"; global "Cmd"]) (const _SHWB_None)); (assign (record [global "Dir"; global "Pending"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc true))); (forStatement (ifStatement (eqn (var (record [global "ShWbMsg"; global "Proc"])) (param (paramref "i"))) (parallel [(assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc true))); (assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true)))])) [paramdef "i" "node_id"])]) in
  rule name params formula statement

let n_NI_FAck =
  let name = "n_NI_FAck" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (record [global "ShWbMsg"; global "Cmd"])) (const _SHWB_FAck)) in
  let statement = (parallel [(assign (record [global "ShWbMsg"; global "Cmd"]) (const _SHWB_None)); (assign (record [global "Dir"; global "Pending"]) (const (boolc false))); (ifStatement (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true))) (assign (record [global "Dir"; global "HeadPtr"]) (var (record [global "ShWbMsg"; global "Proc"]))))]) in
  rule name params formula statement

let n_NI_Wb =
  let name = "n_NI_Wb" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (record [global "WbMsg"; global "Cmd"])) (const _WB_Wb)) in
  let statement = (parallel [(assign (record [global "WbMsg"; global "Cmd"]) (const _WB_None)); (assign (record [global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_InvAck_2 =
  let name = "n_NI_InvAck_2" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(forallFormula ~types [paramdef "i" "node_id"] (andList [(neg (eqn (param (paramref "i")) (param (paramref "src")))); (eqn (var (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]])) (const (boolc false)))])); (eqn (var (record [arr "InvMsg" [paramref "src"]; global "Cmd"])) (const _INV_InvAck))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; arr "InvSet" [paramref "src"]])) (const (boolc true)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Pending"]) (const (boolc false))); (ifStatement (andList [(eqn (var (record [global "Dir"; global "Local"])) (const (boolc true))); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]) (assign (record [global "Dir"; global "Local"]) (const (boolc false))))]) in
  rule name params formula statement

let n_NI_InvAck_1_Home =
  let name = "n_NI_InvAck_1_Home" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(eqn (var (record [arr "InvMsg" [paramref "src"]; global "Cmd"])) (const _INV_InvAck)); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; arr "InvSet" [paramref "src"]])) (const (boolc true)))]); (eqn (var (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]])) (const (boolc true)))]) in
  let statement = (parallel [(assign (record [arr "InvMsg" [paramref "src"]; global "Cmd"]) (const _INV_None)); (assign (record [global "Dir"; arr "InvSet" [paramref "src"]]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_InvAck_1 =
  let name = "n_NI_InvAck_1" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (eqn (var (record [arr "InvMsg" [paramref "src"]; global "Cmd"])) (const _INV_InvAck))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; arr "InvSet" [paramref "src"]])) (const (boolc true)))]); (eqn (var (record [global "Dir"; arr "InvSet" [paramref "dst"]])) (const (boolc true)))]) in
  let statement = (parallel [(assign (record [arr "InvMsg" [paramref "src"]; global "Cmd"]) (const _INV_None)); (assign (record [global "Dir"; arr "InvSet" [paramref "src"]]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_Inv =
  let name = "n_NI_Inv" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (record [arr "InvMsg" [paramref "dst"]; global "Cmd"])) (const _INV_Inv)) in
  let statement = (parallel [(assign (record [arr "InvMsg" [paramref "dst"]; global "Cmd"]) (const _INV_InvAck)); (assign (arr "CacheState" [paramref "dst"]) (const _CACHE_I)); (ifStatement (eqn (var (arr "procCmd" [paramref "dst"])) (const _NODE_Get)) (assign (arr "InvMarked" [paramref "dst"]) (const (boolc true))))]) in
  rule name params formula statement

let n_NI_Remote_PutX =
  let name = "n_NI_Remote_PutX" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(eqn (var (record [arr "UniMsg" [paramref "dst"]; global "Cmd"])) (const _UNI_PutX)); (eqn (var (arr "procCmd" [paramref "dst"])) (const _NODE_GetX))]) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramref "dst"]; global "Cmd"]) (const _UNI_None)); (assign (arr "procCmd" [paramref "dst"]) (const _NODE_None)); (assign (arr "InvMarked" [paramref "dst"]) (const (boolc false))); (assign (arr "CacheState" [paramref "dst"]) (const _CACHE_E))]) in
  rule name params formula statement

let n_NI_Local_PutXAcksDone =
  let name = "n_NI_Local_PutXAcksDone" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"])) (const _UNI_PutX)) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Dir"; global "Pending"]) (const (boolc false))); (assign (record [global "Dir"; global "Local"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc false))); (assign (arr "procCmd" [paramfix "Home" "node_id" (intc 0)]) (const _NODE_None)); (assign (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)]) (const (boolc false))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_E))]) in
  rule name params formula statement

let n_NI_Remote_Put =
  let name = "n_NI_Remote_Put" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (record [arr "UniMsg" [paramref "dst"]; global "Cmd"])) (const _UNI_Put)) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramref "dst"]; global "Cmd"]) (const _UNI_None)); (assign (arr "procCmd" [paramref "dst"]) (const _NODE_None)); (ifelseStatement (eqn (var (arr "InvMarked" [paramref "dst"])) (const (boolc true))) (parallel [(assign (arr "InvMarked" [paramref "dst"]) (const (boolc false))); (assign (arr "CacheState" [paramref "dst"]) (const _CACHE_I))]) (assign (arr "CacheState" [paramref "dst"]) (const _CACHE_S)))]) in
  rule name params formula statement

let n_NI_Local_Put =
  let name = "n_NI_Local_Put" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"])) (const _UNI_Put)) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Dir"; global "Pending"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Dir"; global "Local"]) (const (boolc true))); (assign (arr "procCmd" [paramfix "Home" "node_id" (intc 0)]) (const _NODE_None)); (ifelseStatement (eqn (var (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)])) (const (boolc true))) (parallel [(assign (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)]) (const (boolc false))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I))]) (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_S)))]) in
  rule name params formula statement

let n_NI_Remote_GetX_PutX =
  let name = "n_NI_Remote_GetX_PutX" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (param (paramref "dst")))]); (eqn (var (arr "CacheState" [paramref "dst"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (arr "CacheState" [paramref "dst"]) (const _CACHE_I)); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "ShWbMsg"; global "Cmd"]) (const _SHWB_FAck)); (assign (record [global "ShWbMsg"; global "Proc"]) (param (paramref "src")))]) in
  rule name params formula statement

let n_NI_Remote_GetX_PutX_Home =
  let name = "n_NI_Remote_GetX_PutX_Home" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(eqn (var (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Proc"])) (param (paramref "dst")))]); (eqn (var (arr "CacheState" [paramref "dst"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (arr "CacheState" [paramref "dst"]) (const _CACHE_I)); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Proc"]) (param (paramref "dst")))]) in
  rule name params formula statement

let n_NI_Remote_GetX_Nak =
  let name = "n_NI_Remote_GetX_Nak" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (param (paramref "dst")))]); (neg (eqn (var (arr "CacheState" [paramref "dst"])) (const _CACHE_E)))]) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "NakcMsg"; global "Cmd"]) (const _NAKC_Nakc))]) in
  rule name params formula statement

let n_NI_Remote_GetX_Nak_Home =
  let name = "n_NI_Remote_GetX_Nak_Home" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(eqn (var (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Proc"])) (param (paramref "dst")))]); (neg (eqn (var (arr "CacheState" [paramref "dst"])) (const _CACHE_E)))]) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _UNI_Nak)); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "NakcMsg"; global "Cmd"]) (const _NAKC_Nakc))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX11 =
  let name = "n_NI_Local_GetX_PutX11" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc true)))]); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))) [paramdef "i" "node_id"]); (forStatement (assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX10_home =
  let name = "n_NI_Local_GetX_PutX10_home" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "src")))]); (eqn (var (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc false)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))) [paramdef "i" "node_id"]); (forStatement (ifelseStatement (eqn (param (paramref "i")) (param (paramref "src"))) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])) (const (boolc true)))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "i")))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))])))) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX10 =
  let name = "n_NI_Local_GetX_PutX10" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "src")))]); (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "dst"]])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc false)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))) [paramdef "i" "node_id"]); (forStatement (ifelseStatement (eqn (param (paramref "i")) (param (paramref "src"))) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])) (const (boolc true)))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "i")))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))])))) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX9 =
  let name = "n_NI_Local_GetX_PutX9" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true)))]); (neg (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "src"))))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc false)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))) [paramdef "i" "node_id"]); (forStatement (ifelseStatement (eqn (param (paramref "i")) (param (paramref "src"))) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])) (const (boolc true)))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "i")))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))])))) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX8_home =
  let name = "n_NI_Local_GetX_PutX8_home" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "src")))]); (eqn (var (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc true)))]); (neg (eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_Get)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))) [paramdef "i" "node_id"]); (forStatement (ifelseStatement (eqn (param (paramref "i")) (param (paramref "src"))) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])) (const (boolc true)))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "i")))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))])))) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX8 =
  let name = "n_NI_Local_GetX_PutX8" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "src")))]); (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "dst"]])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc true)))]); (neg (eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_Get)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))) [paramdef "i" "node_id"]); (forStatement (ifelseStatement (eqn (param (paramref "i")) (param (paramref "src"))) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])) (const (boolc true)))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "i")))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))])))) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX7 =
  let name = "n_NI_Local_GetX_PutX7" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (neg (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "src"))))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc true)))]); (neg (eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_Get)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))) [paramdef "i" "node_id"]); (forStatement (ifelseStatement (eqn (param (paramref "i")) (param (paramref "src"))) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])) (const (boolc true)))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (ifelseStatement (andList [(eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "i")))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))])))) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX6 =
  let name = "n_NI_Local_GetX_PutX6" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "src")))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]])) (const (boolc false)))]); (forallFormula ~types [paramdef "i" "node_id"] (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])) (const (boolc false))))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false)))]) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX5 =
  let name = "n_NI_Local_GetX_PutX5" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "src")))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc true)))]); (neg (eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_Get)))]); (eqn (var (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]])) (const (boolc false)))]); (forallFormula ~types [paramdef "i" "node_id"] (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])) (const (boolc false))))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false)))]) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX4 =
  let name = "n_NI_Local_GetX_PutX4" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc true)))]); (eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_Get))]); (eqn (var (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]])) (const (boolc false)))]); (forallFormula ~types [paramdef "i" "node_id"] (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])) (const (boolc false))))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false)))]) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX3 =
  let name = "n_NI_Local_GetX_PutX3" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc false)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false)))]) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX2 =
  let name = "n_NI_Local_GetX_PutX2" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc true)))]); (neg (eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_Get)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false)))]) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I)); (assign (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)]) (const (boolc true)))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX1 =
  let name = "n_NI_Local_GetX_PutX1" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc true)))]); (eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_Get))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))); (assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false)))]) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_PutX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home"))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I)); (assign (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)]) (const (boolc true)))]) in
  rule name params formula statement

let n_NI_Local_GetX_GetX =
  let name = "n_NI_Local_GetX_GetX" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc false)))]); (neg (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "src"))))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_GetX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (record [global "Dir"; global "HeadPtr"])))]) in
  rule name params formula statement

let n_NI_Local_GetX_Nak3 =
  let name = "n_NI_Local_GetX_Nak3" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "src")))]) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_GetX_Nak2 =
  let name = "n_NI_Local_GetX_Nak2" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc true)))]); (neg (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_E)))]) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_GetX_Nak1 =
  let name = "n_NI_Local_GetX_Nak1" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_GetX)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc true)))]) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Remote_Get_Put2 =
  let name = "n_NI_Remote_Get_Put2" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (param (paramref "dst")))]); (eqn (var (arr "CacheState" [paramref "dst"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (arr "CacheState" [paramref "dst"]) (const _CACHE_S)); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Put)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "ShWbMsg"; global "Cmd"]) (const _SHWB_ShWb)); (assign (record [global "ShWbMsg"; global "Proc"]) (param (paramref "src")))]) in
  rule name params formula statement

let n_NI_Remote_Get_Put1 =
  let name = "n_NI_Remote_Get_Put1" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(eqn (var (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"])) (const _UNI_Get)); (eqn (var (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Proc"])) (param (paramref "dst")))]); (eqn (var (arr "CacheState" [paramref "dst"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (arr "CacheState" [paramref "dst"]) (const _CACHE_S)); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _UNI_Put)); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Proc"]) (param (paramref "dst")))]) in
  rule name params formula statement

let n_NI_Remote_Get_Nak2 =
  let name = "n_NI_Remote_Get_Nak2" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (param (paramref "dst")))]); (neg (eqn (var (arr "CacheState" [paramref "dst"])) (const _CACHE_E)))]) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "NakcMsg"; global "Cmd"]) (const _NAKC_Nakc))]) in
  rule name params formula statement

let n_NI_Remote_Get_Nak1 =
  let name = "n_NI_Remote_Get_Nak1" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(eqn (var (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"])) (const _UNI_Get)); (eqn (var (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Proc"])) (param (paramref "dst")))]); (neg (eqn (var (arr "CacheState" [paramref "dst"])) (const _CACHE_E)))]) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _UNI_Nak)); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "NakcMsg"; global "Cmd"]) (const _NAKC_Nakc))]) in
  rule name params formula statement

let n_NI_Local_Get_Put3 =
  let name = "n_NI_Local_Get_Put3" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (neg (eqn (var (record [arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc true)))]); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_S)); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Put)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_Get_Put2 =
  let name = "n_NI_Local_Get_Put2" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (neg (eqn (var (record [arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc false)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Put)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_Get_Put1 =
  let name = "n_NI_Local_Get_Put1" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (neg (eqn (var (record [arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "ShrVld"]) (const (boolc true))); (assign (record [global "Dir"; arr "ShrSet" [paramref "src"]]) (const (boolc true))); (assign (record [global "Dir"; arr "InvSet" [paramfix "Home" "node_id" (intc 0)]]) (var (record [global "Dir"; arr "ShrSet" [paramfix "Home" "node_id" (intc 0)]]))); (forStatement (ifelseStatement (eqn (param (paramref "i")) (param (paramref "src"))) (assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))) (assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])))) [paramdef "i" "node_id"]); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Put)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_Get_Get =
  let name = "n_NI_Local_Get_Get" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (neg (eqn (var (record [arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc false)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (record [global "Dir"; global "HeadPtr"])))]) in
  rule name params formula statement

let n_NI_Local_Get_Nak3 =
  let name = "n_NI_Local_Get_Nak3" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (neg (eqn (var (record [arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "src")))]) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_Get_Nak2 =
  let name = "n_NI_Local_Get_Nak2" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (neg (eqn (var (record [arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true)))]); (eqn (var (record [global "Dir"; global "Local"])) (const (boolc true)))]); (neg (eqn (var (arr "CacheState" [paramref "src"])) (const _CACHE_E)))]) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Local_Get_Nak1 =
  let name = "n_NI_Local_Get_Nak1" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(eqn (var (record [arr "UniMsg" [paramref "src"]; global "Cmd"])) (const _UNI_Get)); (eqn (var (record [arr "UniMsg" [paramref "src"]; global "Proc"])) (var (global "Home")))]); (neg (eqn (var (record [arr "RpMsg" [paramref "src"]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc true)))]) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Nak)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_NI_Nak_Clear =
  let name = "n_NI_Nak_Clear" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (record [global "NakcMsg"; global "Cmd"])) (const _NAKC_Nakc)) in
  let statement = (parallel [(assign (record [global "NakcMsg"; global "Cmd"]) (const _NAKC_None)); (assign (record [global "Dir"; global "Pending"]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_Nak_Home =
  let name = "n_NI_Nak_Home" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"])) (const _UNI_None)) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _UNI_None)); (assign (arr "procCmd" [paramfix "Home" "node_id" (intc 0)]) (const _NODE_None)); (assign (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_Nak =
  let name = "n_NI_Nak" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (record [arr "UniMsg" [paramref "dst"]; global "Cmd"])) (const _UNI_None)) in
  let statement = (parallel [(assign (record [arr "UniMsg" [paramref "dst"]; global "Cmd"]) (const _UNI_None)); (assign (arr "procCmd" [paramref "dst"]) (const _NODE_None)); (assign (arr "InvMarked" [paramref "dst"]) (const (boolc false)))]) in
  rule name params formula statement

let n_PI_Local_Replace =
  let name = "n_PI_Local_Replace" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_S))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Local"]) (const (boolc false))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I))]) in
  rule name params formula statement

let n_PI_Remote_Replace =
  let name = "n_PI_Remote_Replace" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(eqn (var (arr "procCmd" [paramref "src"])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramref "src"])) (const _CACHE_S))]) in
  let statement = (assign (record [arr "RpMsg" [paramref "src"]; global "Cmd"]) (const _RP_Replace)) in
  rule name params formula statement

let n_PI_Local_PutX =
  let name = "n_PI_Local_PutX" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I)); (assign (record [global "Dir"; global "Dirty"]) (const (boolc false))); (ifStatement (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false))) (assign (record [global "Dir"; global "Local"]) (const (boolc false))))]) in
  rule name params formula statement

let n_PI_Remote_PutX =
  let name = "n_PI_Remote_PutX" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(eqn (var (arr "procCmd" [paramref "dst"])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramref "dst"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (arr "CacheState" [paramref "dst"]) (const _CACHE_I)); (assign (record [global "WbMsg"; global "Cmd"]) (const _WB_Wb)); (assign (record [global "WbMsg"; global "Proc"]) (param (paramref "dst")))]) in
  rule name params formula statement

let n_PI_Local_GetX_PutX4 =
  let name = "n_PI_Local_GetX_PutX4" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_S))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc false)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Local"]) (const (boolc true))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (arr "procCmd" [paramfix "Home" "node_id" (intc 0)]) (const _NODE_None)); (assign (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)]) (const (boolc false))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_E))]) in
  rule name params formula statement

let n_PI_Local_GetX_PutX3 =
  let name = "n_PI_Local_GetX_PutX3" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_I))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc false)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Local"]) (const (boolc true))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (assign (arr "procCmd" [paramfix "Home" "node_id" (intc 0)]) (const _NODE_None)); (assign (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)]) (const (boolc false))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_E)); (assign (arr "CacheData" [paramfix "Home" "node_id" (intc 0)]) (var (global "MemData")))]) in
  rule name params formula statement

let n_PI_Local_GetX_PutX2 =
  let name = "n_PI_Local_GetX_PutX2" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_S))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Local"]) (const (boolc true))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (forStatement (parallel [(assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))); (ifelseStatement (orList [(eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "i"))); (andList [(eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])) (const (boolc true)))])]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))]))]) [paramdef "i" "node_id"]); (assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc false))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)]) (const (boolc false))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_E))]) in
  rule name params formula statement

let n_PI_Local_GetX_PutX1 =
  let name = "n_PI_Local_GetX_PutX1" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(andList [(eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_I))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "HeadVld"])) (const (boolc true)))]) in
  let statement = (parallel [(assign (record [global "Dir"; global "Local"]) (const (boolc true))); (assign (record [global "Dir"; global "Dirty"]) (const (boolc true))); (forStatement (parallel [(assign (record [global "Dir"; arr "ShrSet" [paramref "i"]]) (const (boolc false))); (ifelseStatement (orList [(eqn (var (record [global "Dir"; global "HeadPtr"])) (param (paramref "i"))); (andList [(eqn (var (record [global "Dir"; global "ShrVld"])) (const (boolc true))); (eqn (var (record [global "Dir"; arr "ShrSet" [paramref "i"]])) (const (boolc true)))])]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc true))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Dir"; arr "InvSet" [paramref "i"]]) (const (boolc false))); (assign (record [arr "InvMsg" [paramref "i"]; global "Cmd"]) (const _INV_None))]))]) [paramdef "i" "node_id"]); (assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Dir"; global "HeadVld"]) (const (boolc false))); (assign (record [global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (arr "procCmd" [paramfix "Home" "node_id" (intc 0)]) (const _NODE_None)); (assign (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)]) (const (boolc false))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_E))]) in
  rule name params formula statement

let n_PI_Local_GetX_GetX2 =
  let name = "n_PI_Local_GetX_GetX2" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_S))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true)))]) in
  let statement = (parallel [(assign (arr "procCmd" [paramfix "Home" "node_id" (intc 0)]) (const _NODE_GetX)); (assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _UNI_GetX)); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Proc"]) (var (record [global "Dir"; global "HeadPtr"])))]) in
  rule name params formula statement

let n_PI_Local_GetX_GetX1 =
  let name = "n_PI_Local_GetX_GetX1" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_I))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true)))]) in
  let statement = (parallel [(assign (arr "procCmd" [paramfix "Home" "node_id" (intc 0)]) (const _NODE_GetX)); (assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _UNI_GetX)); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Proc"]) (var (record [global "Dir"; global "HeadPtr"])))]) in
  rule name params formula statement

let n_PI_Remote_GetX =
  let name = "n_PI_Remote_GetX" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(eqn (var (arr "procCmd" [paramref "src"])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramref "src"])) (const _CACHE_I))]) in
  let statement = (parallel [(assign (arr "procCmd" [paramref "src"]) (const _NODE_GetX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_GetX)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_PI_Local_Get_Put =
  let name = "n_PI_Local_Get_Put" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_I))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc false)))]) in
  let statement = (parallel [(assign (arr "procCmd" [paramfix "Home" "node_id" (intc 0)]) (const _NODE_None)); (assign (record [global "Dir"; global "Local"]) (const (boolc true))); (ifelseStatement (eqn (var (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)])) (const (boolc true))) (parallel [(assign (arr "InvMarked" [paramfix "Home" "node_id" (intc 0)]) (const (boolc false))); (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_I))]) (assign (arr "CacheState" [paramfix "Home" "node_id" (intc 0)]) (const _CACHE_S)))]) in
  rule name params formula statement

let n_PI_Local_Get_Get =
  let name = "n_PI_Local_Get_Get" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(andList [(andList [(eqn (var (arr "procCmd" [paramfix "Home" "node_id" (intc 0)])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_I))]); (eqn (var (record [global "Dir"; global "Pending"])) (const (boolc false)))]); (eqn (var (record [global "Dir"; global "Dirty"])) (const (boolc true)))]) in
  let statement = (parallel [(assign (arr "procCmd" [paramfix "Home" "node_id" (intc 0)]) (const _NODE_Get)); (assign (record [global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Cmd"]) (const _UNI_Get)); (assign (record [arr "UniMsg" [paramfix "Home" "node_id" (intc 0)]; global "Proc"]) (var (record [global "Dir"; global "HeadPtr"])))]) in
  rule name params formula statement

let n_PI_Remote_Get =
  let name = "n_PI_Remote_Get" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (andList [(eqn (var (arr "procCmd" [paramref "src"])) (const _NODE_None)); (eqn (var (arr "CacheState" [paramref "src"])) (const _CACHE_I))]) in
  let statement = (parallel [(assign (arr "procCmd" [paramref "src"]) (const _NODE_Get)); (assign (record [arr "UniMsg" [paramref "src"]; global "Cmd"]) (const _UNI_Get)); (assign (record [arr "UniMsg" [paramref "src"]; global "Proc"]) (var (global "Home")))]) in
  rule name params formula statement

let n_StoreHome =
  let name = "n_StoreHome" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (arr "CacheState" [paramfix "Home" "node_id" (intc 0)])) (const _CACHE_E)) in
  let statement = (assign (arr "CacheData" [paramfix "Home" "node_id" (intc 0)]) (param (paramref "data"))) in
  rule name params formula statement

let n_Store =
  let name = "n_Store" in
  let params = [paramdef "src" "node_id"; paramdef "dst" "node_id"; paramdef "data" "data_id"] in
  let formula = (eqn (var (arr "CacheState" [paramref "src"])) (const _CACHE_E)) in
  let statement = (assign (arr "CacheData" [paramref "src"]) (param (paramref "data"))) in
  rule name params formula statement

let rules = [n_NI_ReplaceHome; n_NI_Replace; n_NI_ReplaceHome; n_NI_ReplaceShrVld; n_NI_ShWb; n_NI_FAck; n_NI_Wb; n_NI_InvAck_2; n_NI_InvAck_1_Home; n_NI_InvAck_1; n_NI_Inv; n_NI_Remote_PutX; n_NI_Local_PutXAcksDone; n_NI_Remote_Put; n_NI_Local_Put; n_NI_Remote_GetX_PutX; n_NI_Remote_GetX_PutX_Home; n_NI_Remote_GetX_Nak; n_NI_Remote_GetX_Nak_Home; n_NI_Local_GetX_PutX11; n_NI_Local_GetX_PutX10_home; n_NI_Local_GetX_PutX10; n_NI_Local_GetX_PutX9; n_NI_Local_GetX_PutX8_home; n_NI_Local_GetX_PutX8; n_NI_Local_GetX_PutX7; n_NI_Local_GetX_PutX6; n_NI_Local_GetX_PutX5; n_NI_Local_GetX_PutX4; n_NI_Local_GetX_PutX3; n_NI_Local_GetX_PutX2; n_NI_Local_GetX_PutX1; n_NI_Local_GetX_GetX; n_NI_Local_GetX_Nak3; n_NI_Local_GetX_Nak2; n_NI_Local_GetX_Nak1; n_NI_Remote_Get_Put2; n_NI_Remote_Get_Put1; n_NI_Remote_Get_Nak2; n_NI_Remote_Get_Nak1; n_NI_Local_Get_Put3; n_NI_Local_Get_Put2; n_NI_Local_Get_Put1; n_NI_Local_Get_Get; n_NI_Local_Get_Nak3; n_NI_Local_Get_Nak2; n_NI_Local_Get_Nak1; n_NI_Nak_Clear; n_NI_Nak_Home; n_NI_Nak; n_PI_Local_Replace; n_PI_Remote_Replace; n_PI_Local_PutX; n_PI_Remote_PutX; n_PI_Local_GetX_PutX4; n_PI_Local_GetX_PutX3; n_PI_Local_GetX_PutX2; n_PI_Local_GetX_PutX1; n_PI_Local_GetX_GetX2; n_PI_Local_GetX_GetX1; n_PI_Remote_GetX; n_PI_Local_Get_Put; n_PI_Local_Get_Get; n_PI_Remote_Get; n_StoreHome; n_Store]

let n_CacheStateProp =
  let name = "n_CacheStateProp" in
  let params = [] in
  let formula = (forallFormula ~types [paramdef "p" "node_id"] (forallFormula ~types [paramdef "q" "node_id"] (imply (neg (eqn (param (paramref "p")) (param (paramref "q")))) (neg (andList [(eqn (var (arr "CacheState" [paramref "p"])) (const _CACHE_E)); (eqn (var (arr "CacheState" [paramref "q"])) (const _CACHE_E))]))))) in
  prop name params formula

let properties = [n_CacheStateProp]


let protocol = Trans.act {
  name = "n_flash";
  types;
  vardefs;
  init;
  rules;
  properties;
};;

find ~protocol ();;
