
(* This program is translated from its corresponding murphi version *)

open Core.Std
open Utils
open Paramecium
open Loach
open Formula
open InvFinder
open Cmdline

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
  enum "NODE" (int_consts [0; 1; 2; 3;]);
  enum "boolean" [_True; _False];
]

let _NODE_STATE = List.concat [
  [arrdef [("ProcCmd", [])] "NODE_CMD"];
  [arrdef [("InvMarked", [])] "boolean"];
  [arrdef [("CacheState", [])] "CACHE_STATE"]
]

let _DIR_STATE = List.concat [
  [arrdef [("Pending", [])] "boolean"];
  [arrdef [("Local", [])] "boolean"];
  [arrdef [("Dirty", [])] "boolean"];
  [arrdef [("HeadVld", [])] "boolean"];
  [arrdef [("HeadPtr", [])] "NODE"];
  [arrdef [("ShrVld", [])] "boolean"];
  [arrdef [("ShrSet", [paramdef "i0" "NODE"])] "boolean"];
  [arrdef [("InvSet", [paramdef "i1" "NODE"])] "boolean"]
]

let _UNI_MSG = List.concat [
  [arrdef [("Cmd", [])] "UNI_CMD"];
  [arrdef [("Proc", [])] "NODE"]
]

let _INV_MSG = List.concat [
  [arrdef [("Cmd", [])] "INV_CMD"]
]

let _RP_MSG = List.concat [
  [arrdef [("Cmd", [])] "RP_CMD"]
]

let _WB_MSG = List.concat [
  [arrdef [("Cmd", [])] "WB_CMD"];
  [arrdef [("Proc", [])] "NODE"]
]

let _SHWB_MSG = List.concat [
  [arrdef [("Cmd", [])] "SHWB_CMD"];
  [arrdef [("Proc", [])] "NODE"]
]

let _NAKC_MSG = List.concat [
  [arrdef [("Cmd", [])] "NAKC_CMD"]
]

let _STATE = List.concat [
  record_def "Proc" [paramdef "i2" "NODE"] _NODE_STATE;
  record_def "Dir" [] _DIR_STATE;
  record_def "UniMsg" [paramdef "i3" "NODE"] _UNI_MSG;
  record_def "InvMsg" [paramdef "i4" "NODE"] _INV_MSG;
  record_def "RpMsg" [paramdef "i5" "NODE"] _RP_MSG;
  record_def "WbMsg" [] _WB_MSG;
  record_def "ShWbMsg" [] _SHWB_MSG;
  record_def "NakcMsg" [] _NAKC_MSG
]

let vardefs = List.concat [
  [arrdef [("Home", [])] "NODE"];
  record_def "Sta" [] _STATE
]

let _Home = paramfix "Home" "NODE" (intc 0)

let init = (parallel [(assign (global "Home") (param (paramfix "h" "NODE" (intc 0)))); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramfix "h" "NODE" (intc 0)))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (assign (record [global "Sta"; global "WbMsg"; global "Cmd"]) (const _WB_None)); (assign (record [global "Sta"; global "ShWbMsg"; global "Cmd"]) (const _SHWB_None)); (assign (record [global "Sta"; global "NakcMsg"; global "Cmd"]) (const _NAKC_None)); (forStatement (parallel [(assign (record [global "Sta"; arr [("Proc", [paramref "p"])]; global "ProcCmd"]) (const _NODE_None)); (assign (record [global "Sta"; arr [("Proc", [paramref "p"])]; global "InvMarked"]) (const (boolc false))); (assign (record [global "Sta"; arr [("Proc", [paramref "p"])]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("UniMsg", [paramref "p"])]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None)); (assign (record [global "Sta"; arr [("RpMsg", [paramref "p"])]; global "Cmd"]) (const _RP_None))]) [paramdef "p" "NODE"])])

let n_PI_Remote_Get =
  let name = "n_PI_Remote_Get" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "src"])]; global "ProcCmd"])) (const _NODE_None))]); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "src"])]; global "CacheState"])) (const _CACHE_I))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("Proc", [paramref "src"])]; global "ProcCmd"]) (const _NODE_Get)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Get)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_PI_Remote_GetX =
  let name = "n_PI_Remote_GetX" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "src"])]; global "ProcCmd"])) (const _NODE_None))]); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "src"])]; global "CacheState"])) (const _CACHE_I))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("Proc", [paramref "src"])]; global "ProcCmd"]) (const _NODE_GetX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_GetX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_PI_Remote_PutX =
  let name = "n_PI_Remote_PutX" in
  let params = [paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "dst")) (param _Home))); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "ProcCmd"])) (const _NODE_None))]); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; global "WbMsg"; global "Cmd"]) (const _WB_Wb)); (assign (record [global "Sta"; global "WbMsg"; global "Proc"]) (param (paramref "dst")))]) in
  rule name params formula statement

let n_PI_Remote_Replace =
  let name = "n_PI_Remote_Replace" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "src"])]; global "ProcCmd"])) (const _NODE_None))]); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "src"])]; global "CacheState"])) (const _CACHE_S))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("Proc", [paramref "src"])]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; arr [("RpMsg", [paramref "src"])]; global "Cmd"]) (const _RP_Replace))]) in
  rule name params formula statement

let n_NI_Nak =
  let name = "n_NI_Nak" in
  let params = [paramdef "dst" "NODE"] in
  let formula = (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "dst"])]; global "Cmd"])) (const _UNI_Nak)) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [paramref "dst"])]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "ProcCmd"]) (const _NODE_None)); (assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "InvMarked"]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_Local_Get_Nak1 =
  let name = "n_NI_Local_Get_Nak1" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (neg (eqn (var (record [global "Sta"; arr [("RpMsg", [paramref "src"])]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _True))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_NI_Local_Get_Nak2 =
  let name = "n_NI_Local_Get_Nak2" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (neg (eqn (var (record [global "Sta"; arr [("RpMsg", [paramref "src"])]; global "Cmd"])) (const _RP_Replace)))]); (andList [(andList [(eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True))]); (neg (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_E)))])]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_NI_Local_Get_Nak3 =
  let name = "n_NI_Local_Get_Nak3" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (neg (eqn (var (record [global "Sta"; arr [("RpMsg", [paramref "src"])]; global "Cmd"])) (const _RP_Replace)))]); (andList [(andList [(eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src")))])]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_NI_Local_Get_Get =
  let name = "n_NI_Local_Get_Get" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (neg (eqn (var (record [global "Sta"; arr [("RpMsg", [paramref "src"])]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _False))]); (neg (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src"))))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Get)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (var (record [global "Sta"; global "Dir"; global "HeadPtr"])))]) in
  rule name params formula statement

let n_NI_Local_Get_Put1_HeadVld =
  let name = "n_NI_Local_Get_Put1_HeadVld" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (neg (eqn (var (record [global "Sta"; arr [("RpMsg", [paramref "src"])]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "src"])]]) (const (boolc true))); (forStatement (ifelseStatement (eqn (param (paramref "p")) (param (paramref "src"))) (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true))) (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]])))) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Put)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_NI_Local_Get_Put1 =
  let name = "n_NI_Local_Get_Put1" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (neg (eqn (var (record [global "Sta"; arr [("RpMsg", [paramref "src"])]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _False))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Put)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_NI_Local_Get_Put2_Dirty =
  let name = "n_NI_Local_Get_Put2_Dirty" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (neg (eqn (var (record [global "Sta"; arr [("RpMsg", [paramref "src"])]; global "Cmd"])) (const _RP_Replace)))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True))]); (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_S)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Put)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_NI_Remote_Get_Nak =
  let name = "n_NI_Remote_Get_Nak" in
  let params = [paramdef "src" "NODE"; paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (neg (eqn (param (paramref "dst")) (param _Home)))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param (paramref "dst")))]); (neg (eqn (var (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"])) (const _CACHE_E)))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "Sta"; global "NakcMsg"; global "Cmd"]) (const _NAKC_Nakc))]) in
  rule name params formula statement

let n_NI_Remote_Get_Put =
  let name = "n_NI_Remote_Get_Put" in
  let params = [paramdef "src" "NODE"; paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (neg (eqn (param (paramref "dst")) (param _Home)))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_Get))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param (paramref "dst")))]); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"]) (const _CACHE_S)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Put)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param (paramref "dst"))); (ifStatement (neg (eqn (param (paramref "src")) (param _Home))) (parallel [(assign (record [global "Sta"; global "ShWbMsg"; global "Cmd"]) (const _SHWB_ShWb)); (assign (record [global "Sta"; global "ShWbMsg"; global "Proc"]) (param (paramref "src")))]))]) in
  rule name params formula statement

let n_NI_Local_GetX_Nak1 =
  let name = "n_NI_Local_GetX_Nak1" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _True))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_NI_Local_GetX_Nak2 =
  let name = "n_NI_Local_GetX_Nak2" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (andList [(andList [(eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True))]); (neg (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_E)))])]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_NI_Local_GetX_Nak3 =
  let name = "n_NI_Local_GetX_Nak3" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (andList [(andList [(eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src")))])]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_NI_Local_GetX_GetX =
  let name = "n_NI_Local_GetX_GetX" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _False))]); (neg (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src"))))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_GetX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (var (record [global "Sta"; global "Dir"; global "HeadPtr"])))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX_1 =
  let name = "n_NI_Local_GetX_PutX_1" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True))]); (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_Get))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false)))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "InvMarked"]) (const (boolc true)))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX_2 =
  let name = "n_NI_Local_GetX_PutX_2" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True))]); (neg (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_Get)))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false)))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX_3 =
  let name = "n_NI_Local_GetX_PutX_3" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _False))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false)))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX_4 =
  let name = "n_NI_Local_GetX_PutX_4" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src")))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True))]); (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_Get))]); (forallFormula [paramdef "p" "NODE"] (eqn (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]])) (const _False)))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false)))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "InvMarked"]) (const (boolc true)))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX_5 =
  let name = "n_NI_Local_GetX_PutX_5" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src")))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True))]); (neg (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_Get)))]); (forallFormula [paramdef "p" "NODE"] (eqn (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]])) (const _False)))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false)))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX_6 =
  let name = "n_NI_Local_GetX_PutX_6" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src")))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _False))]); (forallFormula [paramdef "p" "NODE"] (eqn (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]])) (const _False)))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false)))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX_7 =
  let name = "n_NI_Local_GetX_PutX_7" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True))]); (neg (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src"))))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True))]); (neg (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_Get)))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))) [paramdef "p" "NODE"]); (forStatement (ifelseStatement (eqn (param (paramref "p")) (param _Home)) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (eqn (param (paramref "p")) (param (paramref "src"))) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (andList [(eqn (var (record [global "Sta"; global "Dir"; global "ShrVld"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]])) (const _True))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_Inv))]) (ifelseStatement (andList [(eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "p")))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]))))) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX_9 =
  let name = "n_NI_Local_GetX_PutX_9" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True))]); (neg (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src"))))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _False))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))) [paramdef "p" "NODE"]); (forStatement (ifelseStatement (eqn (param (paramref "p")) (param _Home)) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (eqn (param (paramref "p")) (param (paramref "src"))) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (andList [(eqn (var (record [global "Sta"; global "Dir"; global "ShrVld"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]])) (const _True))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_Inv))]) (ifelseStatement (andList [(eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "p")))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]))))) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX_11 =
  let name = "n_NI_Local_GetX_PutX_11" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True))]); (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false)))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX_8 =
  let name = "n_NI_Local_GetX_PutX_8" in
  let params = [paramdef "src" "NODE"; paramdef "pp" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src")))]); (eqn (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "pp"])]])) (const _True))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True))]); (neg (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_Get)))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))) [paramdef "p" "NODE"]); (forStatement (ifelseStatement (eqn (param (paramref "p")) (param _Home)) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (eqn (param (paramref "p")) (param (paramref "src"))) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (andList [(eqn (var (record [global "Sta"; global "Dir"; global "ShrVld"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]])) (const _True))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_Inv))]) (ifelseStatement (andList [(eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "p")))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]))))) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Local_GetX_PutX_10 =
  let name = "n_NI_Local_GetX_PutX_10" in
  let params = [paramdef "src" "NODE"; paramdef "pp" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param _Home))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "src")))]); (eqn (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "pp"])]])) (const _True))]); (eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _False))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (param (paramref "src"))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))) [paramdef "p" "NODE"]); (forStatement (ifelseStatement (eqn (param (paramref "p")) (param _Home)) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (eqn (param (paramref "p")) (param (paramref "src"))) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (andList [(eqn (var (record [global "Sta"; global "Dir"; global "ShrVld"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]])) (const _True))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_Inv))]) (ifelseStatement (andList [(eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; global "HeadPtr"])) (param (paramref "p")))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]))))) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param _Home))]) in
  rule name params formula statement

let n_NI_Remote_GetX_Nak =
  let name = "n_NI_Remote_GetX_Nak" in
  let params = [paramdef "src" "NODE"; paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (neg (eqn (param (paramref "dst")) (param _Home)))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param (paramref "dst")))]); (neg (eqn (var (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"])) (const _CACHE_E)))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_Nak)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param (paramref "dst"))); (assign (record [global "Sta"; global "NakcMsg"; global "Cmd"]) (const _NAKC_Nakc))]) in
  rule name params formula statement

let n_NI_Remote_GetX_PutX =
  let name = "n_NI_Remote_GetX_PutX" in
  let params = [paramdef "src" "NODE"; paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param (paramref "dst")))); (neg (eqn (param (paramref "dst")) (param _Home)))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"])) (const _UNI_GetX))]); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"])) (param (paramref "dst")))]); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"])) (const _CACHE_E))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Cmd"]) (const _UNI_PutX)); (assign (record [global "Sta"; arr [("UniMsg", [paramref "src"])]; global "Proc"]) (param (paramref "dst"))); (ifStatement (neg (eqn (param (paramref "src")) (param _Home))) (parallel [(assign (record [global "Sta"; global "ShWbMsg"; global "Cmd"]) (const _SHWB_FAck)); (assign (record [global "Sta"; global "ShWbMsg"; global "Proc"]) (param (paramref "src")))]))]) in
  rule name params formula statement

let n_NI_Remote_Put =
  let name = "n_NI_Remote_Put" in
  let params = [paramdef "dst" "NODE"] in
  let formula = (andList [(neg (eqn (param (paramref "dst")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "dst"])]; global "Cmd"])) (const _UNI_Put))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [paramref "dst"])]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "ProcCmd"]) (const _NODE_None)); (ifelseStatement (eqn (var (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "InvMarked"])) (const _True)) (parallel [(assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "InvMarked"]) (const (boolc false))); (assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"]) (const _CACHE_I))]) (assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"]) (const _CACHE_S)))]) in
  rule name params formula statement

let n_NI_Remote_PutX =
  let name = "n_NI_Remote_PutX" in
  let params = [paramdef "dst" "NODE"] in
  let formula = (andList [(andList [(neg (eqn (param (paramref "dst")) (param _Home))); (eqn (var (record [global "Sta"; arr [("UniMsg", [paramref "dst"])]; global "Cmd"])) (const _UNI_PutX))]); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "ProcCmd"])) (const _NODE_GetX))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [paramref "dst"])]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "ProcCmd"]) (const _NODE_None)); (assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "InvMarked"]) (const (boolc false))); (assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"]) (const _CACHE_E))]) in
  rule name params formula statement

let n_NI_Inv =
  let name = "n_NI_Inv" in
  let params = [paramdef "dst" "NODE"] in
  let formula = (andList [(neg (eqn (param (paramref "dst")) (param _Home))); (eqn (var (record [global "Sta"; arr [("InvMsg", [paramref "dst"])]; global "Cmd"])) (const _INV_Inv))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("InvMsg", [paramref "dst"])]; global "Cmd"]) (const _INV_InvAck)); (assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "CacheState"]) (const _CACHE_I)); (ifStatement (eqn (var (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "ProcCmd"])) (const _NODE_Get)) (assign (record [global "Sta"; arr [("Proc", [paramref "dst"])]; global "InvMarked"]) (const (boolc true))))]) in
  rule name params formula statement

let n_NI_InvAck1 =
  let name = "n_NI_InvAck1" in
  let params = [paramdef "src" "NODE"; paramdef "p" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("InvMsg", [paramref "src"])]; global "Cmd"])) (const _INV_InvAck))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _True))]); (eqn (var (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "src"])]])) (const _True))]); (neg (eqn (param (paramref "p")) (param (paramref "src"))))]); (eqn (var (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]])) (const _True))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("InvMsg", [paramref "src"])]; global "Cmd"]) (const _INV_None)); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "src"])]]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_InvAck2 =
  let name = "n_NI_InvAck2" in
  let params = [paramdef "src" "NODE"] in
  let formula = (andList [(andList [(andList [(andList [(neg (eqn (param (paramref "src")) (param _Home))); (eqn (var (record [global "Sta"; arr [("InvMsg", [paramref "src"])]; global "Cmd"])) (const _INV_InvAck))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _True))]); (eqn (var (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "src"])]])) (const _True))]); (forallFormula [paramdef "p" "NODE"] (andList [(neg (eqn (param (paramref "p")) (param (paramref "src")))); (eqn (var (record [global "Sta"; global "Dir"; arr [("InvSet", [_Home])]])) (const _False))]))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("InvMsg", [paramref "src"])]; global "Cmd"]) (const _INV_None)); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "src"])]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc false))); (ifStatement (andList [(eqn (var (record [global "Sta"; global "Dir"; global "Local"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]) (assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))))]) in
  rule name params formula statement

let n_NI_Replace =
  let name = "n_NI_Replace" in
  let params = [paramdef "src" "NODE"] in
  let formula = (eqn (var (record [global "Sta"; arr [("RpMsg", [paramref "src"])]; global "Cmd"])) (const _RP_Replace)) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("RpMsg", [paramref "src"])]; global "Cmd"]) (const _RP_None)); (ifStatement (eqn (var (record [global "Sta"; global "Dir"; global "ShrVld"])) (const _True)) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "src"])]]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "src"])]]) (const (boolc false)))]))]) in
  rule name params formula statement

let n_PI_Local_Get_Get =
  let name = "n_PI_Local_Get_Get" in
  let params = [] in
  let formula = (andList [(andList [(andList [(eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_None)); (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_I))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"]) (const _NODE_Get)); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Sta"; arr [("UniMsg", [_Home])]; global "Cmd"]) (const _UNI_Get)); (assign (record [global "Sta"; arr [("UniMsg", [_Home])]; global "Proc"]) (var (record [global "Sta"; global "Dir"; global "HeadPtr"])))]) in
  rule name params formula statement

let n_PI_Local_Get_Put =
  let name = "n_PI_Local_Get_Put" in
  let params = [] in
  let formula = (andList [(andList [(andList [(eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_None)); (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_I))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc true))); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"]) (const _NODE_None)); (ifelseStatement (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "InvMarked"])) (const _True)) (parallel [(assign (record [global "Sta"; arr [("Proc", [_Home])]; global "InvMarked"]) (const (boolc false))); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I))]) (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_S)))]) in
  rule name params formula statement

let n_PI_Local_GetX_GetX1 =
  let name = "n_PI_Local_GetX_GetX1" in
  let params = [] in
  let formula = (andList [(andList [(andList [(eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_None)); (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_I))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"]) (const _NODE_GetX)); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Sta"; arr [("UniMsg", [_Home])]; global "Cmd"]) (const _UNI_GetX)); (assign (record [global "Sta"; arr [("UniMsg", [_Home])]; global "Proc"]) (var (record [global "Sta"; global "Dir"; global "HeadPtr"])))]) in
  rule name params formula statement

let n_PI_Local_GetX_GetX2 =
  let name = "n_PI_Local_GetX_GetX2" in
  let params = [] in
  let formula = (andList [(andList [(andList [(eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_None)); (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_S))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True))]) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"]) (const _NODE_GetX)); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Sta"; arr [("UniMsg", [_Home])]; global "Cmd"]) (const _UNI_GetX)); (assign (record [global "Sta"; arr [("UniMsg", [_Home])]; global "Proc"]) (var (record [global "Sta"; global "Dir"; global "HeadPtr"])))]) in
  rule name params formula statement

let n_PI_Local_GetX_PutX_HeadVld =
  let name = "n_PI_Local_GetX_PutX_HeadVld" in
  let params = [] in
  let formula = (andList [(andList [(andList [(andList [(eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_None)); (neg (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_E)))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _True))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc false))); (forStatement (parallel [(assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc false))); (ifelseStatement (eqn (param (paramref "p")) (param _Home)) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]) (ifelseStatement (eqn (param (paramref "p")) (var (record [global "Sta"; global "Dir"; global "HeadPtr"]))) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_Inv))]) (ifelseStatement (andList [(eqn (var (record [global "Sta"; global "Dir"; global "ShrVld"])) (const _True)); (eqn (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]])) (const _True))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_Inv))]) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc false))); (assign (record [global "Sta"; arr [("InvMsg", [paramref "p"])]; global "Cmd"]) (const _INV_None))]))))]) [paramdef "p" "NODE"]); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"]) (const _NODE_None)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "InvMarked"]) (const (boolc false))); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_E))]) in
  rule name params formula statement

let n_PI_Local_GetX_PutX =
  let name = "n_PI_Local_GetX_PutX" in
  let params = [] in
  let formula = (andList [(andList [(andList [(andList [(eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_None)); (neg (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_E)))]); (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _False))]); (eqn (var (record [global "Sta"; global "Dir"; global "HeadVld"])) (const _False))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc true))); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"]) (const _NODE_None)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "InvMarked"]) (const (boolc false))); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_E))]) in
  rule name params formula statement

let n_PI_Local_PutX =
  let name = "n_PI_Local_PutX" in
  let params = [] in
  let formula = (andList [(eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_None)); (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_E))]) in
  let statement = (ifelseStatement (eqn (var (record [global "Sta"; global "Dir"; global "Pending"])) (const _True)) (parallel [(assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc false)))]) (parallel [(assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I)); (assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc false)))])) in
  rule name params formula statement

let n_PI_Local_Replace =
  let name = "n_PI_Local_Replace" in
  let params = [] in
  let formula = (andList [(eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"])) (const _NODE_None)); (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"])) (const _CACHE_S))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc false))); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I))]) in
  rule name params formula statement

let n_NI_Nak_Clear =
  let name = "n_NI_Nak_Clear" in
  let params = [] in
  let formula = (eqn (var (record [global "Sta"; global "NakcMsg"; global "Cmd"])) (const _NAKC_Nakc)) in
  let statement = (parallel [(assign (record [global "Sta"; global "NakcMsg"; global "Cmd"]) (const _NAKC_None)); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_Local_Put =
  let name = "n_NI_Local_Put" in
  let params = [] in
  let formula = (eqn (var (record [global "Sta"; arr [("UniMsg", [_Home])]; global "Cmd"])) (const _UNI_Put)) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [_Home])]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc true))); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"]) (const _NODE_None)); (ifelseStatement (eqn (var (record [global "Sta"; arr [("Proc", [_Home])]; global "InvMarked"])) (const _True)) (parallel [(assign (record [global "Sta"; arr [("Proc", [_Home])]; global "InvMarked"]) (const (boolc false))); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_I))]) (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_S)))]) in
  rule name params formula statement

let n_NI_Local_PutXAcksDone =
  let name = "n_NI_Local_PutXAcksDone" in
  let params = [] in
  let formula = (eqn (var (record [global "Sta"; arr [("UniMsg", [_Home])]; global "Cmd"])) (const _UNI_PutX)) in
  let statement = (parallel [(assign (record [global "Sta"; arr [("UniMsg", [_Home])]; global "Cmd"]) (const _UNI_None)); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Local"]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc false))); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "ProcCmd"]) (const _NODE_None)); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "InvMarked"]) (const (boolc false))); (assign (record [global "Sta"; arr [("Proc", [_Home])]; global "CacheState"]) (const _CACHE_E))]) in
  rule name params formula statement

let n_NI_Wb =
  let name = "n_NI_Wb" in
  let params = [] in
  let formula = (eqn (var (record [global "Sta"; global "WbMsg"; global "Cmd"])) (const _WB_Wb)) in
  let statement = (parallel [(assign (record [global "Sta"; global "WbMsg"; global "Cmd"]) (const _WB_None)); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const (boolc false)))]) in
  rule name params formula statement

let n_NI_FAck =
  let name = "n_NI_FAck" in
  let params = [] in
  let formula = (eqn (var (record [global "Sta"; global "ShWbMsg"; global "Cmd"])) (const _SHWB_FAck)) in
  let statement = (parallel [(assign (record [global "Sta"; global "ShWbMsg"; global "Cmd"]) (const _SHWB_None)); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc false))); (ifStatement (eqn (var (record [global "Sta"; global "Dir"; global "Dirty"])) (const _True)) (assign (record [global "Sta"; global "Dir"; global "HeadPtr"]) (var (record [global "Sta"; global "ShWbMsg"; global "Proc"]))))]) in
  rule name params formula statement

let n_NI_ShWb_Home =
  let name = "n_NI_ShWb_Home" in
  let params = [] in
  let formula = (andList [(eqn (var (record [global "Sta"; global "ShWbMsg"; global "Cmd"])) (const _SHWB_ShWb)); (eqn (var (record [global "Sta"; global "ShWbMsg"; global "Proc"])) (param _Home))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "ShWbMsg"; global "Cmd"]) (const _SHWB_None)); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc true))); (forStatement (ifelseStatement (eqn (param (paramref "p")) (param _Home)) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true)))]) (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]])))) [paramdef "p" "NODE"])]) in
  rule name params formula statement

let n_NI_ShWb =
  let name = "n_NI_ShWb" in
  let params = [] in
  let formula = (andList [(eqn (var (record [global "Sta"; global "ShWbMsg"; global "Cmd"])) (const _SHWB_ShWb)); (neg (eqn (var (record [global "Sta"; global "ShWbMsg"; global "Proc"])) (param _Home)))]) in
  let statement = (parallel [(assign (record [global "Sta"; global "ShWbMsg"; global "Cmd"]) (const _SHWB_None)); (assign (record [global "Sta"; global "Dir"; global "Pending"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const (boolc false))); (assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const (boolc true))); (forStatement (ifelseStatement (eqn (param (paramref "p")) (param _Home)) (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]))) (ifelseStatement (eqn (param (paramref "p")) (var (record [global "Sta"; global "ShWbMsg"; global "Proc"]))) (parallel [(assign (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]) (const (boolc true))); (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (const (boolc true)))]) (assign (record [global "Sta"; global "Dir"; arr [("InvSet", [paramref "p"])]]) (var (record [global "Sta"; global "Dir"; arr [("ShrSet", [paramref "p"])]]))))) [paramdef "p" "NODE"])]) in
  rule name params formula statement

let rules = [n_PI_Remote_Get; n_PI_Remote_GetX; n_PI_Remote_PutX; n_PI_Remote_Replace; n_NI_Nak; n_NI_Local_Get_Nak1; n_NI_Local_Get_Nak2; n_NI_Local_Get_Nak3; n_NI_Local_Get_Get; n_NI_Local_Get_Put1_HeadVld; n_NI_Local_Get_Put1; n_NI_Local_Get_Put2_Dirty; n_NI_Remote_Get_Nak; n_NI_Remote_Get_Put; n_NI_Local_GetX_Nak1; n_NI_Local_GetX_Nak2; n_NI_Local_GetX_Nak3; n_NI_Local_GetX_GetX; n_NI_Local_GetX_PutX_1; n_NI_Local_GetX_PutX_2; n_NI_Local_GetX_PutX_3; n_NI_Local_GetX_PutX_4; n_NI_Local_GetX_PutX_5; n_NI_Local_GetX_PutX_6; n_NI_Local_GetX_PutX_7; n_NI_Local_GetX_PutX_9; n_NI_Local_GetX_PutX_11; n_NI_Local_GetX_PutX_8; n_NI_Local_GetX_PutX_10; n_NI_Remote_GetX_Nak; n_NI_Remote_GetX_PutX; n_NI_Remote_Put; n_NI_Remote_PutX; n_NI_Inv; n_NI_InvAck1; n_NI_InvAck2; n_NI_Replace; n_PI_Local_Get_Get; n_PI_Local_Get_Put; n_PI_Local_GetX_GetX1; n_PI_Local_GetX_GetX2; n_PI_Local_GetX_PutX_HeadVld; n_PI_Local_GetX_PutX; n_PI_Local_PutX; n_PI_Local_Replace; n_NI_Nak_Clear; n_NI_Local_Put; n_NI_Local_PutXAcksDone; n_NI_Wb; n_NI_FAck; n_NI_ShWb_Home; n_NI_ShWb]

let n_CacheStateProp =
  let name = "n_CacheStateProp" in
  let params = [] in
  let formula = (forallFormula [paramdef "p" "NODE"] (forallFormula [paramdef "q" "NODE"] (imply (neg (eqn (param (paramref "p")) (param (paramref "q")))) (neg (andList [(eqn (var (record [global "Sta"; arr [("Proc", [paramref "p"])]; global "CacheState"])) (const _CACHE_E)); (eqn (var (record [global "Sta"; arr [("Proc", [paramref "q"])]; global "CacheState"])) (const _CACHE_E))]))))) in
  prop name params formula

let properties = [n_CacheStateProp]


let protocol = {
  name = "n_flash_nodata";
  types;
  vardefs;
  init;
  rules;
  properties;
}

let () = run_with_cmdline (find ~protocol ~murphi:(In_channel.read_all "n_flash_nodata.m"))

