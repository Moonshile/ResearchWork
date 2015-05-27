

open Core.Std
open Utils
open Paramecium
open Loach
open Formula
open InvFinder

(* Constants *)
let _True = boolc true
let _False = boolc false

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


(* Self-defined types *)
let types = [
  enum "boolean" [_True; _False];
  enum "NODE" (int_consts [0; 1; 2; 3]);
  enum "DATA" (int_consts [1; 2]);
  enum "CACHE_STATE" [_CACHE_I; _CACHE_S; _CACHE_E];
  enum "NODE_CMD" [_NODE_None; _NODE_Get; _NODE_GetX];
  enum "UNI_CMD" [_UNI_None; _UNI_Get; _UNI_GetX; _UNI_Put; _UNI_PutX; _UNI_Nak];
  enum "INV_CMD" [_INV_None; _INV_Inv; _INV_InvAck];
  enum "RP_CMD" [_RP_None; _RP_Replace];
  enum "WB_CMD" [_WB_None; _WB_Wb];
  enum "SHWB_CMD" [_SHWB_None; _SHWB_ShWb; _SHWB_FAck];
  enum "NAKC_CMD" [_NAKC_None; _NAKC_Nakc];
]


let _NODE_STATE  = List.concat [
  [arrdef "ProcCmd" [] "NODE_CMD"];
  [arrdef "InvMarked" [] "boolean"];
  [arrdef "CacheState" [] "CACHE_STATE"];
  [arrdef "CacheData" [] "DATA"]
]

let _DIR_STATE  = List.concat [
  [arrdef "Pending" [] "boolean"];
  [arrdef "Local" [] "boolean"];
  [arrdef "Dirty" [] "boolean"];
  [arrdef "HeadVld" [] "boolean"];
  [arrdef "HeadPtr" [] "NODE"];
  [arrdef "ShrVld" [] "boolean"];
  [arrdef "ShrSet" [paramdef "i0" "NODE"] "boolean"];
  [arrdef "InvSet" [paramdef "i1" "NODE"] "boolean"]
]

let _UNI_MSG  = List.concat [
  [arrdef "Cmd" [] "UNI_CMD"];
  [arrdef "Proc" [] "NODE"];
  [arrdef "Data" [] "DATA"]
]

let _INV_MSG  = List.concat [
  [arrdef "Cmd" [] "INV_CMD"]
]

let _RP_MSG  = List.concat [
  [arrdef "Cmd" [] "RP_CMD"]
]

let _WB_MSG  = List.concat [
  [arrdef "Cmd" [] "WB_CMD"];
  [arrdef "Proc" [] "NODE"];
  [arrdef "Data" [] "DATA"]
]

let _SHWB_MSG  = List.concat [
  [arrdef "Cmd" [] "SHWB_CMD"];
  [arrdef "Proc" [] "NODE"];
  [arrdef "Data" [] "DATA"]
]

let _NAKC_MSG  = List.concat [
  [arrdef "Cmd" [] "NAKC_CMD"]
]

let _STATE  = List.concat [
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


(* Variables *)
let vardefs = List.concat [
  [arrdef "Home" [] "NODE"];
  record_def "Sta" [] _STATE;
]

let home = const (intc 0)

(* init *)
let init = parallel [
  assign (global "Home") (const (intc 0));
  assign (record [global "Sta"; global "MemData"]) (const (intc 1));
  assign (record [global "Sta"; global "Dir"; global "Pending"]) (const _False);
  assign (record [global "Sta"; global "Dir"; global "Local"]) (const _False);
  assign (record [global "Sta"; global "Dir"; global "Dirty"]) (const _False);
  assign (record [global "Sta"; global "Dir"; global "HeadVld"]) (const _False);
  assign (record [global "Sta"; global "Dir"; global "ShrVld"]) (const _False);
  assign (record [global "Sta"; global "WbMsg"; global "Cmd"]) (const _WB_None);
  assign (record [global "Sta"; global "ShWbMsg"; global "Cmd"]) (const _SHWB_None);
  assign (record [global "Sta"; global "NakcMsg"; global "Cmd"]) (const _NAKC_None);
  forStatement (parallel [
    assign (record [global "Sta"; arr "Proc" [paramref "p"]; global "ProcCmd"]) (const _NODE_None);
    assign (record [global "Sta"; arr "Proc" [paramref "p"]; global "InvMarked"]) (const _False);
    assign (record [global "Sta"; arr "Proc" [paramref "p"]; global "CacheState"]) (const _CACHE_I);
    assign (record [global "Sta"; global "Dir"; arr "ShrSet" [paramref "p"]]) (const _False);
    assign (record [global "Sta"; global "Dir"; arr "InvSet" [paramref "p"]]) (const _False);
    assign (record [global "Sta"; arr "UniMsg" [paramref "p"]; global "Cmd"]) (const _UNI_None);
    assign (record [global "Sta"; arr "InvMsg" [paramref "p"]; global "Cmd"]) (const _INV_None);
    assign (record [global "Sta"; arr "RpMsg" [paramref "p"]; global "Cmd"]) (const _RP_None);
  ]) [paramdef "p" "NODE"];
  assign (record [global "Sta"; global "CurrData"]) (const (intc 1));
  assign (record [global "Sta"; global "PrevData"]) (const (intc 1));
  assign (record [global "Sta"; global "LastWrVld"]) (const _False);
  assign (record [global "Sta"; global "Collecting"]) (const _False);
  assign (record [global "Sta"; global "FwdCmd"]) (const _UNI_None);
]

let rules = []

let properties = []

let protocol = Trans.act {
  name = "mutualEx";
  types;
  vardefs;
  init;
  rules;
  properties;
};;

printf "%s" (ToStr.Smv.protocol_act protocol)

