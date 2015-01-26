theory flash imports cache
begin
section{*Main defintions*}
   
   consts N::nat

   definition Home::"nat"   where [simp]: "Home\<equiv>Suc N"

(***************definitions for the enumvalues types****************************************)
(***************definitions for the enumvalues types****************************************)
definition UNI_None::"nat"  where [simp]: "UNI_None\<equiv>  10"   

definition UNI_Get::"nat"  where [simp]: "UNI_Get\<equiv>  11"   

definition UNI_GetX::"nat"  where [simp]: "UNI_GetX\<equiv>  29"   

definition UNI_Put::"nat"  where [simp]: "UNI_Put\<equiv>  12"   

definition UNI_PutX::"nat"  where [simp]: "UNI_PutX\<equiv>  13"   

definition UNI_Nak::"nat"  where [simp]: "UNI_Nak\<equiv>  14"   

definition UNI_CMDType::"nat set" where [simp]: "UNI_CMDType \<equiv> {UNI_None,UNI_Get,UNI_GetX,UNI_Put,UNI_PutX,UNI_Nak}"

definition true::"nat"  where [simp]: "true\<equiv>  27"   

definition false::"nat"  where [simp]: "false\<equiv>  28"   

definition Bool::"nat set" where [simp]: "Bool \<equiv> {true,false}"

definition INV_None::"nat"  where [simp]: "INV_None\<equiv>  15"   

definition INV_Inv::"nat"  where [simp]: "INV_Inv\<equiv>  16"   

definition INV_InvAck::"nat"  where [simp]: "INV_InvAck\<equiv>  17"   

definition INV_CMDType::"nat set" where [simp]: "INV_CMDType \<equiv> {INV_None,INV_Inv,INV_InvAck}"

definition WB_None::"nat"  where [simp]: "WB_None\<equiv>  20"   

definition WB_Wb::"nat"  where [simp]: "WB_Wb\<equiv>  21"   

definition WB_CMDType::"nat set" where [simp]: "WB_CMDType \<equiv> {WB_None,WB_Wb}"

definition NAKC_None::"nat"  where [simp]: "NAKC_None\<equiv>  25"   

definition NAKC_Nakc::"nat"  where [simp]: "NAKC_Nakc\<equiv>  26"   

definition NAKC_CMDType::"nat set" where [simp]: "NAKC_CMDType \<equiv> {NAKC_None,NAKC_Nakc}"

definition Int::"nat set" where [simp]: "Int \<equiv> {,}"

definition RP_None::"nat"  where [simp]: "RP_None\<equiv>  18"   

definition RP_Replace::"nat"  where [simp]: "RP_Replace\<equiv>  19"   

definition RP_CMDType::"nat set" where [simp]: "RP_CMDType \<equiv> {RP_None,RP_Replace}"

definition Int::"nat set" where [simp]: "Int \<equiv> {,,}"

definition NODE_None::"nat"  where [simp]: "NODE_None\<equiv>  7"   

definition NODE_Get::"nat"  where [simp]: "NODE_Get\<equiv>  8"   

definition NODE_GetX::"nat"  where [simp]: "NODE_GetX\<equiv>  9"   

definition NODE_CMDType::"nat set" where [simp]: "NODE_CMDType \<equiv> {NODE_None,NODE_Get,NODE_GetX}"

definition SHWB_None::"nat"  where [simp]: "SHWB_None\<equiv>  22"   

definition SHWB_ShWb::"nat"  where [simp]: "SHWB_ShWb\<equiv>  23"   

definition SHWB_FAck::"nat"  where [simp]: "SHWB_FAck\<equiv>  24"   

definition SHWB_CMDType::"nat set" where [simp]: "SHWB_CMDType \<equiv> {SHWB_None,SHWB_ShWb,SHWB_FAck}"

definition CACHE_I::"nat"  where [simp]: "CACHE_I\<equiv>  4"   

definition CACHE_S::"nat"  where [simp]: "CACHE_S\<equiv>  5"   

definition CACHE_E::"nat"  where [simp]: "CACHE_E\<equiv>  6"   

definition CACHE_STATEType::"nat set" where [simp]: "CACHE_STATEType \<equiv> {CACHE_I,CACHE_S,CACHE_E}"

(***************definitions for the axioms ****************************************)

axiomatization where axiomOn_CacheState  [simp]:"   s (IVar (Para ''CacheState'' i )) \<in> CACHE_STATEType "

axiomatization where axiomOn_Collecting  [simp]:"   s (IVar (Global ''Collecting'' )) \<in> Bool "


axiomatization where axiomOn_Dir-InvSet  [simp]:"   s (IVar (Para ''Dir-InvSet'' i )) \<in> Bool "

axiomatization where axiomOn_Dir-ShrSet  [simp]:"   s (IVar (Para ''Dir-ShrSet'' i )) \<in> Bool "

axiomatization where axiomOn_Dir.Dirty  [simp]:"   s (IVar (Global ''Dir.Dirty'' )) \<in> Bool "


axiomatization where axiomOn_Dir.HeadVld  [simp]:"   s (IVar (Global ''Dir.HeadVld'' )) \<in> Bool "

axiomatization where axiomOn_Dir.Pending  [simp]:"   s (IVar (Global ''Dir.Pending'' )) \<in> Bool "

axiomatization where axiomOn_Dir.ShrVld  [simp]:"   s (IVar (Global ''Dir.ShrVld'' )) \<in> Bool "

axiomatization where axiomOn_Dir.local  [simp]:"   s (IVar (Global ''Dir.local'' )) \<in> Bool "

axiomatization where axiomOn_FwdCmd  [simp]:"   s (IVar (Global ''FwdCmd'' )) \<in> UNI_CMDType "


axiomatization where axiomOn_InvMarked  [simp]:"   s (IVar (Para ''InvMarked'' i )) \<in> Bool "

axiomatization where axiomOn_InvMsg.Cmd  [simp]:"   s (IVar (Para ''InvMsg.Cmd'' i )) \<in> INV_CMDType "




axiomatization where axiomOn_LastWrVld  [simp]:"   s (IVar (Global ''LastWrVld'' )) \<in> Bool "


axiomatization where axiomOn_NakcMsg.Cmd  [simp]:"   s (IVar (Global ''NakcMsg.Cmd'' )) \<in> NAKC_CMDType "



axiomatization where axiomOn_RpMsg.Cmd  [simp]:"   s (IVar (Para ''RpMsg.Cmd'' i )) \<in> RP_CMDType "

axiomatization where axiomOn_ShWbMsg.Cmd  [simp]:"   s (IVar (Global ''ShWbMsg.Cmd'' )) \<in> SHWB_CMDType "



axiomatization where axiomOn_UniMsg.Cmd  [simp]:"   s (IVar (Para ''UniMsg.Cmd'' i )) \<in> UNI_CMDType "



axiomatization where axiomOn_WbMsg.Cmd  [simp]:"   s (IVar (Global ''WbMsg.Cmd'' )) \<in> WB_CMDType "



axiomatization where axiomOn_procCmd  [simp]:"   s (IVar (Para ''procCmd'' i )) \<in> NODE_CMDType "

definition NI_FAck::"   rule" where [simp]:
" NI_FAck    \<equiv>   
let g=( eqn ( IVar ( Global ''ShWbMsg.Cmd'') )  ( Const SHWB_FAck ))  in 
let S=(
let S_1=( assign  (( Global ''ShWbMsg.Cmd''),  ( Const SHWB_None ) ) ) in
let S_2=( assign  (( Global ''Dir.Pending''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.HeadPtr''),  (iteForm ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))   ( IVar ( Global ''ShWbMsg.proc'') )  ( IVar ( Global ''Dir.HeadPtr'') )) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_Inv::" nat \<Rightarrow> rule" where [simp]:
" NI_Inv  iInv1 \<equiv>   
let g=( eqn ( IVar ( Para ''InvMsg.Cmd'' iInv1) )  ( Const INV_Inv ))  in 
let S=(
let S_1=( assign  (( Para ''InvMsg.Cmd'' iInv1),  ( Const INV_InvAck ) ) ) in
let S_2=( assign  (( Para ''CacheState'' iInv1),  ( Const CACHE_I ) ) ) in
let S_3=( assign  (( Para ''InvMarked'' iInv1),  (iteForm ( eqn ( IVar ( Para ''procCmd'' iInv1) )  ( Const NODE_Get ))   ( Const true )  ( IVar ( Para ''InvMarked'' iInv1) )) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_InvAck_1::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_InvAck_1  iInv1  iInv2 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm  (neg ( eqn  (Const iInv1)   (Const iInv2)) )    ( eqn ( IVar ( Para ''Dir-InvSet'' iInv2) )  ( Const true ))  )    ( eqn ( IVar ( Para ''Dir-InvSet'' iInv1) )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const true ))  )    ( eqn ( IVar ( Para ''InvMsg.Cmd'' iInv1) )  ( Const INV_InvAck ))  )  in 
let S=(
let S_1=( assign  (( Para ''InvMsg.Cmd'' iInv1),  ( Const INV_None ) ) ) in
let S_2=( assign  (( Para ''Dir-InvSet'' iInv1),  ( Const false ) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition NI_InvAck_1_Home::" nat \<Rightarrow> rule" where [simp]:
" NI_InvAck_1_Home  iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''InvMsg.Cmd'' iInv1) )  ( Const INV_InvAck ))    ( eqn ( IVar ( Para ''Dir-InvSet'' Home) )  ( Const true ))  )    ( eqn ( IVar ( Para ''Dir-InvSet'' iInv1) )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const true ))  )  in 
let S=(
let S_1=( assign  (( Para ''InvMsg.Cmd'' iInv1),  ( Const INV_None ) ) ) in
let S_2=( assign  (( Para ''Dir-InvSet'' iInv1),  ( Const false ) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition NI_InvAck_2::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_InvAck_2 N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm 
(    let natList=down N in 
    let paraForm=\<lambda>iInvForAll. ( andForm  (neg ( eqn (Const    iInvForAll )   (Const iInv1)) )    ( eqn ( IVar ( Para ''Dir-InvSet'' Home) )  ( Const false ))  )  in
       forallForm natList paraForm)
   ( eqn ( IVar ( Para ''Dir-InvSet'' iInv1) )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const true ))  )    ( eqn ( IVar ( Para ''InvMsg.Cmd'' iInv1) )  ( Const INV_InvAck ))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Pending''),  ( Const false ) ) ) in
let S_2=( assign  (( Global ''Dir.local''),  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )   ( Const false )  ( IVar ( Global ''Dir.local'') )) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition NI_Local_GetX_GetX::" nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_GetX  iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))     (neg ( eqn ( IVar ( Global ''Dir.HeadPtr'') )   (Const iInv1)) )  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_2=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_GetX ) ) ) in
let S_3=( assign  (( Para ''UniMsg.proc'' iInv1),  ( IVar ( Global ''Dir.HeadPtr'') ) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_Local_GetX_Nak1::" nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_Nak1  iInv1 \<equiv>   
let g=( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const true ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Nak ) ) ) in
let S_2=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition NI_Local_GetX_Nak2::" nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_Nak2  iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))     (neg ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_E )) )  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Nak ) ) ) in
let S_2=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition NI_Local_GetX_Nak3::" nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_Nak3  iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )   (Const iInv1))  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Nak ) ) ) in
let S_2=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition NI_Local_GetX_PutX1::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX1 N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))    ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_Get ))  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_2=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_3=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_5=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_6=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_8=( (let ps=% iInvForAll. let S_1=( assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) ) in
let S_2=( assign  (( Para ''Dir-InvSet'' iInvForAll),  ( Const false ) ) ) in
parallelList [S_1,S_2]

 in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_9=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_10=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
let S_11=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
let S_12=( assign  (( Para ''InvMarked'' Home),  ( Const true ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10,S_11,S_12]

) in 
guard g S"

definition NI_Local_GetX_PutX2::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX2 N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))     (neg ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_Get )) )  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_2=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_3=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_5=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_6=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_8=( (let ps=% iInvForAll. let S_1=( assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) ) in
let S_2=( assign  (( Para ''Dir-InvSet'' iInvForAll),  ( Const false ) ) ) in
parallelList [S_1,S_2]

 in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_9=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_10=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
let S_11=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
let S_12=( assign  (( Para ''InvMarked'' Home),  ( Const true ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10,S_11,S_12]

) in 
guard g S"

definition NI_Local_GetX_PutX3::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX3 N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_3=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_4=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_5=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_6=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_7=( (let ps=% iInvForAll. let S_1=( assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) ) in
let S_2=( assign  (( Para ''Dir-InvSet'' iInvForAll),  ( Const false ) ) ) in
parallelList [S_1,S_2]

 in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_8=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_9=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
let S_10=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10]

) in 
guard g S"

definition NI_Local_GetX_PutX4::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX4 N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))    
(    let natList=down N in 
    let paraForm=\<lambda>iInvForAll. ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const false ))  in
       forallForm natList paraForm)
 )    ( eqn ( IVar ( Para ''Dir-ShrSet'' Home) )  ( Const false ))  )    ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_Get ))  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_2=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_3=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_5=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_6=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_8=( (let ps=% iInvForAll. let S_1=( assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) ) in
let S_2=( assign  (( Para ''Dir-InvSet'' iInvForAll),  ( Const false ) ) ) in
parallelList [S_1,S_2]

 in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_9=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_10=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
let S_11=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10,S_11]

) in 
guard g S"

definition NI_Local_GetX_PutX5::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX5 N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))    
(    let natList=down N in 
    let paraForm=\<lambda>iInvForAll. ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const false ))  in
       forallForm natList paraForm)
 )    ( eqn ( IVar ( Para ''Dir-ShrSet'' Home) )  ( Const false ))  )     (neg ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_Get )) )  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )   (Const iInv1))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_2=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_3=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_5=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_6=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_8=( (let ps=% iInvForAll. let S_1=( assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) ) in
let S_2=( assign  (( Para ''Dir-InvSet'' iInvForAll),  ( Const false ) ) ) in
parallelList [S_1,S_2]

 in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_9=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_10=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
let S_11=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10,S_11]

) in 
guard g S"

definition NI_Local_GetX_PutX6::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX6 N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))    
(    let natList=down N in 
    let paraForm=\<lambda>iInvForAll. ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const false ))  in
       forallForm natList paraForm)
 )    ( eqn ( IVar ( Para ''Dir-ShrSet'' Home) )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )   (Const iInv1))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_3=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_4=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_5=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_6=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_7=( (let ps=% iInvForAll. let S_1=( assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) ) in
let S_2=( assign  (( Para ''Dir-InvSet'' iInvForAll),  ( Const false ) ) ) in
parallelList [S_1,S_2]

 in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_8=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_9=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
let S_10=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10]

) in 
guard g S"

definition NI_Local_GetX_PutX7::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX7 N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))     (neg ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_Get )) )  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))  )     (neg ( eqn ( IVar ( Global ''Dir.HeadPtr'') )   (Const iInv1)) )  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_5=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_6=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_8=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_9=( (let ps=% iInvForAll. assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_10=( (let ps=% iInvForAll. assign  (( Para ''Dir-InvSet'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const false )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const true )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const true )  ( Const false )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_11=( assign  (( Para ''InvMsg.Cmd'' Home),  ( Const INV_None ) ) ) in
let S_12=( (let ps=% iInvForAll. assign  (( Para ''InvMsg.Cmd'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const INV_None )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const INV_Inv )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const INV_Inv )  ( Const INV_None )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_13=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_14=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
let S_15=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10,S_11,S_12,S_13,S_14,S_15]

) in 
guard g S"

definition NI_Local_GetX_PutX8::" nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX8 N iInv1 iInv2\<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))     (neg ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_Get )) )  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))  )    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInv2) )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )   (Const iInv1))  )    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_5=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_6=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_8=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_9=( (let ps=% iInvForAll. assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_10=( (let ps=% iInvForAll. assign  (( Para ''Dir-InvSet'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const false )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const true )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const true )  ( Const false )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_11=( assign  (( Para ''InvMsg.Cmd'' Home),  ( Const INV_None ) ) ) in
let S_12=( (let ps=% iInvForAll. assign  (( Para ''InvMsg.Cmd'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const INV_None )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const INV_Inv )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const INV_Inv )  ( Const INV_None )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_13=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_14=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
let S_15=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10,S_11,S_12,S_13,S_14,S_15]

) in 
guard g S"

definition NI_Local_GetX_PutX8_home::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX8_home N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))     (neg ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_Get )) )  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))  )    ( eqn ( IVar ( Para ''Dir-ShrSet'' Home) )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )   (Const iInv1))  )    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_5=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_6=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_8=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_9=( (let ps=% iInvForAll. assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_10=( (let ps=% iInvForAll. assign  (( Para ''Dir-InvSet'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const false )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const true )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const true )  ( Const false )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_11=( assign  (( Para ''InvMsg.Cmd'' Home),  ( Const INV_None ) ) ) in
let S_12=( (let ps=% iInvForAll. assign  (( Para ''InvMsg.Cmd'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const INV_None )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const INV_Inv )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const INV_Inv )  ( Const INV_None )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_13=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_14=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
let S_15=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10,S_11,S_12,S_13,S_14,S_15]

) in 
guard g S"

definition NI_Local_GetX_PutX9::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX9 N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const false ))  )     (neg ( eqn ( IVar ( Global ''Dir.HeadPtr'') )   (Const iInv1)) )  )    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_5=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_6=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_8=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_9=( (let ps=% iInvForAll. assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_10=( (let ps=% iInvForAll. assign  (( Para ''Dir-InvSet'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const false )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const true )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const true )  ( Const false )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_11=( assign  (( Para ''InvMsg.Cmd'' Home),  ( Const INV_None ) ) ) in
let S_12=( (let ps=% iInvForAll. assign  (( Para ''InvMsg.Cmd'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const INV_None )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const INV_Inv )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const INV_Inv )  ( Const INV_None )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_13=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_14=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10,S_11,S_12,S_13,S_14]

) in 
guard g S"

definition NI_Local_GetX_PutX10::" nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX10 N iInv1 iInv2\<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInv2) )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )   (Const iInv1))  )    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_5=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_6=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_8=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_9=( (let ps=% iInvForAll. assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_10=( (let ps=% iInvForAll. assign  (( Para ''Dir-InvSet'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const false )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const true )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const true )  ( Const false )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_11=( assign  (( Para ''InvMsg.Cmd'' Home),  ( Const INV_None ) ) ) in
let S_12=( (let ps=% iInvForAll. assign  (( Para ''InvMsg.Cmd'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const INV_None )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const INV_Inv )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const INV_Inv )  ( Const INV_None )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_13=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_14=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10,S_11,S_12,S_13,S_14]

) in 
guard g S"

definition NI_Local_GetX_PutX10_home::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX10_home N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''Dir-ShrSet'' Home) )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )   (Const iInv1))  )    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_5=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_6=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_8=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_9=( (let ps=% iInvForAll. assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_10=( (let ps=% iInvForAll. assign  (( Para ''Dir-InvSet'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const false )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const true )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const true )  ( Const false )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_11=( assign  (( Para ''InvMsg.Cmd'' Home),  ( Const INV_None ) ) ) in
let S_12=( (let ps=% iInvForAll. assign  (( Para ''InvMsg.Cmd'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const INV_None )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )   ( Const INV_Inv )  (iteForm ( andForm ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))  )   ( Const INV_Inv )  ( Const INV_None )))) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_13=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_14=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10,S_11,S_12,S_13,S_14]

) in 
guard g S"

definition NI_Local_GetX_PutX11::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_GetX_PutX11 N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_E ))  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_2=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_3=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_5=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_6=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
let S_8=( (let ps=% iInvForAll. assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_9=( (let ps=% iInvForAll. assign  (( Para ''Dir-InvSet'' iInvForAll),  ( Const false ) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_10=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_11=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
let S_12=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9,S_10,S_11,S_12]

) in 
guard g S"

definition NI_Local_Get_Get::" nat \<Rightarrow> rule" where [simp]:
" NI_Local_Get_Get  iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_Get ))    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )     (neg ( eqn ( IVar ( Para ''RpMsg.Cmd'' iInv1) )  ( Const RP_Replace )) )  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_2=( assign  (( Para ''UniMsg.proc'' iInv1),  ( IVar ( Global ''Dir.HeadPtr'') ) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition NI_Local_Get_Nak1::" nat \<Rightarrow> rule" where [simp]:
" NI_Local_Get_Nak1  iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_Get ))    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const true ))  )     (neg ( eqn ( IVar ( Para ''RpMsg.Cmd'' iInv1) )  ( Const RP_Replace )) )  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Nak ) ) ) in
let S_2=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition NI_Local_Get_Nak2::" nat \<Rightarrow> rule" where [simp]:
" NI_Local_Get_Nak2  iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_Get ))     (neg ( eqn ( IVar ( Para ''CacheState'' iInv1) )  ( Const CACHE_E )) )  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))  )     (neg ( eqn ( IVar ( Para ''RpMsg.Cmd'' iInv1) )  ( Const RP_Replace )) )  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Nak ) ) ) in
let S_2=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition NI_Local_Get_Nak3::" nat \<Rightarrow> rule" where [simp]:
" NI_Local_Get_Nak3  iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_Get ))    ( eqn ( IVar ( Global ''Dir.HeadPtr'') )   (Const iInv1))  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))  )     (neg ( eqn ( IVar ( Para ''RpMsg.Cmd'' iInv1) )  ( Const RP_Replace )) )  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Nak ) ) ) in
let S_2=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition NI_Local_Get_Put1::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Local_Get_Put1 N iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_Get ))    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )     (neg ( eqn ( IVar ( Para ''RpMsg.Cmd'' iInv1) )  ( Const RP_Replace )) )  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.ShrVld''),  ( Const true ) ) ) in
let S_2=( assign  (( Para ''Dir-ShrSet'' iInv1),  ( Const true ) ) ) in
let S_3=( assign  (( Para ''Dir-InvSet'' Home),  ( IVar ( Para ''Dir-ShrSet'' Home) ) ) ) in
let S_4=( (let ps=% iInvForAll. assign  (( Para ''Dir-InvSet'' iInvForAll),  (iteForm ( eqn (Const    iInvForAll )   (Const iInv1))   ( Const true )  ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )) ) in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_5=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Put ) ) ) in
let S_6=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6]

) in 
guard g S"

definition NI_Local_Get_Put2::" nat \<Rightarrow> rule" where [simp]:
" NI_Local_Get_Put2  iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_Get ))    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )     (neg ( eqn ( IVar ( Para ''RpMsg.Cmd'' iInv1) )  ( Const RP_Replace )) )  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_3=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Put ) ) ) in
let S_4=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2,S_3,S_4]

) in 
guard g S"

definition NI_Local_Get_Put3::" nat \<Rightarrow> rule" where [simp]:
" NI_Local_Get_Put3  iInv1 \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_Get ))    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_E ))  )    ( eqn ( IVar ( Global ''Dir.local'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )     (neg ( eqn ( IVar ( Para ''RpMsg.Cmd'' iInv1) )  ( Const RP_Replace )) )  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )  (Const    Home))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.Dirty''),  ( Const false ) ) ) in
let S_2=( assign  (( Global ''Dir.HeadVld''),  ( Const true ) ) ) in
let S_3=( assign  (( Global ''Dir.HeadPtr''),   (Const iInv1) ) ) in
let S_4=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_S ) ) ) in
let S_5=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Put ) ) ) in
let S_6=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6]

) in 
guard g S"

definition NI_Local_Put::"   rule" where [simp]:
" NI_Local_Put    \<equiv>   
let g=( eqn ( IVar ( Para ''UniMsg.Cmd'' Home) )  ( Const UNI_Put ))  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' Home),  ( Const UNI_None ) ) ) in
let S_2=( assign  (( Global ''Dir.Pending''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.Dirty''),  ( Const false ) ) ) in
let S_4=( assign  (( Global ''Dir.local''),  ( Const true ) ) ) in
let S_5=( assign  (( Para ''procCmd'' Home),  ( Const NODE_None ) ) ) in
let S_6=( assign  (( Para ''InvMarked'' Home),  (iteForm ( eqn ( IVar ( Para ''InvMarked'' Home) )  ( Const true ))   ( Const false )  ( IVar ( Para ''InvMarked'' Home) )) ) ) in
let S_7=( assign  (( Para ''CacheState'' Home),  (iteForm ( eqn ( IVar ( Para ''InvMarked'' Home) )  ( Const true ))   ( Const CACHE_I )  ( Const CACHE_S )) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7]

) in 
guard g S"

definition NI_Local_PutXAcksDone::"   rule" where [simp]:
" NI_Local_PutXAcksDone    \<equiv>   
let g=( eqn ( IVar ( Para ''UniMsg.Cmd'' Home) )  ( Const UNI_PutX ))  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' Home),  ( Const UNI_None ) ) ) in
let S_2=( assign  (( Global ''Dir.Pending''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.local''),  ( Const true ) ) ) in
let S_4=( assign  (( Global ''Dir.HeadVld''),  ( Const false ) ) ) in
let S_5=( assign  (( Para ''procCmd'' Home),  ( Const NODE_None ) ) ) in
let S_6=( assign  (( Para ''InvMarked'' Home),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_E ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7]

) in 
guard g S"

definition NI_Nak::" nat \<Rightarrow> rule" where [simp]:
" NI_Nak  iInv1 \<equiv>   
let g=( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_None ))  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_None ) ) ) in
let S_2=( assign  (( Para ''procCmd'' iInv1),  ( Const NODE_None ) ) ) in
let S_3=( assign  (( Para ''InvMarked'' iInv1),  ( Const false ) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_Nak_Clear::"   rule" where [simp]:
" NI_Nak_Clear    \<equiv>   
let g=( eqn ( IVar ( Global ''NakcMsg.Cmd'') )  ( Const NAKC_Nakc ))  in 
let S=(
let S_1=( assign  (( Global ''NakcMsg.Cmd''),  ( Const NAKC_None ) ) ) in
let S_2=( assign  (( Global ''Dir.Pending''),  ( Const false ) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition NI_Nak_Home::"   rule" where [simp]:
" NI_Nak_Home    \<equiv>   
let g=( eqn ( IVar ( Para ''UniMsg.Cmd'' Home) )  ( Const UNI_None ))  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' Home),  ( Const UNI_None ) ) ) in
let S_2=( assign  (( Para ''procCmd'' Home),  ( Const NODE_None ) ) ) in
let S_3=( assign  (( Para ''InvMarked'' Home),  ( Const false ) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_Remote_GetX_Nak::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Remote_GetX_Nak  iInv1  iInv2 \<equiv>   
let g=( andForm ( andForm ( andForm  (neg ( eqn  (Const iInv1)   (Const iInv2)) )     (neg ( eqn ( IVar ( Para ''CacheState'' iInv2) )  ( Const CACHE_E )) )  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )   (Const iInv2))  )    ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))  )  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Nak ) ) ) in
let S_2=( assign  (( Para ''UniMsg.proc'' iInv1),   (Const iInv2) ) ) in
let S_3=( assign  (( Global ''NakcMsg.Cmd''),  ( Const NAKC_Nakc ) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_Remote_GetX_Nak_Home::" nat \<Rightarrow> rule" where [simp]:
" NI_Remote_GetX_Nak_Home  iInv1 \<equiv>   
let g=( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' Home) )  ( Const UNI_GetX ))     (neg ( eqn ( IVar ( Para ''CacheState'' iInv1) )  ( Const CACHE_E )) )  )    ( eqn ( IVar ( Para ''UniMsg.proc'' Home) )   (Const iInv1))  )  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' Home),  ( Const UNI_Nak ) ) ) in
let S_2=( assign  (( Para ''UniMsg.proc'' Home),   (Const iInv1) ) ) in
let S_3=( assign  (( Global ''NakcMsg.Cmd''),  ( Const NAKC_Nakc ) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_Remote_GetX_PutX::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Remote_GetX_PutX  iInv1  iInv2 \<equiv>   
let g=( andForm ( andForm ( andForm  (neg ( eqn  (Const iInv1)   (Const iInv2)) )    ( eqn ( IVar ( Para ''CacheState'' iInv2) )  ( Const CACHE_E ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )   (Const iInv2))  )    ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_GetX ))  )  in 
let S=(
let S_1=( assign  (( Para ''CacheState'' iInv2),  ( Const CACHE_I ) ) ) in
let S_2=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_PutX ) ) ) in
let S_3=( assign  (( Para ''UniMsg.proc'' iInv1),   (Const iInv2) ) ) in
let S_4=( assign  (( Global ''ShWbMsg.Cmd''),  ( Const SHWB_FAck ) ) ) in
let S_5=( assign  (( Global ''ShWbMsg.proc''),   (Const iInv1) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5]

) in 
guard g S"

definition NI_Remote_GetX_PutX_Home::" nat \<Rightarrow> rule" where [simp]:
" NI_Remote_GetX_PutX_Home  iInv1 \<equiv>   
let g=( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' Home) )  ( Const UNI_GetX ))    ( eqn ( IVar ( Para ''CacheState'' iInv1) )  ( Const CACHE_E ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' Home) )   (Const iInv1))  )  in 
let S=(
let S_1=( assign  (( Para ''CacheState'' iInv1),  ( Const CACHE_I ) ) ) in
let S_2=( assign  (( Para ''UniMsg.Cmd'' Home),  ( Const UNI_PutX ) ) ) in
let S_3=( assign  (( Para ''UniMsg.proc'' Home),   (Const iInv1) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_Remote_Get_Nak1::" nat \<Rightarrow> rule" where [simp]:
" NI_Remote_Get_Nak1  iInv1 \<equiv>   
let g=( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' Home) )  ( Const UNI_Get ))     (neg ( eqn ( IVar ( Para ''CacheState'' iInv1) )  ( Const CACHE_E )) )  )    ( eqn ( IVar ( Para ''UniMsg.proc'' Home) )   (Const iInv1))  )  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' Home),  ( Const UNI_Nak ) ) ) in
let S_2=( assign  (( Para ''UniMsg.proc'' Home),   (Const iInv1) ) ) in
let S_3=( assign  (( Global ''NakcMsg.Cmd''),  ( Const NAKC_Nakc ) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_Remote_Get_Nak2::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Remote_Get_Nak2  iInv1  iInv2 \<equiv>   
let g=( andForm ( andForm ( andForm  (neg ( eqn  (Const iInv1)   (Const iInv2)) )     (neg ( eqn ( IVar ( Para ''CacheState'' iInv2) )  ( Const CACHE_E )) )  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )   (Const iInv2))  )    ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_Get ))  )  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Nak ) ) ) in
let S_2=( assign  (( Para ''UniMsg.proc'' iInv1),   (Const iInv2) ) ) in
let S_3=( assign  (( Global ''NakcMsg.Cmd''),  ( Const NAKC_Nakc ) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_Remote_Get_Put1::" nat \<Rightarrow> rule" where [simp]:
" NI_Remote_Get_Put1  iInv1 \<equiv>   
let g=( andForm ( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' Home) )  ( Const UNI_Get ))    ( eqn ( IVar ( Para ''CacheState'' iInv1) )  ( Const CACHE_E ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' Home) )   (Const iInv1))  )  in 
let S=(
let S_1=( assign  (( Para ''CacheState'' iInv1),  ( Const CACHE_S ) ) ) in
let S_2=( assign  (( Para ''UniMsg.Cmd'' Home),  ( Const UNI_Put ) ) ) in
let S_3=( assign  (( Para ''UniMsg.proc'' Home),   (Const iInv1) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_Remote_Get_Put2::" nat \<Rightarrow> nat \<Rightarrow> rule" where [simp]:
" NI_Remote_Get_Put2  iInv1  iInv2 \<equiv>   
let g=( andForm ( andForm ( andForm  (neg ( eqn  (Const iInv1)   (Const iInv2)) )    ( eqn ( IVar ( Para ''CacheState'' iInv2) )  ( Const CACHE_E ))  )    ( eqn ( IVar ( Para ''UniMsg.proc'' iInv1) )   (Const iInv2))  )    ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_Get ))  )  in 
let S=(
let S_1=( assign  (( Para ''CacheState'' iInv2),  ( Const CACHE_S ) ) ) in
let S_2=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Put ) ) ) in
let S_3=( assign  (( Para ''UniMsg.proc'' iInv1),   (Const iInv2) ) ) in
let S_4=( assign  (( Global ''ShWbMsg.Cmd''),  ( Const SHWB_ShWb ) ) ) in
let S_5=( assign  (( Global ''ShWbMsg.proc''),   (Const iInv1) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5]

) in 
guard g S"

definition NI_Remote_Put::" nat \<Rightarrow> rule" where [simp]:
" NI_Remote_Put  iInv1 \<equiv>   
let g=( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_Put ))  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_None ) ) ) in
let S_2=( assign  (( Para ''procCmd'' iInv1),  ( Const NODE_None ) ) ) in
let S_3=( assign  (( Para ''InvMarked'' iInv1),  (iteForm ( eqn ( IVar ( Para ''InvMarked'' iInv1) )  ( Const true ))   ( Const false )  ( IVar ( Para ''InvMarked'' iInv1) )) ) ) in
let S_4=( assign  (( Para ''CacheState'' iInv1),  (iteForm ( eqn ( IVar ( Para ''InvMarked'' iInv1) )  ( Const true ))   ( Const CACHE_I )  ( Const CACHE_S )) ) ) in
parallelList [S_1,S_2,S_3,S_4]

) in 
guard g S"

definition NI_Remote_PutX::" nat \<Rightarrow> rule" where [simp]:
" NI_Remote_PutX  iInv1 \<equiv>   
let g=( andForm ( eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) )  ( Const UNI_PutX ))    ( eqn ( IVar ( Para ''procCmd'' iInv1) )  ( Const NODE_GetX ))  )  in 
let S=(
let S_1=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_None ) ) ) in
let S_2=( assign  (( Para ''procCmd'' iInv1),  ( Const NODE_None ) ) ) in
let S_3=( assign  (( Para ''InvMarked'' iInv1),  ( Const false ) ) ) in
let S_4=( assign  (( Para ''CacheState'' iInv1),  ( Const CACHE_E ) ) ) in
parallelList [S_1,S_2,S_3,S_4]

) in 
guard g S"

definition NI_Replace::" nat \<Rightarrow> rule" where [simp]:
" NI_Replace  iInv1 \<equiv>   
let g=( andForm ( eqn ( IVar ( Para ''RpMsg.Cmd'' iInv1) )  ( Const RP_Replace ))    ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const false ))  )  in 
let S=(
assign  (( Para ''RpMsg.Cmd'' iInv1),  ( Const RP_None ) )) in 
guard g S"

definition NI_ReplaceHome::"   rule" where [simp]:
" NI_ReplaceHome    \<equiv>   
let g=( andForm ( eqn ( IVar ( Para ''RpMsg.Cmd'' Home) )  ( Const RP_Replace ))    ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const false ))  )  in 
let S=(
assign  (( Para ''RpMsg.Cmd'' Home),  ( Const RP_None ) )) in 
guard g S"

definition NI_ReplaceHomeShrVld::"   rule" where [simp]:
" NI_ReplaceHomeShrVld    \<equiv>   
let g=( andForm ( eqn ( IVar ( Para ''RpMsg.Cmd'' Home) )  ( Const RP_Replace ))    ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))  )  in 
let S=(
let S_1=( assign  (( Para ''RpMsg.Cmd'' Home),  ( Const RP_None ) ) ) in
let S_2=( assign  (( Para ''Dir-ShrSet'' Home),  ( Const false ) ) ) in
let S_3=( assign  (( Para ''Dir-InvSet'' Home),  ( Const false ) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_ReplaceShrVld::" nat \<Rightarrow> rule" where [simp]:
" NI_ReplaceShrVld  iInv1 \<equiv>   
let g=( andForm ( eqn ( IVar ( Para ''RpMsg.Cmd'' iInv1) )  ( Const RP_Replace ))    ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))  )  in 
let S=(
let S_1=( assign  (( Para ''RpMsg.Cmd'' iInv1),  ( Const RP_None ) ) ) in
let S_2=( assign  (( Para ''Dir-ShrSet'' iInv1),  ( Const false ) ) ) in
let S_3=( assign  (( Para ''Dir-InvSet'' iInv1),  ( Const false ) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition NI_ShWb::" nat \<Rightarrow>   rule" where [simp]:
" NI_ShWb N  \<equiv>   
let g=( eqn ( IVar ( Global ''ShWbMsg.Cmd'') )  ( Const SHWB_ShWb ))  in 
let S=(
let S_1=( assign  (( Global ''ShWbMsg.Cmd''),  ( Const SHWB_None ) ) ) in
let S_2=( assign  (( Global ''Dir.Pending''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.Dirty''),  ( Const false ) ) ) in
let S_4=( assign  (( Global ''Dir.ShrVld''),  ( Const true ) ) ) in
let S_5=( (let ps=% iInvForAll. let S_1=( assign  (( Para ''Dir-ShrSet'' iInvForAll),  (iteForm ( eqn ( IVar ( Global ''ShWbMsg.proc'') )  (Const    iInvForAll ))   ( Const true )  (iteForm ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))   ( Const true )  ( Const false ))) ) ) in
let S_2=( assign  (( Para ''Dir-InvSet'' iInvForAll),  (iteForm ( eqn ( IVar ( Global ''ShWbMsg.proc'') )  (Const    iInvForAll ))   ( Const true )  (iteForm ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))   ( Const true )  ( Const false ))) ) ) in
parallelList [S_1,S_2]

 in
let natList=down N in
(forallSent natList  ps) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5]

) in 
guard g S"

definition NI_Wb::"   rule" where [simp]:
" NI_Wb    \<equiv>   
let g=( eqn ( IVar ( Global ''WbMsg.Cmd'') )  ( Const WB_Wb ))  in 
let S=(
let S_1=( assign  (( Global ''WbMsg.Cmd''),  ( Const WB_None ) ) ) in
let S_2=( assign  (( Global ''Dir.Dirty''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.HeadVld''),  ( Const false ) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition PI_Local_GetX_GetX1::"   rule" where [simp]:
" PI_Local_GetX_GetX1    \<equiv>   
let g=( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_None ))    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_I ))  )  in 
let S=(
let S_1=( assign  (( Para ''procCmd'' Home),  ( Const NODE_GetX ) ) ) in
let S_2=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_3=( assign  (( Para ''UniMsg.Cmd'' Home),  ( Const UNI_GetX ) ) ) in
let S_4=( assign  (( Para ''UniMsg.proc'' Home),  ( IVar ( Global ''Dir.HeadPtr'') ) ) ) in
parallelList [S_1,S_2,S_3,S_4]

) in 
guard g S"

definition PI_Local_GetX_GetX2::"   rule" where [simp]:
" PI_Local_GetX_GetX2    \<equiv>   
let g=( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_None ))    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_S ))  )  in 
let S=(
let S_1=( assign  (( Para ''procCmd'' Home),  ( Const NODE_GetX ) ) ) in
let S_2=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_3=( assign  (( Para ''UniMsg.Cmd'' Home),  ( Const UNI_GetX ) ) ) in
let S_4=( assign  (( Para ''UniMsg.proc'' Home),  ( IVar ( Global ''Dir.HeadPtr'') ) ) ) in
parallelList [S_1,S_2,S_3,S_4]

) in 
guard g S"

definition PI_Local_GetX_PutX1::" nat \<Rightarrow>   rule" where [simp]:
" PI_Local_GetX_PutX1 N  \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_None ))    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_I ))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.local''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_3=( (let ps=% iInvForAll. let S_1=( assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) ) in
let S_2=( assign  (( Para ''Dir-InvSet'' iInvForAll),  (iteForm ( orForm ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))    ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )  )   ( Const true )  ( Const false )) ) ) in
let S_3=( assign  (( Para ''InvMsg.Cmd'' iInvForAll),  (iteForm ( orForm ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))    ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )  )   ( Const INV_Inv )  ( Const INV_None )) ) ) in
parallelList [S_1,S_2,S_3]

 in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_4=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_5=( assign  (( Global ''Dir.HeadVld''),  ( Const false ) ) ) in
let S_6=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''procCmd'' Home),  ( Const NODE_None ) ) ) in
let S_8=( assign  (( Para ''InvMarked'' Home),  ( Const false ) ) ) in
let S_9=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_E ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9]

) in 
guard g S"

definition PI_Local_GetX_PutX2::" nat \<Rightarrow>   rule" where [simp]:
" PI_Local_GetX_PutX2 N  \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_None ))    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_S ))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.local''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_3=( (let ps=% iInvForAll. let S_1=( assign  (( Para ''Dir-ShrSet'' iInvForAll),  ( Const false ) ) ) in
let S_2=( assign  (( Para ''Dir-InvSet'' iInvForAll),  (iteForm ( orForm ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))    ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )  )   ( Const true )  ( Const false )) ) ) in
let S_3=( assign  (( Para ''InvMsg.Cmd'' iInvForAll),  (iteForm ( orForm ( eqn ( IVar ( Global ''Dir.HeadPtr'') )  (Const    iInvForAll ))    ( andForm ( eqn ( IVar ( Global ''Dir.ShrVld'') )  ( Const true ))    ( eqn ( IVar ( Para ''Dir-ShrSet'' iInvForAll) )  ( Const true ))  )  )   ( Const INV_Inv )  ( Const INV_None )) ) ) in
parallelList [S_1,S_2,S_3]

 in
let natList=down N in
(forallSent natList  ps) ) ) in
let S_4=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_5=( assign  (( Global ''Dir.HeadVld''),  ( Const false ) ) ) in
let S_6=( assign  (( Global ''Dir.ShrVld''),  ( Const false ) ) ) in
let S_7=( assign  (( Para ''InvMarked'' Home),  ( Const false ) ) ) in
let S_8=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_E ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8]

) in 
guard g S"

definition PI_Local_GetX_PutX3::"   rule" where [simp]:
" PI_Local_GetX_PutX3    \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_None ))    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_I ))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.local''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_3=( assign  (( Para ''procCmd'' Home),  ( Const NODE_None ) ) ) in
let S_4=( assign  (( Para ''InvMarked'' Home),  ( Const false ) ) ) in
let S_5=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_E ) ) ) in
let S_6=( assign  (( Para ''CacheData'' Home),  ( IVar ( Global ''MemData'') ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5,S_6]

) in 
guard g S"

definition PI_Local_GetX_PutX4::"   rule" where [simp]:
" PI_Local_GetX_PutX4    \<equiv>   
let g=( andForm ( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_None ))    ( eqn ( IVar ( Global ''Dir.HeadVld'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_S ))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.local''),  ( Const true ) ) ) in
let S_2=( assign  (( Global ''Dir.Dirty''),  ( Const true ) ) ) in
let S_3=( assign  (( Para ''procCmd'' Home),  ( Const NODE_None ) ) ) in
let S_4=( assign  (( Para ''InvMarked'' Home),  ( Const false ) ) ) in
let S_5=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_E ) ) ) in
parallelList [S_1,S_2,S_3,S_4,S_5]

) in 
guard g S"

definition PI_Local_Get_Get::"   rule" where [simp]:
" PI_Local_Get_Get    \<equiv>   
let g=( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_None ))    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const true ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_I ))  )  in 
let S=(
let S_1=( assign  (( Para ''procCmd'' Home),  ( Const NODE_Get ) ) ) in
let S_2=( assign  (( Global ''Dir.Pending''),  ( Const true ) ) ) in
let S_3=( assign  (( Para ''UniMsg.Cmd'' Home),  ( Const UNI_Get ) ) ) in
let S_4=( assign  (( Para ''UniMsg.proc'' Home),  ( IVar ( Global ''Dir.HeadPtr'') ) ) ) in
parallelList [S_1,S_2,S_3,S_4]

) in 
guard g S"

definition PI_Local_Get_Put::"   rule" where [simp]:
" PI_Local_Get_Put    \<equiv>   
let g=( andForm ( andForm ( andForm ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_None ))    ( eqn ( IVar ( Global ''Dir.Dirty'') )  ( Const false ))  )    ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const false ))  )    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_I ))  )  in 
let S=(
let S_1=( assign  (( Para ''procCmd'' Home),  ( Const NODE_None ) ) ) in
let S_2=( assign  (( Global ''Dir.local''),  ( Const true ) ) ) in
let S_3=( assign  (( Para ''InvMarked'' Home),  (iteForm ( eqn ( IVar ( Para ''InvMarked'' Home) )  ( Const true ))   ( Const false )  ( IVar ( Para ''InvMarked'' Home) )) ) ) in
let S_4=( assign  (( Para ''CacheState'' Home),  (iteForm ( eqn ( IVar ( Para ''InvMarked'' Home) )  ( Const true ))   ( Const CACHE_I )  ( Const CACHE_S )) ) ) in
parallelList [S_1,S_2,S_3,S_4]

) in 
guard g S"

definition PI_Local_PutX::"   rule" where [simp]:
" PI_Local_PutX    \<equiv>   
let g=( andForm ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_None ))    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_E ))  )  in 
let S=(
let S_1=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
let S_2=( assign  (( Global ''Dir.Dirty''),  ( Const false ) ) ) in
let S_3=( assign  (( Global ''Dir.local''),  (iteForm ( eqn ( IVar ( Global ''Dir.Pending'') )  ( Const true ))   ( IVar ( Global ''Dir.local'') )  ( Const false )) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition PI_Local_Replace::"   rule" where [simp]:
" PI_Local_Replace    \<equiv>   
let g=( andForm ( eqn ( IVar ( Para ''procCmd'' Home) )  ( Const NODE_None ))    ( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_S ))  )  in 
let S=(
let S_1=( assign  (( Global ''Dir.local''),  ( Const false ) ) ) in
let S_2=( assign  (( Para ''CacheState'' Home),  ( Const CACHE_I ) ) ) in
parallelList [S_1,S_2]

) in 
guard g S"

definition PI_Remote_Get::" nat \<Rightarrow> rule" where [simp]:
" PI_Remote_Get  iInv1 \<equiv>   
let g=( andForm ( eqn ( IVar ( Para ''procCmd'' iInv1) )  ( Const NODE_None ))    ( eqn ( IVar ( Para ''CacheState'' iInv1) )  ( Const CACHE_I ))  )  in 
let S=(
let S_1=( assign  (( Para ''procCmd'' iInv1),  ( Const NODE_Get ) ) ) in
let S_2=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_Get ) ) ) in
let S_3=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition PI_Remote_GetX::" nat \<Rightarrow> rule" where [simp]:
" PI_Remote_GetX  iInv1 \<equiv>   
let g=( andForm ( eqn ( IVar ( Para ''procCmd'' iInv1) )  ( Const NODE_None ))    ( eqn ( IVar ( Para ''CacheState'' iInv1) )  ( Const CACHE_I ))  )  in 
let S=(
let S_1=( assign  (( Para ''procCmd'' iInv1),  ( Const NODE_GetX ) ) ) in
let S_2=( assign  (( Para ''UniMsg.Cmd'' iInv1),  ( Const UNI_GetX ) ) ) in
let S_3=( assign  (( Para ''UniMsg.proc'' iInv1),  (Const    Home) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition PI_Remote_PutX::" nat \<Rightarrow> rule" where [simp]:
" PI_Remote_PutX  iInv1 \<equiv>   
let g=( andForm ( eqn ( IVar ( Para ''procCmd'' iInv1) )  ( Const NODE_None ))    ( eqn ( IVar ( Para ''CacheState'' iInv1) )  ( Const CACHE_E ))  )  in 
let S=(
let S_1=( assign  (( Para ''CacheState'' iInv1),  ( Const CACHE_I ) ) ) in
let S_2=( assign  (( Global ''WbMsg.Cmd''),  ( Const WB_Wb ) ) ) in
let S_3=( assign  (( Global ''WbMsg.proc''),   (Const iInv1) ) ) in
parallelList [S_1,S_2,S_3]

) in 
guard g S"

definition PI_Remote_Replace::" nat \<Rightarrow> rule" where [simp]:
" PI_Remote_Replace  iInv1 \<equiv>   
let g=( andForm ( eqn ( IVar ( Para ''procCmd'' iInv1) )  ( Const NODE_None ))    ( eqn ( IVar ( Para ''CacheState'' iInv1) )  ( Const CACHE_S ))  )  in 
let S=(
( assign  (( Para ''RpMsg.Cmd'' iInv1),  ( Const RP_Replace ) ) )) in 
guard g S"

definition Store::" nat \<Rightarrow> rule" where [simp]:
" Store  iInv1 \<equiv>   
let g=( eqn ( IVar ( Para ''CacheState'' iInv1) )  ( Const CACHE_E ))  in 
let S=(
assign  (( Para ''CacheData'' iInv1),  ( Const CACHE_E ) )) in 
guard g S"

definition StoreHome::"   rule" where [simp]:
" StoreHome    \<equiv>   
let g=( eqn ( IVar ( Para ''CacheState'' Home) )  ( Const CACHE_E ))  in 
let S=(
assign  (( Para ''CacheData'' Home),  ( Const CACHE_E ) )) in 
guard g S"

definition rules::"nat \<Rightarrow> rule set"  where [simp] :
"rules N\<equiv> {r. ( r=NI_FAck )\<or>
exLessP N (%i.  r=NI_Inv i )  \<or>
exLessP N (%i j.  r=NI_InvAck_1 i j)  \<or>
exLessP N (%i.  r=NI_InvAck_1_Home i )  \<or>
exLessP N (%i.  r=NI_InvAck_2 N i)  \<or>
exLessP N (%i.  r=NI_Local_GetX_GetX i )  \<or>
exLessP N (%i.  r=NI_Local_GetX_Nak1 i )  \<or>
exLessP N (%i.  r=NI_Local_GetX_Nak2 i )  \<or>
exLessP N (%i.  r=NI_Local_GetX_Nak3 i )  \<or>
exLessP N (%i.  r=NI_Local_GetX_PutX1 N i)  \<or>
exLessP N (%i.  r=NI_Local_GetX_PutX2 N i)  \<or>
exLessP N (%i.  r=NI_Local_GetX_PutX3 N i)  \<or>
exLessP N (%i.  r=NI_Local_GetX_PutX4 N i)  \<or>
exLessP N (%i.  r=NI_Local_GetX_PutX5 N i)  \<or>
exLessP N (%i.  r=NI_Local_GetX_PutX6 N i)  \<or>
exLessP N (%i.  r=NI_Local_GetX_PutX7 N i)  \<or>
exTwoLessP N (%i j.  r=NI_Local_GetX_PutX8  i j)  \<or>
exLessP N (%i.  r=NI_Local_GetX_PutX8_home N i)  \<or>
exLessP N (%i.  r=NI_Local_GetX_PutX9 N i)  \<or>
exTwoLessP N (%i j.  r=NI_Local_GetX_PutX10  i j)  \<or>
exLessP N (%i.  r=NI_Local_GetX_PutX10_home N i)  \<or>
exLessP N (%i.  r=NI_Local_GetX_PutX11 N i)  \<or>
exLessP N (%i.  r=NI_Local_Get_Get i )  \<or>
exLessP N (%i.  r=NI_Local_Get_Nak1 i )  \<or>
exLessP N (%i.  r=NI_Local_Get_Nak2 i )  \<or>
exLessP N (%i.  r=NI_Local_Get_Nak3 i )  \<or>
exLessP N (%i.  r=NI_Local_Get_Put1 N i)  \<or>
exLessP N (%i.  r=NI_Local_Get_Put2 i )  \<or>
exLessP N (%i.  r=NI_Local_Get_Put3 i )  \<or>
( r=NI_Local_Put )\<or>
( r=NI_Local_PutXAcksDone )\<or>
exLessP N (%i.  r=NI_Nak i )  \<or>
( r=NI_Nak_Clear )\<or>
( r=NI_Nak_Home )\<or>
exLessP N (%i j.  r=NI_Remote_GetX_Nak i j)  \<or>
exLessP N (%i.  r=NI_Remote_GetX_Nak_Home i )  \<or>
exLessP N (%i j.  r=NI_Remote_GetX_PutX i j)  \<or>
exLessP N (%i.  r=NI_Remote_GetX_PutX_Home i )  \<or>
exLessP N (%i.  r=NI_Remote_Get_Nak1 i )  \<or>
exLessP N (%i j.  r=NI_Remote_Get_Nak2 i j)  \<or>
exLessP N (%i.  r=NI_Remote_Get_Put1 i )  \<or>
exLessP N (%i j.  r=NI_Remote_Get_Put2 i j)  \<or>
exLessP N (%i.  r=NI_Remote_Put i )  \<or>
exLessP N (%i.  r=NI_Remote_PutX i )  \<or>
exLessP N (%i.  r=NI_Replace i )  \<or>
( r=NI_ReplaceHome )\<or>
( r=NI_ReplaceHomeShrVld )\<or>
exLessP N (%i.  r=NI_ReplaceShrVld i )  \<or>
( r=NI_ShWb N)\<or>
( r=NI_Wb )\<or>
( r=PI_Local_GetX_GetX1 )\<or>
( r=PI_Local_GetX_GetX2 )\<or>
( r=PI_Local_GetX_PutX1 N)\<or>
( r=PI_Local_GetX_PutX2 N)\<or>
( r=PI_Local_GetX_PutX3 )\<or>
( r=PI_Local_GetX_PutX4 )\<or>
( r=PI_Local_Get_Get )\<or>
( r=PI_Local_Get_Put )\<or>
( r=PI_Local_PutX )\<or>
( r=PI_Local_Replace )\<or>
exLessP N (%i.  r=PI_Remote_Get i )  \<or>
exLessP N (%i.  r=PI_Remote_GetX i )  \<or>
exLessP N (%i.  r=PI_Remote_PutX i )  \<or>
exLessP N (%i.  r=PI_Remote_Replace i )  \<or>
exLessP N (%i.  r=Store i )  \<or>
( r=StoreHome )}"

definition invariants::"nat \<Rightarrow> formula set"  where [simp] :
"invariants N\<equiv> {f.  }"

 definition iniStateSpecOfShWbMsg.Cmd::"   formula" where [simp] :

  " iniStateSpecOfShWbMsg.Cmd \<equiv>  eqn ( ( IVar ( Global ''ShWbMsg.Cmd'') )) ( Const SHWB_None )"


 definition iniStateSpecOfInvMarked::" nat \<Rightarrow>  formula" where [simp] :

  " iniStateSpecOfInvMarked N\<equiv>  forallForm  (down N)  (%iInv1. eqn ( IVar ( Para ''InvMarked'' iInv1) ) ( Const false ))"


 definition iniStateSpecOfInvMarked::"   formula" where [simp] :

  " iniStateSpecOfInvMarked \<equiv>  eqn ( ( IVar ( Para ''InvMarked'' Home) )) ( Const false )"


 definition iniStateSpecOfDir-InvSet::" nat \<Rightarrow>  formula" where [simp] :

  " iniStateSpecOfDir-InvSet N\<equiv>  forallForm  (down N)  (%iInv1. eqn ( IVar ( Para ''Dir-InvSet'' iInv1) ) ( Const CACHE_I ))"


 definition iniStateSpecOfDir-InvSet::"   formula" where [simp] :

  " iniStateSpecOfDir-InvSet \<equiv>  eqn ( ( IVar ( Para ''Dir-InvSet'' Home) )) ( Const false )"


 definition iniStateSpecOfDir.ShrVld::"   formula" where [simp] :

  " iniStateSpecOfDir.ShrVld \<equiv>  eqn ( ( IVar ( Global ''Dir.ShrVld'') )) ( Const false )"


 definition iniStateSpecOfWbMsg.Cmd::"   formula" where [simp] :

  " iniStateSpecOfWbMsg.Cmd \<equiv>  eqn ( ( IVar ( Global ''WbMsg.Cmd'') )) ( Const WB_None )"


 definition iniStateSpecOfDir.HeadPtr::"   formula" where [simp] :

  " iniStateSpecOfDir.HeadPtr \<equiv>  eqn ( ( IVar ( Global ''Dir.HeadPtr'') )) (Const    Home)"


 definition iniStateSpecOfRpMsg.Cmd::" nat \<Rightarrow>  formula" where [simp] :

  " iniStateSpecOfRpMsg.Cmd N\<equiv>  forallForm  (down N)  (%iInv1. eqn ( IVar ( Para ''RpMsg.Cmd'' iInv1) ) ( Const RP_None ))"


 definition iniStateSpecOfRpMsg.Cmd::"   formula" where [simp] :

  " iniStateSpecOfRpMsg.Cmd \<equiv>  eqn ( ( IVar ( Para ''RpMsg.Cmd'' Home) )) ( Const RP_None )"


 definition iniStateSpecOfDir.HeadVld::"   formula" where [simp] :

  " iniStateSpecOfDir.HeadVld \<equiv>  eqn ( ( IVar ( Global ''Dir.HeadVld'') )) ( Const false )"


 definition iniStateSpecOfDir.Dirty::"   formula" where [simp] :

  " iniStateSpecOfDir.Dirty \<equiv>  eqn ( ( IVar ( Global ''Dir.Dirty'') )) ( Const false )"


 definition iniStateSpecOfDir-ShrSet::" nat \<Rightarrow>  formula" where [simp] :

  " iniStateSpecOfDir-ShrSet N\<equiv>  forallForm  (down N)  (%iInv1. eqn ( IVar ( Para ''Dir-ShrSet'' iInv1) ) ( Const false ))"


 definition iniStateSpecOfDir-ShrSet::"   formula" where [simp] :

  " iniStateSpecOfDir-ShrSet \<equiv>  eqn ( ( IVar ( Para ''Dir-ShrSet'' Home) )) ( Const false )"


 definition iniStateSpecOfNakcMsg.Cmd::"   formula" where [simp] :

  " iniStateSpecOfNakcMsg.Cmd \<equiv>  eqn ( ( IVar ( Global ''NakcMsg.Cmd'') )) ( Const NAKC_None )"


 definition iniStateSpecOfCacheState::" nat \<Rightarrow>  formula" where [simp] :

  " iniStateSpecOfCacheState N\<equiv>  forallForm  (down N)  (%iInv1. eqn ( IVar ( Para ''CacheState'' iInv1) ) ( Const CACHE_I ))"


 definition iniStateSpecOfCacheState::"   formula" where [simp] :

  " iniStateSpecOfCacheState \<equiv>  eqn ( ( IVar ( Para ''CacheState'' Home) )) ( Const CACHE_I )"


 definition iniStateSpecOfDir.local::"   formula" where [simp] :

  " iniStateSpecOfDir.local \<equiv>  eqn ( ( IVar ( Global ''Dir.local'') )) ( Const false )"


 definition iniStateSpecOfDir.Pending::"   formula" where [simp] :

  " iniStateSpecOfDir.Pending \<equiv>  eqn ( ( IVar ( Global ''Dir.Pending'') )) ( Const false )"


 definition iniStateSpecOfInvMsg.Cmd::" nat \<Rightarrow>  formula" where [simp] :

  " iniStateSpecOfInvMsg.Cmd N\<equiv>  forallForm  (down N)  (%iInv1. eqn ( IVar ( Para ''InvMsg.Cmd'' iInv1) ) ( Const INV_None ))"


 definition iniStateSpecOfInvMsg.Cmd::"   formula" where [simp] :

  " iniStateSpecOfInvMsg.Cmd \<equiv>  eqn ( ( IVar ( Para ''InvMsg.Cmd'' Home) )) ( Const INV_None )"


 definition iniStateSpecOfShWbMsg.proc::"   formula" where [simp] :

  " iniStateSpecOfShWbMsg.proc \<equiv>  eqn ( ( IVar ( Global ''ShWbMsg.proc'') )) (Const    Home)"


 definition iniStateSpecOfUniMsg.Cmd::" nat \<Rightarrow>  formula" where [simp] :

  " iniStateSpecOfUniMsg.Cmd N\<equiv>  forallForm  (down N)  (%iInv1. eqn ( IVar ( Para ''UniMsg.Cmd'' iInv1) ) ( Const UNI_None ))"


 definition iniStateSpecOfUniMsg.Cmd::"   formula" where [simp] :

  " iniStateSpecOfUniMsg.Cmd \<equiv>  eqn ( ( IVar ( Para ''UniMsg.Cmd'' Home) )) ( Const UNI_None )"


 definition iniStateSpecOfprocCmd::" nat \<Rightarrow>  formula" where [simp] :

  " iniStateSpecOfprocCmd N\<equiv>  forallForm  (down N)  (%iInv1. eqn ( IVar ( Para ''procCmd'' iInv1) ) ( Const NODE_None ))"


 definition iniStateSpecOfprocCmd::"   formula" where [simp] :

  " iniStateSpecOfprocCmd \<equiv>  eqn ( ( IVar ( Para ''procCmd'' Home) )) ( Const NODE_None )"


definition allIniSpecs ::"nat\<Rightarrow>formula list" where [simp]:

   "allIniSpecs  N \<equiv>[ (  iniStateSpecOfInvMarked N )
, (  iniStateSpecOfInvMarked )
, (  iniStateSpecOfDir-InvSet N )
, (  iniStateSpecOfDir-InvSet )
, (  iniStateSpecOfDir.ShrVld )
, (  iniStateSpecOfWbMsg.Cmd )
, (  iniStateSpecOfDir.HeadPtr )
, (  iniStateSpecOfRpMsg.Cmd N )
, (  iniStateSpecOfRpMsg.Cmd )
, (  iniStateSpecOfDir.HeadVld )
, (  iniStateSpecOfDir.Dirty )
, (  iniStateSpecOfDir-ShrSet N )
, (  iniStateSpecOfDir-ShrSet )
, (  iniStateSpecOfNakcMsg.Cmd )
, (  iniStateSpecOfCacheState N )
, (  iniStateSpecOfCacheState )
, (  iniStateSpecOfDir.local )
, (  iniStateSpecOfDir.Pending )
, (  iniStateSpecOfInvMsg.Cmd N )
, (  iniStateSpecOfInvMsg.Cmd )
, (  iniStateSpecOfShWbMsg.proc )
, (  iniStateSpecOfUniMsg.Cmd N )
, (  iniStateSpecOfUniMsg.Cmd )
, (  iniStateSpecOfprocCmd N )
, (  iniStateSpecOfprocCmd )
, (  iniStateSpecOfShWbMsg.Cmd )
]"

