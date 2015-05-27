
ruleset src : NODE do
rule "NI_Local_GetX_PutX"
  exists p : NODE do p != src & Sta.Dir.InvSet[p] end &
  src != Home &
  Sta.UniMsg[src].Cmd = UNI_GetX &
  Sta.UniMsg[src].Proc = Home &
  !Sta.Dir.Pending &
  (Sta.Dir.Dirty -> Sta.Dir.Local & Sta.Proc[Home].CacheState = CACHE_E) & forall p : NODE do p != src -> !Sta.Dir.ShrSet[p] end
==>
begin
  if (Sta.Dir.Dirty) then
    Sta.Dir.Local := false;
    Sta.Dir.Dirty := true;
    Sta.Dir.HeadVld := true;
    Sta.Dir.HeadPtr := src;
    Sta.Dir.ShrVld := false;
    for p : NODE do
      Sta.Dir.ShrSet[p] := false;
      Sta.Dir.InvSet[p] := false;
    end;
    Sta.UniMsg[src].Cmd := UNI_PutX;
    Sta.UniMsg[src].Proc := Home;
    Sta.UniMsg[src].Data := Sta.Proc[Home].CacheData;
    Sta.Proc[Home].CacheState := CACHE_I;
    undefine Sta.Proc[Home].CacheData;
  elsif (Sta.Dir.HeadVld ->
         Sta.Dir.HeadPtr = src  &
         forall p : NODE do p != src -> !Sta.Dir.ShrSet[p] end) then
    Sta.Dir.Local := false;
    Sta.Dir.Dirty := true;
    Sta.Dir.HeadVld := true;
    Sta.Dir.HeadPtr := src;
    Sta.Dir.ShrVld := false;
    for p : NODE do
      Sta.Dir.ShrSet[p] := false;
      Sta.Dir.InvSet[p] := false;
    end;
    Sta.UniMsg[src].Cmd := UNI_PutX;
    Sta.UniMsg[src].Proc := Home;
    Sta.UniMsg[src].Data := Sta.MemData;
    Sta.Proc[Home].CacheState := CACHE_I;
    undefine Sta.Proc[Home].CacheData;
    if (Sta.Dir.Local) then
      Sta.Proc[Home].CacheState := CACHE_I;
      undefine Sta.Proc[Home].CacheData;
      if (Sta.Proc[Home].ProcCmd = NODE_Get) then
        Sta.Proc[Home].InvMarked := true;
      end;
    end;
  else
    Sta.Dir.Pending := true;
    Sta.Dir.Local := false;
    Sta.Dir.Dirty := true;
    Sta.Dir.HeadVld := true;
    Sta.Dir.HeadPtr := src;
    Sta.Dir.ShrVld := false;
    for p : NODE do
      Sta.Dir.ShrSet[p] := false;
      if ( p != Home & p != src &
           ( Sta.Dir.ShrVld & Sta.Dir.ShrSet[p] |
             Sta.Dir.HeadVld & Sta.Dir.HeadPtr = p ) ) then
        Sta.Dir.InvSet[p] := true;
        Sta.InvMsg[p].Cmd := INV_Inv;
      else
        Sta.Dir.InvSet[p] := false;
        Sta.InvMsg[p].Cmd := INV_None;
      end;
    end;
    Sta.UniMsg[src].Cmd := UNI_PutX;
    Sta.UniMsg[src].Proc := Home;
    Sta.UniMsg[src].Data := Sta.MemData;
    if (Sta.Dir.Local) then
      Sta.Proc[Home].CacheState := CACHE_I;
      undefine Sta.Proc[Home].CacheData;
      if (Sta.Proc[Home].ProcCmd = NODE_Get) then
        Sta.Proc[Home].InvMarked := true;
      end;
    end;
    Sta.Requester := src;
    Sta.Collecting := true;
    Sta.PrevData := Sta.CurrData;
    if (Sta.Dir.HeadPtr != src) then
      Sta.LastOtherInvAck := Sta.Dir.HeadPtr;
    else
      for p : NODE do
        if (p != src & Sta.Dir.ShrSet[p]) then Sta.LastOtherInvAck := p end;
      end;
    end;
  end;
endrule;
endruleset;
