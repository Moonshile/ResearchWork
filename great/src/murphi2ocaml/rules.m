
ruleset src : NODE do
rule "NI_InvAck"
  src != Home &
  Sta.InvMsg[src].Cmd = INV_InvAck &
  Sta.Dir.Pending & Sta.Dir.InvSet[src]  &exists p : NODE do p != src & Sta.Dir.InvSet[p] end
==>
begin
  Sta.InvMsg[src].Cmd := INV_None;
  Sta.Dir.InvSet[src] := false;
  for p : NODE do
    Sta.LastOtherInvAck := p;
  end;
  if (exists p : NODE do p != src & Sta.Dir.InvSet[p] end) then
    Sta.LastInvAck := src;
    for p : NODE do
      if (p != src & Sta.Dir.InvSet[p]) then
        Sta.LastOtherInvAck := p;
      end;
    end;
  else
    Sta.Dir.Pending := false;
    if (Sta.Dir.Local & !Sta.Dir.Dirty) then
      Sta.Dir.Local := false;
    end;
    Sta.Collecting := false;
    Sta.LastInvAck := src;
  end;
endrule;
endruleset;
