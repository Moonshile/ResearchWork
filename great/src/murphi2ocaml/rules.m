
ruleset src : NODE do
rule "NI_InvAck"
  src != Home &
  Sta.InvMsg[src].Cmd = INV_InvAck &
  Sta.Dir.Pending & Sta.Dir.InvSet[src]  &exists p : NODE do p != src & Sta.Dir.InvSet[p] end
==>
begin
  if (exists p : NODE do p != src & Sta.Dir.InvSet[p] end) then
    Sta.LastInvAck := src;
    for p : NODE do
      if (p != src & Sta.Dir.InvSet[p]) then
        Sta.LastOtherInvAck := p;
      end;
    end;
  elsif (Sta.InvMsg[src].Cmd = INV_InvAck) then
    Sta.Collecting := false;
    Sta.LastInvAck := src;
  else
    Sta.Dir.Pending := false;
    if (Sta.Dir.Local & !Sta.Dir.Dirty) then
      Sta.Dir.Local := false;
    end;
  end;
endrule;
endruleset;
