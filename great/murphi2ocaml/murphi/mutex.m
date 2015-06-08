const num_clients : 4;

type client: scalarset(num_clients);

var turn: client;
    want: array[client] of boolean;
    crit: array[client] of boolean;
  
  
ruleset c1: client do
rule "rule_req"
    want[c1] = false ==>
begin
    want[c1] := true
endrule; endruleset;

ruleset c1: client do
rule "rule_enter"
    want[c1] = true & crit[c1] = false & turn = c1 ==>
begin
    crit[c1] := true
endrule; endruleset;

ruleset c1: client; c2: client do
rule "rule_exit"
    crit[c1] = true ==>
begin
    turn := c2;
    crit[c1] := false;
    want[c1] := false;
endrule; endruleset;


startstate
begin
    for i: client do
        want[i] := false;
        crit[i] := false;
    endfor;
    turn:=1;
endstartstate;

ruleset i:client; j: client do
invariant "coherence"
  crit[i] = true & crit[j] = true;
endruleset;
