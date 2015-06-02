const clientNUMS : 3;
type location: enum{ M, O,  E, S, I};

     client: scalarset(clientNUMS);

var state : array [client] of location;

    x : boolean; 
    

ruleset i : client do
rule "t1" state[i] = E ==> begin
      state[i] := M; endrule; 
   

rule "t2"
      state[i] = I ==>begin
      for j: client do
      if (j=i) then state[j]:=S;
      elsif (state[j]=E) then state[j]:=S;
      elsif (state[j]=M)  then state[j]:=S;
      else   state[j]:=state[j];
      endif;
      endfor; endrule;
         

rule "t3"
      state[i] = S ==>begin
      for j: client do
      if (j=i) then state[j]:=E ;
      else   state[j]:=I;
      endif;
      endfor; endrule;
      

rule "t4"
      state[i] =O ==>begin
      for j: client do
      if (j=i) then state[j]:=E ;
      else   state[j]:=I;
      endif;
      endfor; endrule;
      
  
rule "t5"
      state[i] =I ==>begin
      for j: client do
      if (j=i) then state[j]:=E ;
      else   state[j]:=I;
      endif;
      endfor; endrule;
endruleset;

startstate
begin
 for i: client do
    state[i] := I; 
  endfor; 
endstartstate;

ruleset i:client; j: client do
invariant "coherence"
  state[i] = M & state[j] = M;
endruleset;





