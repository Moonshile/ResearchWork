const num_clients : 3;




type channel1Type : enum{emptyC, req_shared, req_exclusive,invalidate, grant_shared, grant_exclusive,  invalidate_ack};

     --channel2_4Type:enum{emptyC, req_shared, req_exclusive,invalidate, grant_shared, grant_exclusive,  invalidate_ack};
     
     --channel3Type: enum{emptyC, req_shared, req_exclusive,invalidate, grant_shared, grant_exclusive,  invalidate_ack};
     
     cache_state : enum{invalid, shared, exclusive};

     client: 1 .. num_clients;

var channel1: array[client] of channel1Type;

    channel2_4: array[client] of channel1Type;

    channel3: array[client] of channel1Type;

    home_sharer_list, home_invalidate_list: array[client] of boolean;

    home_exclusive_granted: boolean;

    home_current_command: channel1Type;

    home_current_client: client;

    cache: array[client] of cache_state;

ruleset cl: client do
 rule "client requests shared access"
      cache[cl] = invalid & channel1[cl] = emptyC ==> 
        begin channel1[cl] := req_shared end;

 rule "client requests exclusive access"
      (cache[cl] = invalid | cache[cl] = shared ) & channel1[cl] = emptyC ==> 
        begin channel1[cl] := req_exclusive end;

 rule "home picks new request"
       home_current_command = emptyC & channel1[cl] != emptyC ==>
        begin home_current_command := channel1[cl]; 
              home_current_client := cl;
              channel1[cl] := emptyC;
              for i: client do
               home_invalidate_list[i] := home_sharer_list[i]
              endfor;
        end;

 rule "home sends invalidate message"
      (home_current_command = req_shared & home_exclusive_granted
       | home_current_command = req_exclusive)
      & home_invalidate_list[cl] & channel2_4[cl] = emptyC ==>
       begin
        channel2_4[cl] := invalidate;
        home_invalidate_list[cl] := false;
       end;

 rule "home receives invalidate acknowledgement"
      home_current_command != emptyC &
      channel3[cl] = invalidate_ack &
      home_exclusive_granted ==>
      begin 
       channel3[cl] := emptyC;
       home_sharer_list[cl] := false;
       home_exclusive_granted := false;
      end;
      
 rule "home receives invalidate acknowledgement2"
      home_current_command != emptyC &
      channel3[cl] = invalidate_ack &
      !home_exclusive_granted ==>
      begin 
       channel3[cl] := emptyC;
       home_sharer_list[cl] := false;
      end;
 
 rule "sharer invalidates cache"
      channel2_4[cl] = invalidate & channel3[cl] = emptyC ==>
      begin
       channel2_4[cl] := emptyC;
       channel3[cl] := invalidate_ack;
       cache[cl] := invalid;
      end;
 rule "client receives shared grant"
      channel2_4[cl] = grant_shared ==>
      begin
       cache[cl] := shared;
       channel2_4[cl] := emptyC;
      end;

 rule "client receives exclusive grant"
      channel2_4[cl] = grant_exclusive ==>
      begin
       cache[cl] := exclusive;
       channel2_4[cl] := emptyC;
      end;



 rule "home sends reply to client -- shared"
      home_current_command = req_shared& 
      !home_exclusive_granted & 
      channel2_4[cl] = emptyC &
      home_current_client = cl ==>
      begin
       channel2_4[cl] := grant_shared;
       home_current_command := emptyC;
       home_sharer_list[cl] := true;
      end;
      
   
      

 rule "home sends reply to client -- exclusive"
      home_current_command = req_exclusive
      & forall i: client do home_sharer_list[i] = false endforall
      & channel2_4[cl] = emptyC
      & home_current_client = cl      ==>
      begin
       channel2_4[cl] := grant_exclusive;
       home_current_command := emptyC;
       home_sharer_list[cl] := true;
       home_exclusive_granted := true;
      end;


end;


startstate
begin
home_current_command := emptyC;
home_exclusive_granted := false;
 for i: client do
   channel1[i] := emptyC;
   channel2_4[i] := emptyC; 
   channel3[i] := emptyC;   
   cache[i] := invalid;
   home_sharer_list[i] := false;
   home_invalidate_list[i] := false;
  endfor;
 home_current_client := 1;
end;
/*
-- AG !((Node[2].channel3 = invalidate_ack & home_current_client = 2) & home_current_command = req_shared)
invariant "inv1"
    !(channel3[2] = invalidate_ack & home_current_client = 2 & home_current_command = req_shared);

--AG !((home_current_client = 2 & home_current_command = req_shared) & Node[2].channel2_4 = invalidate)
invariant "inv2"
    !((home_current_client = 2 & home_current_command = req_shared) & channel2_4[2] = invalidate);
    
--AG !(((home_current_client = 2 & home_current_command = req_shared) & home_exclusive_granted = TRUE) & Node[2].home_invalidate_list = TRUE)
invariant "inv3"
    !(((home_current_client = 2 & home_current_command = req_shared) & home_exclusive_granted) & home_invalidate_list[2]);
    
--AG !((Node[2].channel1 = req_shared & home_exclusive_granted = TRUE) & Node[2].home_sharer_list = TRUE)
invariant "inv4"
    !((channel1[2] = req_shared & home_exclusive_granted) & home_sharer_list[2]);
    
--AG !(Node[2].home_sharer_list = TRUE & Node[2].channel1 = emptyC)
invariant "inv5"
    !(home_sharer_list[2] = TRUE & channel1[2] = emptyC);
    
--AG !((Node[2].channel1 = req_shared & home_current_command = req_exclusive) & home_current_client = 2)
invariant "inv6"
    !((channel1[2] = req_shared & home_current_command = req_exclusive) & home_current_client = 2);
    
--AG !((Node[2].channel1 = emptyC & home_current_client = 2) & !(home_current_command = emptyC))
invariant "inv7"
    !((channel1[2] = emptyC & home_current_client = 2) & !(home_current_command = emptyC));
*/

---ruleset do
 ---invariant "coherent"
 --(cache[0] = exclusive) -> cache[1] = invalid
---e-nd;

---ruleset c1:client; c2: client do
 ---invariant "coherent"
 ---(c1 != c2 & cache[c1] = exclusive) -> cache[c2] = invalid
---end;
