const num_clients : 3;




type channel1Type : enum{emptyCh1, req_shared, req_exclusive};

     channel2_4Type:enum { emptyCh2_4,invalidate, grant_shared, grant_exclusive};
     
     channel3Type: enum {emptyCh3,  invalidate_ack};
     
     cache_state : enum{invalid, shared, exclusive};

     client: 1 .. num_clients;

var channel1: array[client] of channel1Type;

    channel2_4: array[client] of channel2_4Type;

    channel3: array[client] of channel3Type;

    home_sharer_list, home_invalidate_list: array[client] of boolean;

    home_exclusive_granted: boolean;

    home_current_command: channel1Type;

    home_current_client: client;

    cache: array[client] of cache_state;

ruleset cl: client do
 rule "client requests shared access"
      cache[cl] = invalid & channel1[cl] = emptyCh1 ==> 
        begin channel1[cl] := req_shared end;

 rule "client requests exclusive access"
      (cache[cl] = invalid | cache[cl] = shared ) & channel1[cl] = emptyCh1 ==> 
        begin channel1[cl] := req_exclusive end;

 rule "home picks new request"
       home_current_command = emptyCh1 & channel1[cl] != emptyCh1 ==>
        begin home_current_command := channel1[cl]; 
              channel1[cl] := emptyCh1;
              home_current_client := cl;
              for i: client do
               home_invalidate_list[i] := home_sharer_list[i]
              endfor;
        end;

 rule "home sends invalidate message"
      (home_current_command = req_shared & home_exclusive_granted
       | home_current_command = req_exclusive)
      & home_invalidate_list[cl] & channel2_4[cl] = emptyCh2_4 ==>
       begin
        channel2_4[cl] := invalidate;
        home_invalidate_list[cl] := false;
       end;

 rule "home receives invalidate acknowledgement"
      home_current_command != emptyCh1 & channel3[cl] = invalidate_ack ==>
      begin 
       home_sharer_list[cl] := false;
       home_exclusive_granted := false;
       channel3[cl] := emptyCh3;
      end;
    
 
 rule "sharer invalidates cache"
      channel2_4[cl] = invalidate & channel3[cl] = emptyCh3 ==>
      begin
       channel2_4[cl] := emptyCh2_4;
       channel3[cl] := invalidate_ack;
       cache[cl] := invalid;
      end;
 rule "client receives shared grant"
      channel2_4[cl] = grant_shared ==>
      begin
       cache[cl] := shared;
       channel2_4[cl] := emptyCh2_4;
      end;

 rule "client receives exclusive grant"
      channel2_4[cl] = grant_exclusive ==>
      begin
       cache[cl] := exclusive;
       channel2_4[cl] := emptyCh2_4;
      end;

end;



 rule "home sends reply to client -- shared"
      home_current_command = req_shared
      & !home_exclusive_granted & channel2_4[home_current_client] = emptyCh2_4 ==>
      begin
       home_sharer_list[home_current_client] := true;
       home_current_command := emptyCh1;
       channel2_4[home_current_client] := grant_shared;
      end;
      
   
      

 rule "home sends reply to client -- exclusive"
      home_current_command = req_exclusive
      & forall i: client do home_sharer_list[i] = false endforall
      & channel2_4[home_current_client] = emptyCh2_4 ==>
      begin
       home_sharer_list[home_current_client] := true;
       home_current_command := emptyCh1;
       home_exclusive_granted := true;
       channel2_4[home_current_client] := grant_exclusive;
      end;



startstate
begin
 for i: client do
   channel1[i] := emptyCh1;
   channel2_4[i] := emptyCh2_4; 
   channel3[i] := emptyCh3;   
   cache[i] := invalid;
   home_sharer_list[i] := false;
   home_invalidate_list[i] := false;
  endfor;
home_current_command := emptyCh1;
home_current_client := 1;
home_exclusive_granted := false;
end;

---ruleset do
 ---invariant "coherent"
 --(cache[0] = exclusive) -> cache[1] = invalid
---e-nd;

---ruleset c1:client; c2: client do
 ---invariant "coherent"
 ---(c1 != c2 & cache[c1] = exclusive) -> cache[c2] = invalid
---end;
