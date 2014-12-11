/* Godson-T Cache protocol by zhouyan */

/* constants */
const
	NUM_NODE : 3;
	NUM_LOCK : 2;
	NUM_CACHE : 2;
	NUM_DATA : 2;
	NUM_ADDR : 3;

/* types */
type
/*
	TYPE_NODE : scalarset(NUM_NODE);
	TYPE_DATA : scalarset(NUM_DATA);
	TYPE_LOCK : scalarset(NUM_LOCK);
	TYPE_CACHE : scalarset(NUM_CACHE);
	TYPE_ADDR : scalarset(NUM_ADDR);
	*/
	
	TYPE_NODE : 1 .. NUM_NODE;
	TYPE_DATA : 1 .. NUM_DATA;
	TYPE_LOCK : 1 .. NUM_LOCK;
	TYPE_CACHE : 1 .. NUM_CACHE;
	TYPE_ADDR : 1 .. NUM_ADDR;

	FLAG_LOCK : boolean;
	FLAG_FIRSTREAD : boolean;

	CACHE_STATE : enum{INVALID, DIRTY, VALID};
	CACHE : record
		state : CACHE_STATE;
		addr : TYPE_ADDR;
		data : TYPE_DATA;
	end;
	
	MEMORY : record 
		addr : TYPE_ADDR;
		data : TYPE_DATA;
	end;

	LOCK : record
		id : TYPE_LOCK;
		owner : TYPE_NODE;
		beUsed : FLAG_LOCK;
	end;
	
	
	NODE : record
		cache : array [TYPE_CACHE] of CACHE;
		hasLock : FLAG_LOCK;
		firstRead : array [TYPE_ADDR] of FLAG_FIRSTREAD;
	end;

var	
	memory : array [TYPE_ADDR] of MEMORY;
	/* Yi Lv */
	lasting : array [TYPE_ADDR] of boolean;

	lock : array [TYPE_LOCK] of LOCK;
	node : array [TYPE_NODE] of NODE;
	random : TYPE_CACHE;

function getLoc(i:TYPE_NODE) : TYPE_CACHE;
begin
	for j:TYPE_CACHE do
		if node[i].cache[j].state = INVALID then
			return j;
		endif;
	endfor;
	return random;
end;

procedure replace(i:TYPE_NODE; j:TYPE_CACHE);
begin
	if node[i].cache[j].state = DIRTY then
		node[i].cache[j].state := INVALID;
		memory[node[i].cache[j].addr].data := node[i].cache[j].data;
		/* Yi Lv */
		lasting[node[i].cache[j].addr] := false;
	endif;
end;

ruleset d:TYPE_DATA do
	startstate "Init"
		for i:TYPE_NODE do
			for j:TYPE_CACHE do
				node[i].cache[j].state := INVALID;
			endfor;
			node[i].hasLock := false;
			for a: TYPE_ADDR do
				node[i].firstRead[a] := true;
			endfor;
		endfor;
		for k:TYPE_LOCK do
			lock[k].id := k;
			lock[k].beUsed := false;
		endfor;
		for m:TYPE_ADDR do
			memory[m].addr := m;
			memory[m].data := d;
			/* Yi Lv */
			lasting[m] := false;
		endfor;
		for j:TYPE_CACHE do
			random := j;
		endfor;
	end;
end;

/* Read *****************************************************/
ruleset i:TYPE_NODE; a:TYPE_ADDR do
	rule "Read no lock, not in cache"
		!node[i].hasLock
		& forall j:TYPE_CACHE do 
			node[i].cache[j].state = INVALID | node[i].cache[j].addr != memory[a].addr
		endforall
		==>
		var tmp:TYPE_CACHE;
		begin
			tmp := getLoc(i);
			replace(i,tmp);
			node[i].cache[tmp].state := VALID;
			node[i].cache[tmp].addr := memory[a].addr;
			node[i].cache[tmp].data := memory[a].data;
			/* Yi Lv */
			lasting[a] := false;
	end;
end;

ruleset i:TYPE_NODE; a:TYPE_ADDR; l:TYPE_LOCK do
	rule "Read right lock, first read"
		node[i].hasLock & lock[l].beUsed = true & lock[l].owner = i
		& node[i].firstRead[a]
		==>
		var tmp : TYPE_CACHE;
		var flag : boolean;
		begin
			flag := false;
			for j:TYPE_CACHE do
				if !flag then
					if node[i].cache[j].state != INVALID & node[i].cache[j].addr = memory[a].addr then
						replace(i,j);
						node[i].cache[j].state := VALID;
						node[i].cache[j].addr := memory[a].addr;
						node[i].cache[j].data := memory[a].data;
						node[i].firstRead[a] := false;
						flag := true;
					endif;
				endif;
			endfor;
			if !flag then
				tmp := getLoc(i);
				replace(i,tmp);
				node[i].cache[tmp].state := VALID;
				node[i].cache[tmp].addr := memory[a].addr;
				node[i].cache[tmp].data := memory[a].data;
				node[i].firstRead[a] := false;
			endif;
	end;
	
	rule "Raed right lock, not first read, not in cache"
		node[i].hasLock & lock[l].beUsed = true & lock[l].owner = i
		& !node[i].firstRead[a]
		& forall k:TYPE_CACHE do
			node[i].cache[k].state = INVALID | node[i].cache[k].addr != memory[a].addr
		endforall
		==>
		var tmp : TYPE_CACHE;
		begin
		tmp := getLoc(i);
		replace(i,tmp);
		node[i].cache[tmp].state := VALID;
		node[i].cache[tmp].addr := memory[a].addr;
		node[i].cache[tmp].data := memory[a].data;
	end;
end;
/***********************************************************/

/* Write *****************************************************/
ruleset i:TYPE_NODE; a:TYPE_ADDR; d:TYPE_DATA do
	rule "Write no lock(write back), not in cache"
		!node[i].hasLock
		& forall j:TYPE_CACHE do 
			node[i].cache[j].state = INVALID | node[i].cache[j].addr != memory[a].addr
		endforall
		==>
		var tmp : TYPE_CACHE;
		begin
		tmp := getLoc(i);
		replace(i,tmp);
		node[i].cache[tmp].state := DIRTY;
		node[i].cache[tmp].addr := memory[a].addr;
		node[i].cache[tmp].data := d;
		/* Yi Lv */
		lasting[a] := false;
	end;
	
	rule "Write no lock(write back), in cache"
		!node[i].hasLock
		& exists j:TYPE_CACHE do 
			node[i].cache[j].state != INVALID & node[i].cache[j].addr = memory[a].addr
		endexists
		==>
		var flag : boolean;
		begin
		flag := false;
		/* Yi Lv */
		lasting[a] := false;
		for k:TYPE_CACHE do
			if !flag then
				if node[i].cache[k].state != INVALID & node[i].cache[k].addr = memory[a].addr then
					node[i].cache[k].state := DIRTY;
					node[i].cache[k].data := d;
					flag := true;
				endif;
			endif;
		endfor;
	end;
end;

ruleset i:TYPE_NODE; a:TYPE_ADDR; d:TYPE_DATA; l:TYPE_LOCK do
	rule "has right lock(write through), not in cache"
		node[i].hasLock & lock[l].beUsed = true & lock[l].owner = i
		& forall j:TYPE_CACHE do 
			node[i].cache[j].state = INVALID | node[i].cache[j].addr != memory[a].addr
		endforall
		==>
		begin
		memory[a].data := d;
		/* Yi Lv */
		lasting[a] := true;
	end;
	
	rule "has right lock(write through), in cache"
		node[i].hasLock & lock[l].beUsed = true & lock[l].owner = i
		& exists j:TYPE_CACHE do 
			node[i].cache[j].state != INVALID & node[i].cache[j].addr = memory[a].addr
		endexists
		==>
		var flag : boolean;
		begin
		memory[a].data := d;
		flag := false;
		/* Yi Lv */
		lasting[a] := true;
		for k:TYPE_CACHE do
			if !flag then
				if node[i].cache[k].state != INVALID & node[i].cache[k].addr = memory[a].addr then
					node[i].cache[k].state := VALID;
					node[i].cache[k].data := d;
					flag := true;
				endif;
			endif;
		endfor;
	end;
end;
/***********************************************************/

/* Acquire & Release *****************************************/
ruleset i:TYPE_NODE; l:TYPE_LOCK do
	rule "Acquire"
		!node[i].hasLock & 
		forall j : TYPE_LOCK do lock[j].beUsed = false end
		==>
		begin
		lock[l].beUsed := true;
		lock[l].owner := i;
		node[i].hasLock := true;
		for j:TYPE_ADDR do
			node[i].firstRead[j] := true;
		endfor;
		/* Yi Lv */
		/*
		for j:TYPE_CACHE do
			if node[i].cache[j].state = DIRTY then
				memory[node[i].cache[j].addr].data := node[i].cache[j].data;
				node[i].cache[j].state := VALID;
				lasting[node[i].cache[j].addr] := false;
			endif;
			if node[i].cache[j].state = VALID then
				node[i].cache[j].state := INVALID;
			endif;
		endfor;
		*/
	end;
	
	rule "Release"
		node[i].hasLock & lock[l].beUsed & lock[l].owner = i
		==>
		begin
			lock[l].beUsed := false;
			node[i].hasLock := false;
			/*
			for j:TYPE_CACHE do
				if node[i].cache[j].state = DIRTY then
					memory[node[i].cache[j].addr].data := node[i].cache[j].data;
					node[i].cache[j].state := VALID;
					lasting[node[i].cache[j].addr] := false;
				endif;
				if node[i].cache[j].state = VALID then
					node[i].cache[j].state := INVALID;
				endif;
			endfor;
			*/
	end;

end;
/*************************************************************/

ruleset i:TYPE_CACHE do
	rule "set random"
		true 
		==>
		random := i;
	end;
end;


/* model checking properties*************************/
ruleset i:TYPE_NODE do
	invariant "one node one lock restrict"
	node[i].hasLock -> ((exists j:TYPE_LOCK do lock[j].beUsed & lock[j].owner = i endexists)
										 & !(exists m:TYPE_LOCK; n:TYPE_LOCK do m != n & lock[m].beUsed & lock[n].beUsed & lock[m].owner = i & lock[n].owner = i endexists))
end;

ruleset i:TYPE_NODE; j:TYPE_CACHE; a:TYPE_ADDR  do
	invariant "cache data equals memory"
	(node[i].hasLock = true & node[i].firstRead[a] = false & node[i].cache[j].state = VALID & node[i].cache[j].addr = memory[a].addr & lasting[a] = true) ->
		(node[i].cache[j].data = memory[a].data)
end;
