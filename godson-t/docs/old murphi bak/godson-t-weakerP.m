/* Godson-T Cache protocol for 2 memory address and 2 caches in each node */
/* The caches will take initiative in replacement while needed */
/* Locks could be nested, e.i., many locks will be acquired at the same time, however one node can only hold one lock */
/* By Kaiqiang Duan, Nov. 18, 2014 */

const
    NUM_NODE: 3;
    NUM_CACHE: 2;
    NUM_ADDR: 2;
    NUM_DATA: 2;
    NUM_LOCK: 2;

type
    TYPE_NODE: 1 .. NUM_NODE;
    TYPE_CACHE: 1 .. NUM_CACHE;
    TYPE_ADDR: 1 .. NUM_ADDR;
    TYPE_DATA: 1 .. NUM_DATA;
    TYPE_LOCK: 1 .. NUM_LOCK;
    
    CACHE_STATE: enum{INVALID, DIRTY, VALID};
    
    CACHE: record
        state: CACHE_STATE;
        addr: TYPE_ADDR;
        data: TYPE_DATA;
    end;
    
    MEMORY: record
        data: TYPE_DATA;
    end;
    
    LOCK: record
        owner: TYPE_NODE;
        beUsed: boolean;
        criticalRegion: TYPE_ADDR;
        inCriticalRegion: boolean;
    end;
    
    NODE: record
        cache: array [TYPE_CACHE] of CACHE;
        hasLock: boolean;
        firstRead: array [TYPE_ADDR] of boolean;
    end;
    
    /* assistant types */
    
    /* 
     * These are stages of that caches take initiative in replacement
     * NON: do not need Replacement
     * REQUIRE: require to replace
     * RANDOM: if there does not exist a INVALID cache, then choose a random one
     * RANDINV: if there exists at least a INVALID cache, then choose a random INVALID one
     * DESIGNATED: the cache to be replaced has been designated
     * TOREP: after DESIGNATED, if the designated cache is DIRTY, then replace it
     * DONE: the replacement has been done
     */
    REPLACE_STAGE: enum{NON, REQUIRE, RANDOM, RANDINV, DESIGNATED, TOREP, DONE};
    
    REPLACE_RULE: enum{NONE, NLNCR, NLNCW, LNCFR, LCFR, LNCNFR};
    
var
    /* modeling variables */
    memory: array [TYPE_ADDR] of MEMORY;
    lock: array [TYPE_LOCK] of LOCK;
    node: array [TYPE_NODE] of NODE;
    /* assistant variables */
    curNode: TYPE_NODE;
    curCache: TYPE_CACHE;
    curMemory: TYPE_ADDR;
    curData: TYPE_DATA;
    curLock: TYPE_LOCK;
    replace: REPLACE_STAGE;
    repRule: REPLACE_RULE;
    
ruleset d: TYPE_DATA do
    startstate "Init"
        for i: TYPE_NODE do
            for j: TYPE_CACHE do
                node[i].cache[j].state := INVALID;
            endfor;
            node[i].hasLock := false;
            for a: TYPE_ADDR do
                node[i].firstRead[a] := true;
            endfor;
            curNode := i;
        endfor;
        for j: TYPE_CACHE do
            curCache := j;
        endfor;
        for a: TYPE_ADDR do
            memory[a].data := d;
            curMemory := a;
        endfor;
        curData := d;
        for l: TYPE_LOCK do
            lock[l].beUsed := false;
            lock[l].inCriticalRegion := false;
            curLock := l;
        endfor;
        replace := NON;
        repRule := NONE;
    end;
end;

/********************************** Replace ***************************************/

/* Replacement in INVALID caches (In fact, it need none replacement) */
ruleset i: TYPE_NODE do
    rule "RI"
        replace = REQUIRE &
        i = curNode &
        exists j: TYPE_CACHE do
            node[i].cache[j].state = INVALID
        endexists
    ==>
    begin
        replace := RANDINV;
    end;
end;

/* Choose a Random INVALID Cache */
ruleset i: TYPE_NODE; j: TYPE_CACHE do
    rule "CRIC"
        replace = RANDINV &
        i = curNode &
        node[i].cache[j].state = INVALID
    ==>
    begin
        curCache := j;
        /* need none replacement */
        replace := DONE;
    end;
end;

/* Replacement in None INVALID caches */
ruleset i: TYPE_NODE do
    rule "RNI"
        replace = REQUIRE &
        i = curNode &
        forall j: TYPE_CACHE do
            node[i].cache[j].state != INVALID
        endforall
    ==>
    begin
        replace := RANDOM;
    end;
end;

/* Choose a Random Cache in all caches */
ruleset i: TYPE_CACHE do
    rule "CRC"
        replace = RANDOM
    ==>
    begin
        curCache := i;
        replace := DESIGNATED;
    end;
end;

/* the Designated Cache is Not DIRTY */
ruleset i: TYPE_NODE; j: TYPE_CACHE do
    rule "DCND"
        replace = DESIGNATED &
        i = curNode &
        j = curCache &
        node[i].cache[j].state != DIRTY
    ==>
    begin
        replace := DONE;
    end;
end;

/* the Designated Cache is DIRTY */
ruleset i: TYPE_NODE; j: TYPE_CACHE do
    rule "DCD"
        replace = DESIGNATED &
        i = curNode &
        j = curCache &
        node[i].cache[j].state = DIRTY
    ==>
    begin
        replace := TOREP;
    end;
end;

/* replace */
ruleset i: TYPE_NODE; j: TYPE_CACHE; m: TYPE_ADDR do
    rule "Replace"
        replace = TOREP &
        i = curNode &
        j = curCache &
        m = node[i].cache[j].addr
    ==>
    begin
        memory[m].data := node[i].cache[j].data;
        node[i].cache[j].state := INVALID;
        replace := DONE;
    end;
end;

/********************************** No Lock Read ***********************************/

/* No Lock in Cache Read */
/* Omitted because of non-correlation to other nodes */

/* No Lock Not in Cache Read, REQUIRE replacement */
ruleset i: TYPE_NODE; a: TYPE_ADDR do
    rule "NLNCRR"
        replace = NON &
        !node[i].hasLock &
        forall j: TYPE_CACHE do
            node[i].cache[j].state = INVALID |
            node[i].cache[j].addr != a
        endforall
    ==>
    begin
        curNode := i;
        curMemory := a;
        replace := REQUIRE;
        repRule := NLNCR;
    end;
end;

/* No Lock Not in Cache Read, DONE replacement */
ruleset i: TYPE_NODE; j: TYPE_CACHE; a: TYPE_ADDR do
    rule "NLNCRD"
        replace = DONE &
        repRule = NLNCR &
        i = curNode &
        j = curCache &
        a = curMemory
    ==>
    begin
        node[i].cache[j].addr := a;
        node[i].cache[j].data := memory[a].data;
        node[i].cache[j].state := VALID;
        replace := NON;
        repRule := NONE;
    end;
end;


/********************************** No Lock Write **********************************/

/* No Lock in Cache Write */
ruleset i: TYPE_NODE; j: TYPE_CACHE; a: TYPE_ADDR; d: TYPE_DATA do
    rule "NLCW"
        replace = NON &
        !node[i].hasLock &
        node[i].cache[j].state != INVALID &
        node[i].cache[j].addr = a
    ==>
    begin
        node[i].cache[j].data := d;
        node[i].cache[j].state := DIRTY;
    end;
end;

/* No Lock Not in Cache Write, REQUIRE replacement */
ruleset i: TYPE_NODE; a: TYPE_ADDR; d: TYPE_DATA do
    rule "NLNCWR"
        replace = NON &
        !node[i].hasLock &
        forall j: TYPE_CACHE do
            node[i].cache[j].state = INVALID |
            node[i].cache[j].addr != a
        endforall
    ==>
    begin
        curNode := i;
        curMemory := a;
        curData := d;
        replace := REQUIRE;
        repRule := NLNCW;
    end;
end;

/* No Lock Not in Cache Write, DONE replacement */
ruleset i: TYPE_NODE; j: TYPE_CACHE; a: TYPE_ADDR; d: TYPE_DATA do
    rule "NLNCWD"
        replace = DONE &
        repRule = NLNCW &
        i = curNode &
        j = curCache &
        a = curMemory &
        d = curData
    ==>
    begin
        node[i].cache[j].addr := a;
        node[i].cache[j].data := d;
        node[i].cache[j].state := DIRTY;
        replace := NON;
        repRule := NONE;
    end;
end;

/************************************* Locked Read *********************************/

/* Locked in Cache First Read, REQUIRE replacement */
ruleset i: TYPE_NODE; j: TYPE_CACHE; a: TYPE_ADDR; l: TYPE_LOCK do
    rule "LCFRR"
        replace = NON &
        node[i].hasLock &
        lock[l].beUsed &
        lock[l].owner = i &
        node[i].firstRead[a] &
        node[i].cache[j].state != INVALID &
        node[i].cache[j].addr = a
    ==>
    begin
        curNode := i;
        curCache := j;
        curMemory := a;
        curLock := l;
        /* curCache has already been DESIGNATED to a */
        replace := DESIGNATED;
        repRule := LCFR;
    end;
end;

/* Locked in Cache First Read, DONE replacement */
ruleset i: TYPE_NODE; j: TYPE_CACHE; a: TYPE_ADDR; l: TYPE_LOCK do
    rule "LCFRD"
        replace = DONE &
        repRule = LCFR &
        i = curNode &
        j = curCache &
        a = curMemory &
        l = curLock
    ==>
    begin
        node[i].cache[j].data := memory[a].data;
        node[i].cache[j].state := VALID;
        node[i].firstRead[a] := false;
        lock[l].criticalRegion := a;
        lock[l].inCriticalRegion := true;
        replace := NON;
        repRule := NONE;
    end;
end;

/* Locked Not in Cache First Read, REQUIRE replacement */
ruleset i: TYPE_NODE; a: TYPE_ADDR; l: TYPE_LOCK do
    rule "LNCFRR"
        replace = NON &
        node[i].hasLock &
        lock[l].beUsed &
        lock[l].owner = i &
        node[i].firstRead[a] &
        forall j: TYPE_CACHE do
            node[i].cache[j].state = INVALID |
            node[i].cache[j].addr != a
        endforall
    ==>
    begin
        curNode := i;
        curMemory := a;
        curLock := l;
        replace := REQUIRE;
        repRule := LNCFR;
    end;
end;

/* Locked Not in Cache First Read, DONE replacement */
ruleset i: TYPE_NODE; j: TYPE_CACHE; a: TYPE_ADDR; l: TYPE_LOCK do
    rule "LNCFRD"
        replace = DONE &
        repRule = LNCFR &
        i = curNode &
        j = curCache &
        a = curMemory &
        l = curLock
    ==>
    begin
        node[i].cache[j].addr := a;
        node[i].cache[j].data := memory[a].data;
        node[i].cache[j].state := VALID;
        node[i].firstRead[a] := false;
        lock[l].criticalRegion := a;
        lock[l].inCriticalRegion := true;
        replace := NON;
        repRule := NONE;
    end;
end;

/* Locked in Cache Not First Read */
/* Omitted because of non-correlation to other nodes */

/* Locked Not in Cache Not First Read, REQUIRE replacement */
ruleset i: TYPE_NODE; a: TYPE_ADDR; l: TYPE_LOCK do
    rule "LNCNFRR"
        replace = NON &
        node[i].hasLock &
        lock[l].beUsed &
        lock[l].owner = i &
        !node[i].firstRead[a] &
        forall j: TYPE_CACHE do
            node[i].cache[j].state = INVALID |
            node[i].cache[j].addr != a
        endforall
    ==>
    begin
        curNode := i;
        curMemory := a;
        curLock := l;
        replace := REQUIRE;
        repRule := LNCNFR;
    end;
end;

/* Locked Not in Cache Not First Read, DONE replacement */
ruleset i: TYPE_NODE; j: TYPE_CACHE; a: TYPE_ADDR; l: TYPE_LOCK do
    rule "LNCNFRD"
        replace = DONE &
        repRule = LNCNFR &
        i = curNode &
        j = curCache &
        a = curMemory &
        l = curLock
    ==>
    begin
        node[i].cache[j].addr := a;
        node[i].cache[j].data := memory[a].data;
        node[i].cache[j].state := VALID;
        lock[l].criticalRegion := a;
        lock[l].inCriticalRegion := true;
        replace := NON;
        repRule := NONE;
    end;
end;

/************************************* Locked Write ********************************/

/* Locked in Cache Write */
ruleset i: TYPE_NODE; j: TYPE_CACHE; a: TYPE_ADDR; d: TYPE_DATA; l: TYPE_LOCK do
    rule "LCW"
        replace = NON &
        node[i].hasLock &
        lock[l].beUsed &
        lock[l].owner = i &
        node[i].cache[j].state != INVALID &
        node[i].cache[j].addr = a
    ==>
    begin
        memory[a].data := d;
        node[i].cache[j].data := d;
        node[i].cache[j].state := VALID;
        lock[l].criticalRegion := a;
        lock[l].inCriticalRegion := true;
    end;
end;

/* Locked Not in Cache Write */
ruleset i: TYPE_NODE; a: TYPE_ADDR; d: TYPE_DATA; l: TYPE_LOCK do
    rule "LNCW"
        replace = NON &
        node[i].hasLock &
        lock[l].beUsed &
        lock[l].owner = i &
        forall j: TYPE_CACHE do
            node[i].cache[j].state = INVALID |
            node[i].cache[j].addr != a
        endforall
    ==>
    begin
        memory[a].data := d;
        lock[l].criticalRegion := a;
        lock[l].inCriticalRegion := true;
    end;
end;

/************************************* Lock Manager ********************************/

ruleset i: TYPE_NODE; l: TYPE_LOCK do
    /* Acquire */
    rule "Acquire"
        replace = NON &
        !node[i].hasLock &
        !lock[l].beUsed
    ==>
    begin
        lock[l].beUsed := true;
        lock[l].owner := i;
        node[i].hasLock := true;
        for j: TYPE_ADDR do
            node[i].firstRead[j] := true;
        endfor;
    end;
    
    /* Release */
    rule "Release"
        replace = NON &
        node[i].hasLock &
        lock[l].beUsed &
        lock[l].owner = i
    ==>
        lock[l].beUsed := false;
        node[i].hasLock := false;
        lock[l].inCriticalRegion := false;
    end;
end;

/************************************* Properties **********************************/

/* Deadlock-Free: One Node One Lock restrict */
ruleset i: TYPE_NODE do
    invariant "DeadlockFree"
    (
        replace = NON &
        node[i].hasLock
    ) -> (
        exists l: TYPE_LOCK do
            lock[l].beUsed &
            lock[l].owner = i
        endexists &
        forall m: TYPE_LOCK; n: TYPE_LOCK do
            m = n |
            !lock[m].beUsed |
            !lock[n].beUsed |
            lock[m].owner != i |
            lock[n].owner != i
        endforall
    )
end;

/* Coherence: Cached Data Equals Memory */
ruleset i: TYPE_NODE; j: TYPE_CACHE; a: TYPE_ADDR do
    invariant "Coherence"
    (
        replace = NON &
        node[i].hasLock &
        !node[i].firstRead[a] &
        node[i].cache[j].state = VALID &
        node[i].cache[j].addr = a &
        forall m: TYPE_LOCK; n: TYPE_LOCK do
            m != n -> (
                lock[m].inCriticalRegion &
                lock[n].inCriticalRegion &
                lock[m].criticalRegion != lock[n].criticalRegion
            )
        endforall
    ) -> 
    node[i].cache[j].data = memory[a].data
end;

/************************************* Debug ***************************************/


