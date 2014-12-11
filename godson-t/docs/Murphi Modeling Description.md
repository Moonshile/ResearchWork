
- - -

[TOC]

- - -

### About Godson-T
Godson-T is a many-core architecture designed and implemented by Institite of Computing Technology, Chinese Academy of Sciences. And *Godson-T cache coherence protocol* (Godson-T protocol for short) is the cache coherence protocol used in Godson-T system.

### Model in murphi
#### Requirements

+ There exist 2 caches in each node, and 2 memory address. In fact, this is not a big problem because murphi is a parameterized modeling language.
+ The caches will take initiative in replacement while need, e.i., we should define related rules. In fact, this brings easiness for modeling. However, the replacement procedure is still the hardest problem in modeling.
+ Locks could be nested, e.i., many locks will be acquired at the same time, while one node can only hold one lock.

#### Approach to Replacement
Modeling replacement procedure itself is not a hard work, but for further use in our research tools and experiments, indexes or ranks or parameters of arrays are better to be integer constants or simple integer variables rather than members of complex data structures. And this task is not that easy.

So what is the replacement procedure? It is the situation that we need designate a cache in the node. If there exist some INVALID caches, we randomly choose one (in practise, we should obey some principles such as LRU); else if there does not exist any INVALID chache, we still randomly choose one, but if it is DIRTY, then we *Replace* it into the associated memory address and then mark it as INVALID. We implement this procedure with a series of rules, and before these rules, we need store the current state and after these rules, we need return to the stored state. Thus, a rule that needs replacement will be seperated into 2 rules.

I designed two different methods for replacement procedure in modeling Godson-T.

**Replacement Procedure #1**

![godsont-rep-1](https://raw.githubusercontent.com/Moonshile/MoonshileImages/master/Log/godsont-rep-1.png)

The problem in this procedure is that, in LRU, the nodes will choose proper caches without concerning about if its state  is VALID or DIRTY, but this procedure concerns.

**Replacement Procedure #2**

![godsont-rep-2](https://raw.githubusercontent.com/Moonshile/MoonshileImages/master/Log/godsont-rep-2.png)

This procedure is better because it's nearer to the protocol and it has fewer states. The states in it are described following in detail. They are stages of that caches take initiative in replacement.

* NON: do not need Replacement
* REQUIRE: require to replace
* RANDOM: if there does not exist a INVALID cache, then choose a random one
* RANDINV: if there exists at least a INVALID cache, then choose a random INVALID one
* DESIGNATED: the cache to be replaced has been designated
* TOREP: after DESIGNATED, if the designated cache is DIRTY, then replace it
* DONE: the replacement has been done

### Model Rules in Godson-T
As we mentioned, all rules that need replace cached data to memory should be separated into two parts in our murphi model, which is inspired by interrupts in systems programming or function calls in programming languages. The two stages are listed as follows:

+ Firstly, save current state to the *assistant variables* such as `curNode`, `curCache`, etc. Then *REQUIRE* the replacement.
+ Secondly, after the replacement has been *DONE*, retrieve the stored states and resume the interrupted rules, and set the replacement state to *NON*.

There are also some tips we should keep careful.

+ All normal rules except replacement stages and resumed stages, must be triggered under the condition that the replacement state is *NON*.
+ All resumed stages must be triggered under the condition that the replacement state is *DONE*.
+ The replacement state must be asserted in every *guard* part of each rule including invariants.
+ If need to assert cache state and cache content, cache state must be asserted first.
+ If need to assert attibutes of locks, the *beUsed* attribute must be asserted first.

The last three tips can be ignored if we initialize all variables in the start state.

### Deal with Counterexamples
#### Properties
There are two properties that the cache coherence protocol must hold.

+ **Deadlock-free Property**: One node can only hold one lock at the same time.
+ **Coherence Property**: The cached data must be the same with data in their associated memories. In Godson-T, this means *Read* operation in *critical region* must cache correct data.

#### Counterexamples and Solutions
With those *strong* properties above, our model will generate a *counterexample*, which implies that two different *process* could cache the same location of memory in critical region protected by different locks, and once one of them modified the memory (with *write through* strategy), the other would cache wrong data.

There are two solutions to this problem.

+ Do not allow multiple locks, e.i., there is only one lock there
+ Weaken the *Coherence Property*, so as to permit the counterexamples, and only require that the cached data must be correct in which critical region protected by the same lock.

### Problem: Pseudo-Protection
There are still counter examples even if we use one lock:

>A node *i* required a lock to enter its critical region, and before it left, another node *j* wrote the memory in *i*'s critical region. Then caches in *i* were not coherent.

The reason of this incoherence is that in our model, the locks do not protect the critical regions in reality.

Since the pseudo-protection of locks lead to the incoherence, we must re-write our murphi code:

1. Add an array attribute `inProtection` of boolean to each lock, so that we can record what actually the critical region protected by this lock is.
2. Before each no-lock-write, we check if the address is in protection of a lock.
3. Before each locked write of a node *i*, we assert that if the address is in protection of lock *l*, then *l* must be the lock that *i* holds.
4. After each operation in critical region, we add the protection record in the new attribute `inProtection`.
5. When a node releases it lock, we remove all protection by the lock.

### Problem: Confusing Replacement
Even if we protect the critical regions in reality, there are still some errors. Suppose there is only one cache in each node (so that fasten the generation of corresponding counterexamples):

>There existed some nodes such as *j* that hold a dirty copy of address *a*. Then a node *i* required a lock, and read *a* as a first read, then *j* wrote memory in another address, which forced the dirty cache of address *a* to be written back. Then, cache in *i* was inherent.

This error caused by the *Confusing Replacement* strategy we use in our model. In fact, there are two diffirent situation which caused a "replacement".

+ If a locked first read occurs, we need write back ALL dirty caches in ALL nodes first, then act the read.
+ In other situations, only when a dirty cache need to be reused, we write back it no matter what state the other caches is in.

In our current model, we confuse the former situation with the latter, which brings an error. Now we need modify our replacement procedure.

![Replacement Procedure](https://raw.githubusercontent.com/Moonshile/MoonshileImages/master/Log/godsont-rep.png)

As the figure illustrated, we add two states in our replacement procedure: REQREPALL and REPALLDONE. Now the states or stages in replacement procedure means:

* NON: do not need Replacement
* REQUIRE: require to replace
* REQREPALL: in Locked First Read, need replace all dirty caches in all nodes
* RANDOM: if there does not exist a INVALID cache, then choose a random one
* RANDINV: if there exists at least a INVALID cache, then choose a random INVALID one
* DESIGNATED: the cache to be replaced has been designated
* TOREP: after DESIGNATED, if the designated cache is DIRTY, then replace it
* DONE: the replacement has been done
* REPALLDONE: the REQREPALL has been done

That is, if we meet a locked 1st read, we first replace all dirty caches in all nodes, then resume to original steps. In this approach, not only can we use nested locks in our model, but also the model can hold the strong *Coherence Property* without weakening it:

>The cached data must be the same with data in their associated memories. In Godson-T, this means *Read* operation in *critical region* must cache correct data.

Now our modeling work has been done well!

### About GSNF
*Guard-Statement Normal Form* (GSNF) is the principle that we use while modeling in murphi. It requires that

+ Never use `if` clause in the *assignment* part of a rule.
+ The indexes or ranks or parameters of array variables must be simple and constant.
    + Simple: the indexes mustn't be nested variables, e.i., are not reference form complex data structures.
    + Constant: the indexes must be either parameter of rulesets or constants, e.i., it's not a common variable.
+ No procedures and functions.
+ The rule names are better to be identifiers.
