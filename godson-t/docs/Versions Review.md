
[TOC]

- - -

##  Godson-T Murphi Modeling
### Review Counterexample-Versions
There are several versions that bring counterexamples.

#### The Very First Version
The very first version `godson-t-ce-0.m`, will cause a counterexample ce0, as `ce0.txt` records, which implies that:

>Two different process could cache the same location of memory in critical region protected by different locks, and once one of them modified the memory (with write through strategy), the other would cache wrong data.

One simple strategy to eliminate this counterexample is to use only 1 lock so that there will exist at most one critical region.

#### The 1 Lock Version
The 1 lock version `godson-t-ce-1-1lock.m` is derived from `godson-t-ce-0.m`, changing the lock parameter to 1 and cache parameter to 1. But this version will generate another counterexample, called ce1, as `ce1.txt` records:

>A node i required a lock to enter its critical region, and before it left, another node j wrote the memory in i's critical region. Then caches in i were not coherent.

The reason is that in our implementation, locks do not prevent other node from writing a critical region, which is called *Pseudo-Protection*.

What is interesting is that, if we do not modify cache parameter, this version runs well. Because when number of cache lines is greater than number of memory cells, the dirty caches will never be forced to write back. But in fact, capacity of caches is far lower than memory, thus this counterexample must happen.

#### The Protection Version
The protection version `godson-t-ce-2-inProtection.m` is derived from `godson-t-ce-1-1lock.m`. Since the pseudo-protection of locks lead to the incoherence, we must re-write our murphi code:

>1. Add an array attribute inProtection of boolean to each lock, so that we can record what actually the critical region protected by this lock is.
>2. Before each no-lock-write, we check if the address is in protection of a lock.
Before each locked write of a node i, we assert that if the address is in protection of lock l, then l must be the lock that i holds.
>3. After each operation in critical region, we add the protection record in the new attribute inProtection.
>4. When a node releases it lock, we remove all protection by the lock.

In fact, in this protection version, there will never exist two different process which try to enter the same critical region because of full protection of it. Therefore, **the *1 lock restrict* is useless now**! We change the lock parameter to 2 or more again (certainly, it can still be 1).

**Unfortunately**, even if we protect the critical regions in reality, there are still some errors. ! Suppose there is only one cache in each node (so that fasten the generation of corresponding counterexamples). This counterexample is called ce2 and be recorded in `ce2.txt`:

>There existed some nodes such as *j* that hold a dirty copy of address *a*. Then a node *i* required a lock, and read *a* as a first read, then *j* wrote memory in another address, which forced the dirty cache of address *a* to be written back. Then, cache in *i* was inherent.

Recall the *FIRST READ IN CRITICAL REGION* defined in Godson-T. Godson-T thinks its first read in ciritical region by node *i* is undependable, because there might exist dirty caches which haven't been written back in other nodes. So all nodes should write back their dirty caches, then node *i* retry its read to get a dependable data.

But, in our model, this type of *replace* is confused with others. So we need to design another procedure of replacement, which replaces all dirty copy in all nodes while a locked first read is comming. Thus we get our first correct version `godson-t-repAll-protect.m`.

#### The Replace-All-Only Version
What if we just give a replacement of all dirty copy in all nodes while a locked first read is comming, and replacements of other types as usual? That is, what if we do not use *full protection* strategy in our current correct version?

Obviously, withour protection, the first counterexample *ce0* will come out again, here it is recorded in `ce3.txt`.

#### The Replace-All-1-Lock Version
What if we add the *1 lock restrict* to the replace-all-only version as in `godson-t-ce-4-repAll-1lock.m`? The terrible counterexample *ce1* will come again! It is recorded in `ce4.txt`.

### Review Correct Versions
All correct versions need to provide a *replace all* strategy, which requires to replace all dirty copy in all nodes while a locked first read is comming. As we known, this version has a obvious counterexample *ce1* under its *Pseudo-Protection*:

>A node i required a lock to enter its critical region, and before it left, another node j wrote the memory in i's critical region. Then caches in i were not coherent.

#### Full-Protenction Version
This version `godson-t-repAll-protect.m` is the correct one that requires to protect critical regions in reality, e.i., before each *write*, check whether the memory unit is not in critical region or the node holds this critical region. Thus we change the *pseudo-protection* to *Full-Protection*, to get a correct version.


