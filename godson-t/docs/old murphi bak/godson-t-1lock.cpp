/******************************
  Program "godson-t-1lock.m" compiled by "Caching Murphi Release 5.4.9"

  Murphi Last Compiled date: "Jun  2 2014"
 ******************************/

/********************
  Parameter
 ********************/
#define MURPHI_VERSION "Caching Murphi Release 5.4.9"
#define MURPHI_DATE "Jun  2 2014"
#define PROTOCOL_NAME "godson-t-1lock"
#define BITS_IN_WORLD 304
#define ALIGN

/********************
  Include
 ********************/
#include "mu_prolog.hpp"

/********************
  Decl declaration
 ********************/

class mu_1_TYPE_NODE: public mu__byte
{
 public:
  inline int operator=(int val) { return mu__byte::operator=(val); };
  inline int operator=(const mu_1_TYPE_NODE& val) { return mu__byte::operator=((int) val); };
  mu_1_TYPE_NODE (const char *name, int os): mu__byte(1, 3, 2, name, os) {};
  mu_1_TYPE_NODE (void): mu__byte(1, 3, 2) {};
  mu_1_TYPE_NODE (int val): mu__byte(1, 3, 2, "Parameter or function result.", 0)
  {
    operator=(val);
  };
  char * Name() { return tsprintf("%d",value()); };
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort() {};
  void print_statistic() {};
};

/*** end of subrange decl ***/
mu_1_TYPE_NODE mu_1_TYPE_NODE_undefined_var;

class mu_1_TYPE_CACHE: public mu__byte
{
 public:
  inline int operator=(int val) { return mu__byte::operator=(val); };
  inline int operator=(const mu_1_TYPE_CACHE& val) { return mu__byte::operator=((int) val); };
  mu_1_TYPE_CACHE (const char *name, int os): mu__byte(1, 2, 2, name, os) {};
  mu_1_TYPE_CACHE (void): mu__byte(1, 2, 2) {};
  mu_1_TYPE_CACHE (int val): mu__byte(1, 2, 2, "Parameter or function result.", 0)
  {
    operator=(val);
  };
  char * Name() { return tsprintf("%d",value()); };
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort() {};
  void print_statistic() {};
};

/*** end of subrange decl ***/
mu_1_TYPE_CACHE mu_1_TYPE_CACHE_undefined_var;

class mu_1_TYPE_ADDR: public mu__byte
{
 public:
  inline int operator=(int val) { return mu__byte::operator=(val); };
  inline int operator=(const mu_1_TYPE_ADDR& val) { return mu__byte::operator=((int) val); };
  mu_1_TYPE_ADDR (const char *name, int os): mu__byte(1, 2, 2, name, os) {};
  mu_1_TYPE_ADDR (void): mu__byte(1, 2, 2) {};
  mu_1_TYPE_ADDR (int val): mu__byte(1, 2, 2, "Parameter or function result.", 0)
  {
    operator=(val);
  };
  char * Name() { return tsprintf("%d",value()); };
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort() {};
  void print_statistic() {};
};

/*** end of subrange decl ***/
mu_1_TYPE_ADDR mu_1_TYPE_ADDR_undefined_var;

class mu_1_TYPE_DATA: public mu__byte
{
 public:
  inline int operator=(int val) { return mu__byte::operator=(val); };
  inline int operator=(const mu_1_TYPE_DATA& val) { return mu__byte::operator=((int) val); };
  mu_1_TYPE_DATA (const char *name, int os): mu__byte(1, 2, 2, name, os) {};
  mu_1_TYPE_DATA (void): mu__byte(1, 2, 2) {};
  mu_1_TYPE_DATA (int val): mu__byte(1, 2, 2, "Parameter or function result.", 0)
  {
    operator=(val);
  };
  char * Name() { return tsprintf("%d",value()); };
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort() {};
  void print_statistic() {};
};

/*** end of subrange decl ***/
mu_1_TYPE_DATA mu_1_TYPE_DATA_undefined_var;

class mu_1_TYPE_LOCK: public mu__byte
{
 public:
  inline int operator=(int val) { return mu__byte::operator=(val); };
  inline int operator=(const mu_1_TYPE_LOCK& val) { return mu__byte::operator=((int) val); };
  mu_1_TYPE_LOCK (const char *name, int os): mu__byte(1, 1, 1, name, os) {};
  mu_1_TYPE_LOCK (void): mu__byte(1, 1, 1) {};
  mu_1_TYPE_LOCK (int val): mu__byte(1, 1, 1, "Parameter or function result.", 0)
  {
    operator=(val);
  };
  char * Name() { return tsprintf("%d",value()); };
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort() {};
  void print_statistic() {};
};

/*** end of subrange decl ***/
mu_1_TYPE_LOCK mu_1_TYPE_LOCK_undefined_var;

class mu_1_CACHE_STATE: public mu__byte
{
 public:
  inline int operator=(int val) { return value(val); };
  inline int operator=(const mu_1_CACHE_STATE& val) { return value(val.value()); };
  static const char *values[];
  friend ostream& operator<< (ostream& s, mu_1_CACHE_STATE& val)
  {
    if (val.defined())
      return ( s << mu_1_CACHE_STATE::values[ int(val) - 1] );
    else return ( s << "Undefined" );
  };

  mu_1_CACHE_STATE (const char *name, int os): mu__byte(1, 3, 2, name, os) {};
  mu_1_CACHE_STATE (void): mu__byte(1, 3, 2) {};
  mu_1_CACHE_STATE (int val): mu__byte(1, 3, 2, "Parameter or function result.", 0)
  {
     operator=(val);
  };
  const char * Name() { return values[ value() -1]; };
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort() {};
  void print_statistic() {};
  virtual void print()
  {
    if (defined())
      cout << name << ":" << values[ value() -1] << '\n';
    else
      cout << name << ":Undefined\n";
  };
};

const char *mu_1_CACHE_STATE::values[] = {"INVALID","DIRTY","VALID",NULL };

/*** end of enum declaration ***/
mu_1_CACHE_STATE mu_1_CACHE_STATE_undefined_var;

class mu_1_CACHE
{
 public:
  char *name;
  char longname[BUFFER_SIZE/4];
  void set_self_2( const char *n, const char *n2, int os);
  void set_self_ar( const char *n, const char *n2, int os);
  void set_self(const char *n, int os);
  mu_1_CACHE_STATE mu_state;
  mu_1_TYPE_ADDR mu_addr;
  mu_1_TYPE_DATA mu_data;
  mu_1_CACHE ( const char *n, int os ) { set_self(n,os); };
  mu_1_CACHE ( void ) {};

  virtual ~mu_1_CACHE(); 
friend int CompareWeight(mu_1_CACHE& a, mu_1_CACHE& b)
  {
    int w;
    w = CompareWeight(a.mu_state, b.mu_state);
    if (w!=0) return w;
    w = CompareWeight(a.mu_addr, b.mu_addr);
    if (w!=0) return w;
    w = CompareWeight(a.mu_data, b.mu_data);
    if (w!=0) return w;
  return 0;
}
friend int Compare(mu_1_CACHE& a, mu_1_CACHE& b)
  {
    int w;
    w = Compare(a.mu_state, b.mu_state);
    if (w!=0) return w;
    w = Compare(a.mu_addr, b.mu_addr);
    if (w!=0) return w;
    w = Compare(a.mu_data, b.mu_data);
    if (w!=0) return w;
  return 0;
}
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort()
  {
    mu_state.MultisetSort();
    mu_addr.MultisetSort();
    mu_data.MultisetSort();
  }
  void print_statistic()
  {
    mu_state.print_statistic();
    mu_addr.print_statistic();
    mu_data.print_statistic();
  }
  void clear() {
    mu_state.clear();
    mu_addr.clear();
    mu_data.clear();
 };
  void undefine() {
    mu_state.undefine();
    mu_addr.undefine();
    mu_data.undefine();
 };
  void reset() {
    mu_state.reset();
    mu_addr.reset();
    mu_data.reset();
 };
  void print() {
    mu_state.print();
    mu_addr.print();
    mu_data.print();
  };
  void print_diff(state *prevstate) {
    mu_state.print_diff(prevstate);
    mu_addr.print_diff(prevstate);
    mu_data.print_diff(prevstate);
  };
  void to_state(state *thestate) {
    mu_state.to_state(thestate);
    mu_addr.to_state(thestate);
    mu_data.to_state(thestate);
  };
virtual bool isundefined() { Error.Error("Checking undefinedness of a non-base type"); return TRUE;}
virtual bool ismember() { Error.Error("Checking membership for a non-base type"); return TRUE;}
  mu_1_CACHE& operator= (const mu_1_CACHE& from) {
    mu_state.value(from.mu_state.value());
    mu_addr.value(from.mu_addr.value());
    mu_data.value(from.mu_data.value());
    return *this;
  };
};

  void mu_1_CACHE::set_self_ar( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    int l1 = strlen(n1), l2 = strlen(n2);
    strcpy( longname, n1 );
    longname[l1] = '[';
    strcpy( longname+l1+1, n2 );
    longname[l1+l2+1] = ']';
    longname[l1+l2+2] = 0;
    set_self( longname, os );
  };
  void mu_1_CACHE::set_self_2( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    strcpy( longname, n1 );
    strcat( longname, n2 );
    set_self( longname, os );
  };
void mu_1_CACHE::set_self(const char *n, int os)
{
  name = (char *)n;

  if (name) mu_state.set_self_2(name, ".state", os + 0 ); else mu_state.set_self_2(NULL, NULL, 0);
  if (name) mu_addr.set_self_2(name, ".addr", os + 8 ); else mu_addr.set_self_2(NULL, NULL, 0);
  if (name) mu_data.set_self_2(name, ".data", os + 16 ); else mu_data.set_self_2(NULL, NULL, 0);
}

mu_1_CACHE::~mu_1_CACHE()
{
}

/*** end record declaration ***/
mu_1_CACHE mu_1_CACHE_undefined_var;

class mu_1_MEMORY
{
 public:
  char *name;
  char longname[BUFFER_SIZE/4];
  void set_self_2( const char *n, const char *n2, int os);
  void set_self_ar( const char *n, const char *n2, int os);
  void set_self(const char *n, int os);
  mu_1_TYPE_DATA mu_data;
  mu_1_MEMORY ( const char *n, int os ) { set_self(n,os); };
  mu_1_MEMORY ( void ) {};

  virtual ~mu_1_MEMORY(); 
friend int CompareWeight(mu_1_MEMORY& a, mu_1_MEMORY& b)
  {
    int w;
    w = CompareWeight(a.mu_data, b.mu_data);
    if (w!=0) return w;
  return 0;
}
friend int Compare(mu_1_MEMORY& a, mu_1_MEMORY& b)
  {
    int w;
    w = Compare(a.mu_data, b.mu_data);
    if (w!=0) return w;
  return 0;
}
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort()
  {
    mu_data.MultisetSort();
  }
  void print_statistic()
  {
    mu_data.print_statistic();
  }
  void clear() {
    mu_data.clear();
 };
  void undefine() {
    mu_data.undefine();
 };
  void reset() {
    mu_data.reset();
 };
  void print() {
    mu_data.print();
  };
  void print_diff(state *prevstate) {
    mu_data.print_diff(prevstate);
  };
  void to_state(state *thestate) {
    mu_data.to_state(thestate);
  };
virtual bool isundefined() { Error.Error("Checking undefinedness of a non-base type"); return TRUE;}
virtual bool ismember() { Error.Error("Checking membership for a non-base type"); return TRUE;}
  mu_1_MEMORY& operator= (const mu_1_MEMORY& from) {
    mu_data.value(from.mu_data.value());
    return *this;
  };
};

  void mu_1_MEMORY::set_self_ar( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    int l1 = strlen(n1), l2 = strlen(n2);
    strcpy( longname, n1 );
    longname[l1] = '[';
    strcpy( longname+l1+1, n2 );
    longname[l1+l2+1] = ']';
    longname[l1+l2+2] = 0;
    set_self( longname, os );
  };
  void mu_1_MEMORY::set_self_2( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    strcpy( longname, n1 );
    strcat( longname, n2 );
    set_self( longname, os );
  };
void mu_1_MEMORY::set_self(const char *n, int os)
{
  name = (char *)n;

  if (name) mu_data.set_self_2(name, ".data", os + 0 ); else mu_data.set_self_2(NULL, NULL, 0);
}

mu_1_MEMORY::~mu_1_MEMORY()
{
}

/*** end record declaration ***/
mu_1_MEMORY mu_1_MEMORY_undefined_var;

class mu_1_LOCK
{
 public:
  char *name;
  char longname[BUFFER_SIZE/4];
  void set_self_2( const char *n, const char *n2, int os);
  void set_self_ar( const char *n, const char *n2, int os);
  void set_self(const char *n, int os);
  mu_1_TYPE_NODE mu_owner;
  mu_0_boolean mu_beUsed;
  mu_1_LOCK ( const char *n, int os ) { set_self(n,os); };
  mu_1_LOCK ( void ) {};

  virtual ~mu_1_LOCK(); 
friend int CompareWeight(mu_1_LOCK& a, mu_1_LOCK& b)
  {
    int w;
    w = CompareWeight(a.mu_owner, b.mu_owner);
    if (w!=0) return w;
    w = CompareWeight(a.mu_beUsed, b.mu_beUsed);
    if (w!=0) return w;
  return 0;
}
friend int Compare(mu_1_LOCK& a, mu_1_LOCK& b)
  {
    int w;
    w = Compare(a.mu_owner, b.mu_owner);
    if (w!=0) return w;
    w = Compare(a.mu_beUsed, b.mu_beUsed);
    if (w!=0) return w;
  return 0;
}
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort()
  {
    mu_owner.MultisetSort();
    mu_beUsed.MultisetSort();
  }
  void print_statistic()
  {
    mu_owner.print_statistic();
    mu_beUsed.print_statistic();
  }
  void clear() {
    mu_owner.clear();
    mu_beUsed.clear();
 };
  void undefine() {
    mu_owner.undefine();
    mu_beUsed.undefine();
 };
  void reset() {
    mu_owner.reset();
    mu_beUsed.reset();
 };
  void print() {
    mu_owner.print();
    mu_beUsed.print();
  };
  void print_diff(state *prevstate) {
    mu_owner.print_diff(prevstate);
    mu_beUsed.print_diff(prevstate);
  };
  void to_state(state *thestate) {
    mu_owner.to_state(thestate);
    mu_beUsed.to_state(thestate);
  };
virtual bool isundefined() { Error.Error("Checking undefinedness of a non-base type"); return TRUE;}
virtual bool ismember() { Error.Error("Checking membership for a non-base type"); return TRUE;}
  mu_1_LOCK& operator= (const mu_1_LOCK& from) {
    mu_owner.value(from.mu_owner.value());
    mu_beUsed.value(from.mu_beUsed.value());
    return *this;
  };
};

  void mu_1_LOCK::set_self_ar( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    int l1 = strlen(n1), l2 = strlen(n2);
    strcpy( longname, n1 );
    longname[l1] = '[';
    strcpy( longname+l1+1, n2 );
    longname[l1+l2+1] = ']';
    longname[l1+l2+2] = 0;
    set_self( longname, os );
  };
  void mu_1_LOCK::set_self_2( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    strcpy( longname, n1 );
    strcat( longname, n2 );
    set_self( longname, os );
  };
void mu_1_LOCK::set_self(const char *n, int os)
{
  name = (char *)n;

  if (name) mu_owner.set_self_2(name, ".owner", os + 0 ); else mu_owner.set_self_2(NULL, NULL, 0);
  if (name) mu_beUsed.set_self_2(name, ".beUsed", os + 8 ); else mu_beUsed.set_self_2(NULL, NULL, 0);
}

mu_1_LOCK::~mu_1_LOCK()
{
}

/*** end record declaration ***/
mu_1_LOCK mu_1_LOCK_undefined_var;

class mu_1__type_0
{
 public:
  mu_1_CACHE array[ 2 ];
 public:
  char *name;
  char longname[BUFFER_SIZE/4];
  void set_self( const char *n, int os);
  void set_self_2( const char *n, const char *n2, int os);
  void set_self_ar( const char *n, const char *n2, int os);
  mu_1__type_0 (const char *n, int os) { set_self(n, os); };
  mu_1__type_0 ( void ) {};
  virtual ~mu_1__type_0 ();
  mu_1_CACHE& operator[] (int index) /* const */
  {
#ifndef NO_RUN_TIME_CHECKING
    if ( ( index >= 1 ) && ( index <= 2 ) )
      return array[ index - 1 ];
    else {
      if (index==UNDEFVAL) 
	Error.Error("Indexing to %s using an undefined value.", name);
      else
	Error.Error("%d not in index range of %s.", index, name);
      return array[0];
    }
#else
    return array[ index - 1 ];
#endif
  };
  mu_1__type_0& operator= (const mu_1__type_0& from)
  {
    for (int i = 0; i < 2; i++)
      array[i] = from.array[i];
    return *this;
  }

friend int CompareWeight(mu_1__type_0& a, mu_1__type_0& b)
  {
    int w;
    for (int i=0; i<2; i++) {
      w = CompareWeight(a.array[i], b.array[i]);
      if (w!=0) return w;
    }
    return 0;
  }
friend int Compare(mu_1__type_0& a, mu_1__type_0& b)
  {
    int w;
    for (int i=0; i<2; i++) {
      w = Compare(a.array[i], b.array[i]);
      if (w!=0) return w;
    }
    return 0;
  }
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort()
  {
    for (int i=0; i<2; i++)
      array[i].MultisetSort();
  }
  void print_statistic()
  {
    for (int i=0; i<2; i++)
      array[i].print_statistic();
  }
  void clear() { for (int i = 0; i < 2; i++) array[i].clear(); };

  void undefine() { for (int i = 0; i < 2; i++) array[i].undefine(); };

  void reset() { for (int i = 0; i < 2; i++) array[i].reset(); };

  void to_state(state *thestate)
  {
    for (int i = 0; i < 2; i++)
      array[i].to_state(thestate);
  };

  void print()
  {
    for (int i = 0; i < 2; i++)
      array[i].print(); };

  void print_diff(state *prevstate)
  {
    for (int i = 0; i < 2; i++)
      array[i].print_diff(prevstate);
  };
};

  void mu_1__type_0::set_self_ar( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    int l1 = strlen(n1), l2 = strlen(n2);
    strcpy( longname, n1 );
    longname[l1] = '[';
    strcpy( longname+l1+1, n2 );
    longname[l1+l2+1] = ']';
    longname[l1+l2+2] = 0;
    set_self( longname, os );
  };
  void mu_1__type_0::set_self_2( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    strcpy( longname, n1 );
    strcat( longname, n2 );
    set_self( longname, os );
  };
void mu_1__type_0::set_self( const char *n, int os)
{
  char* s;
  name = (char *)n;
  for(int i = 0; i < 2; i++) {
    array[i].set_self_ar(n, s=tsprintf("%d",i + 1), i * 24 + os);
    delete[] s;
  }
};
mu_1__type_0::~mu_1__type_0()
{
}
/*** end array declaration ***/
mu_1__type_0 mu_1__type_0_undefined_var;

class mu_1__type_1
{
 public:
  mu_0_boolean array[ 2 ];
 public:
  char *name;
  char longname[BUFFER_SIZE/4];
  void set_self( const char *n, int os);
  void set_self_2( const char *n, const char *n2, int os);
  void set_self_ar( const char *n, const char *n2, int os);
  mu_1__type_1 (const char *n, int os) { set_self(n, os); };
  mu_1__type_1 ( void ) {};
  virtual ~mu_1__type_1 ();
  mu_0_boolean& operator[] (int index) /* const */
  {
#ifndef NO_RUN_TIME_CHECKING
    if ( ( index >= 1 ) && ( index <= 2 ) )
      return array[ index - 1 ];
    else {
      if (index==UNDEFVAL) 
	Error.Error("Indexing to %s using an undefined value.", name);
      else
	Error.Error("%d not in index range of %s.", index, name);
      return array[0];
    }
#else
    return array[ index - 1 ];
#endif
  };
  mu_1__type_1& operator= (const mu_1__type_1& from)
  {
    for (int i = 0; i < 2; i++)
      array[i].value(from.array[i].value());
    return *this;
  }

friend int CompareWeight(mu_1__type_1& a, mu_1__type_1& b)
  {
    int w;
    for (int i=0; i<2; i++) {
      w = CompareWeight(a.array[i], b.array[i]);
      if (w!=0) return w;
    }
    return 0;
  }
friend int Compare(mu_1__type_1& a, mu_1__type_1& b)
  {
    int w;
    for (int i=0; i<2; i++) {
      w = Compare(a.array[i], b.array[i]);
      if (w!=0) return w;
    }
    return 0;
  }
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort()
  {
    for (int i=0; i<2; i++)
      array[i].MultisetSort();
  }
  void print_statistic()
  {
    for (int i=0; i<2; i++)
      array[i].print_statistic();
  }
  void clear() { for (int i = 0; i < 2; i++) array[i].clear(); };

  void undefine() { for (int i = 0; i < 2; i++) array[i].undefine(); };

  void reset() { for (int i = 0; i < 2; i++) array[i].reset(); };

  void to_state(state *thestate)
  {
    for (int i = 0; i < 2; i++)
      array[i].to_state(thestate);
  };

  void print()
  {
    for (int i = 0; i < 2; i++)
      array[i].print(); };

  void print_diff(state *prevstate)
  {
    for (int i = 0; i < 2; i++)
      array[i].print_diff(prevstate);
  };
};

  void mu_1__type_1::set_self_ar( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    int l1 = strlen(n1), l2 = strlen(n2);
    strcpy( longname, n1 );
    longname[l1] = '[';
    strcpy( longname+l1+1, n2 );
    longname[l1+l2+1] = ']';
    longname[l1+l2+2] = 0;
    set_self( longname, os );
  };
  void mu_1__type_1::set_self_2( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    strcpy( longname, n1 );
    strcat( longname, n2 );
    set_self( longname, os );
  };
void mu_1__type_1::set_self( const char *n, int os)
{
  char* s;
  name = (char *)n;
  for(int i = 0; i < 2; i++) {
    array[i].set_self_ar(n, s=tsprintf("%d",i + 1), i * 8 + os);
    delete[] s;
  }
};
mu_1__type_1::~mu_1__type_1()
{
}
/*** end array declaration ***/
mu_1__type_1 mu_1__type_1_undefined_var;

class mu_1_NODE
{
 public:
  char *name;
  char longname[BUFFER_SIZE/4];
  void set_self_2( const char *n, const char *n2, int os);
  void set_self_ar( const char *n, const char *n2, int os);
  void set_self(const char *n, int os);
  mu_1__type_0 mu_cache;
  mu_0_boolean mu_hasLock;
  mu_1__type_1 mu_firstRead;
  mu_1_NODE ( const char *n, int os ) { set_self(n,os); };
  mu_1_NODE ( void ) {};

  virtual ~mu_1_NODE(); 
friend int CompareWeight(mu_1_NODE& a, mu_1_NODE& b)
  {
    int w;
    w = CompareWeight(a.mu_cache, b.mu_cache);
    if (w!=0) return w;
    w = CompareWeight(a.mu_hasLock, b.mu_hasLock);
    if (w!=0) return w;
    w = CompareWeight(a.mu_firstRead, b.mu_firstRead);
    if (w!=0) return w;
  return 0;
}
friend int Compare(mu_1_NODE& a, mu_1_NODE& b)
  {
    int w;
    w = Compare(a.mu_cache, b.mu_cache);
    if (w!=0) return w;
    w = Compare(a.mu_hasLock, b.mu_hasLock);
    if (w!=0) return w;
    w = Compare(a.mu_firstRead, b.mu_firstRead);
    if (w!=0) return w;
  return 0;
}
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort()
  {
    mu_cache.MultisetSort();
    mu_hasLock.MultisetSort();
    mu_firstRead.MultisetSort();
  }
  void print_statistic()
  {
    mu_cache.print_statistic();
    mu_hasLock.print_statistic();
    mu_firstRead.print_statistic();
  }
  void clear() {
    mu_cache.clear();
    mu_hasLock.clear();
    mu_firstRead.clear();
 };
  void undefine() {
    mu_cache.undefine();
    mu_hasLock.undefine();
    mu_firstRead.undefine();
 };
  void reset() {
    mu_cache.reset();
    mu_hasLock.reset();
    mu_firstRead.reset();
 };
  void print() {
    mu_cache.print();
    mu_hasLock.print();
    mu_firstRead.print();
  };
  void print_diff(state *prevstate) {
    mu_cache.print_diff(prevstate);
    mu_hasLock.print_diff(prevstate);
    mu_firstRead.print_diff(prevstate);
  };
  void to_state(state *thestate) {
    mu_cache.to_state(thestate);
    mu_hasLock.to_state(thestate);
    mu_firstRead.to_state(thestate);
  };
virtual bool isundefined() { Error.Error("Checking undefinedness of a non-base type"); return TRUE;}
virtual bool ismember() { Error.Error("Checking membership for a non-base type"); return TRUE;}
  mu_1_NODE& operator= (const mu_1_NODE& from) {
    mu_cache = from.mu_cache;
    mu_hasLock.value(from.mu_hasLock.value());
    mu_firstRead = from.mu_firstRead;
    return *this;
  };
};

  void mu_1_NODE::set_self_ar( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    int l1 = strlen(n1), l2 = strlen(n2);
    strcpy( longname, n1 );
    longname[l1] = '[';
    strcpy( longname+l1+1, n2 );
    longname[l1+l2+1] = ']';
    longname[l1+l2+2] = 0;
    set_self( longname, os );
  };
  void mu_1_NODE::set_self_2( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    strcpy( longname, n1 );
    strcat( longname, n2 );
    set_self( longname, os );
  };
void mu_1_NODE::set_self(const char *n, int os)
{
  name = (char *)n;

  if (name) mu_cache.set_self_2(name, ".cache", os + 0 ); else mu_cache.set_self_2(NULL, NULL, 0);
  if (name) mu_hasLock.set_self_2(name, ".hasLock", os + 48 ); else mu_hasLock.set_self_2(NULL, NULL, 0);
  if (name) mu_firstRead.set_self_2(name, ".firstRead", os + 56 ); else mu_firstRead.set_self_2(NULL, NULL, 0);
}

mu_1_NODE::~mu_1_NODE()
{
}

/*** end record declaration ***/
mu_1_NODE mu_1_NODE_undefined_var;

class mu_1_REPLACE_STAGE: public mu__byte
{
 public:
  inline int operator=(int val) { return value(val); };
  inline int operator=(const mu_1_REPLACE_STAGE& val) { return value(val.value()); };
  static const char *values[];
  friend ostream& operator<< (ostream& s, mu_1_REPLACE_STAGE& val)
  {
    if (val.defined())
      return ( s << mu_1_REPLACE_STAGE::values[ int(val) - 4] );
    else return ( s << "Undefined" );
  };

  mu_1_REPLACE_STAGE (const char *name, int os): mu__byte(4, 10, 3, name, os) {};
  mu_1_REPLACE_STAGE (void): mu__byte(4, 10, 3) {};
  mu_1_REPLACE_STAGE (int val): mu__byte(4, 10, 3, "Parameter or function result.", 0)
  {
     operator=(val);
  };
  const char * Name() { return values[ value() -4]; };
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort() {};
  void print_statistic() {};
  virtual void print()
  {
    if (defined())
      cout << name << ":" << values[ value() -4] << '\n';
    else
      cout << name << ":Undefined\n";
  };
};

const char *mu_1_REPLACE_STAGE::values[] = {"NON","REQUIRE","RANDOM","RANDINV","DESIGNATED","TOREP","DONE",NULL };

/*** end of enum declaration ***/
mu_1_REPLACE_STAGE mu_1_REPLACE_STAGE_undefined_var;

class mu_1_REPLACE_RULE: public mu__byte
{
 public:
  inline int operator=(int val) { return value(val); };
  inline int operator=(const mu_1_REPLACE_RULE& val) { return value(val.value()); };
  static const char *values[];
  friend ostream& operator<< (ostream& s, mu_1_REPLACE_RULE& val)
  {
    if (val.defined())
      return ( s << mu_1_REPLACE_RULE::values[ int(val) - 11] );
    else return ( s << "Undefined" );
  };

  mu_1_REPLACE_RULE (const char *name, int os): mu__byte(11, 16, 3, name, os) {};
  mu_1_REPLACE_RULE (void): mu__byte(11, 16, 3) {};
  mu_1_REPLACE_RULE (int val): mu__byte(11, 16, 3, "Parameter or function result.", 0)
  {
     operator=(val);
  };
  const char * Name() { return values[ value() -11]; };
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort() {};
  void print_statistic() {};
  virtual void print()
  {
    if (defined())
      cout << name << ":" << values[ value() -11] << '\n';
    else
      cout << name << ":Undefined\n";
  };
};

const char *mu_1_REPLACE_RULE::values[] = {"NONE","NLNCR","NLNCW","LNCFR","LCFR","LNCNFR",NULL };

/*** end of enum declaration ***/
mu_1_REPLACE_RULE mu_1_REPLACE_RULE_undefined_var;

class mu_1__type_2
{
 public:
  mu_1_MEMORY array[ 2 ];
 public:
  char *name;
  char longname[BUFFER_SIZE/4];
  void set_self( const char *n, int os);
  void set_self_2( const char *n, const char *n2, int os);
  void set_self_ar( const char *n, const char *n2, int os);
  mu_1__type_2 (const char *n, int os) { set_self(n, os); };
  mu_1__type_2 ( void ) {};
  virtual ~mu_1__type_2 ();
  mu_1_MEMORY& operator[] (int index) /* const */
  {
#ifndef NO_RUN_TIME_CHECKING
    if ( ( index >= 1 ) && ( index <= 2 ) )
      return array[ index - 1 ];
    else {
      if (index==UNDEFVAL) 
	Error.Error("Indexing to %s using an undefined value.", name);
      else
	Error.Error("%d not in index range of %s.", index, name);
      return array[0];
    }
#else
    return array[ index - 1 ];
#endif
  };
  mu_1__type_2& operator= (const mu_1__type_2& from)
  {
    for (int i = 0; i < 2; i++)
      array[i] = from.array[i];
    return *this;
  }

friend int CompareWeight(mu_1__type_2& a, mu_1__type_2& b)
  {
    int w;
    for (int i=0; i<2; i++) {
      w = CompareWeight(a.array[i], b.array[i]);
      if (w!=0) return w;
    }
    return 0;
  }
friend int Compare(mu_1__type_2& a, mu_1__type_2& b)
  {
    int w;
    for (int i=0; i<2; i++) {
      w = Compare(a.array[i], b.array[i]);
      if (w!=0) return w;
    }
    return 0;
  }
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort()
  {
    for (int i=0; i<2; i++)
      array[i].MultisetSort();
  }
  void print_statistic()
  {
    for (int i=0; i<2; i++)
      array[i].print_statistic();
  }
  void clear() { for (int i = 0; i < 2; i++) array[i].clear(); };

  void undefine() { for (int i = 0; i < 2; i++) array[i].undefine(); };

  void reset() { for (int i = 0; i < 2; i++) array[i].reset(); };

  void to_state(state *thestate)
  {
    for (int i = 0; i < 2; i++)
      array[i].to_state(thestate);
  };

  void print()
  {
    for (int i = 0; i < 2; i++)
      array[i].print(); };

  void print_diff(state *prevstate)
  {
    for (int i = 0; i < 2; i++)
      array[i].print_diff(prevstate);
  };
};

  void mu_1__type_2::set_self_ar( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    int l1 = strlen(n1), l2 = strlen(n2);
    strcpy( longname, n1 );
    longname[l1] = '[';
    strcpy( longname+l1+1, n2 );
    longname[l1+l2+1] = ']';
    longname[l1+l2+2] = 0;
    set_self( longname, os );
  };
  void mu_1__type_2::set_self_2( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    strcpy( longname, n1 );
    strcat( longname, n2 );
    set_self( longname, os );
  };
void mu_1__type_2::set_self( const char *n, int os)
{
  char* s;
  name = (char *)n;
  for(int i = 0; i < 2; i++) {
    array[i].set_self_ar(n, s=tsprintf("%d",i + 1), i * 8 + os);
    delete[] s;
  }
};
mu_1__type_2::~mu_1__type_2()
{
}
/*** end array declaration ***/
mu_1__type_2 mu_1__type_2_undefined_var;

class mu_1__type_3
{
 public:
  mu_1_LOCK array[ 1 ];
 public:
  char *name;
  char longname[BUFFER_SIZE/4];
  void set_self( const char *n, int os);
  void set_self_2( const char *n, const char *n2, int os);
  void set_self_ar( const char *n, const char *n2, int os);
  mu_1__type_3 (const char *n, int os) { set_self(n, os); };
  mu_1__type_3 ( void ) {};
  virtual ~mu_1__type_3 ();
  mu_1_LOCK& operator[] (int index) /* const */
  {
#ifndef NO_RUN_TIME_CHECKING
    if ( ( index >= 1 ) && ( index <= 1 ) )
      return array[ index - 1 ];
    else {
      if (index==UNDEFVAL) 
	Error.Error("Indexing to %s using an undefined value.", name);
      else
	Error.Error("%d not in index range of %s.", index, name);
      return array[0];
    }
#else
    return array[ index - 1 ];
#endif
  };
  mu_1__type_3& operator= (const mu_1__type_3& from)
  {
      array[0] = from.array[0];
    return *this;
  }

friend int CompareWeight(mu_1__type_3& a, mu_1__type_3& b)
  {
    int w;
    for (int i=0; i<1; i++) {
      w = CompareWeight(a.array[i], b.array[i]);
      if (w!=0) return w;
    }
    return 0;
  }
friend int Compare(mu_1__type_3& a, mu_1__type_3& b)
  {
    int w;
    for (int i=0; i<1; i++) {
      w = Compare(a.array[i], b.array[i]);
      if (w!=0) return w;
    }
    return 0;
  }
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort()
  {
    for (int i=0; i<1; i++)
      array[i].MultisetSort();
  }
  void print_statistic()
  {
    for (int i=0; i<1; i++)
      array[i].print_statistic();
  }
  void clear() { for (int i = 0; i < 1; i++) array[i].clear(); };

  void undefine() { for (int i = 0; i < 1; i++) array[i].undefine(); };

  void reset() { for (int i = 0; i < 1; i++) array[i].reset(); };

  void to_state(state *thestate)
  {
    for (int i = 0; i < 1; i++)
      array[i].to_state(thestate);
  };

  void print()
  {
    for (int i = 0; i < 1; i++)
      array[i].print(); };

  void print_diff(state *prevstate)
  {
    for (int i = 0; i < 1; i++)
      array[i].print_diff(prevstate);
  };
};

  void mu_1__type_3::set_self_ar( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    int l1 = strlen(n1), l2 = strlen(n2);
    strcpy( longname, n1 );
    longname[l1] = '[';
    strcpy( longname+l1+1, n2 );
    longname[l1+l2+1] = ']';
    longname[l1+l2+2] = 0;
    set_self( longname, os );
  };
  void mu_1__type_3::set_self_2( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    strcpy( longname, n1 );
    strcat( longname, n2 );
    set_self( longname, os );
  };
void mu_1__type_3::set_self( const char *n, int os)
{
  char* s;
  name = (char *)n;
  for(int i = 0; i < 1; i++) {
    array[i].set_self_ar(n, s=tsprintf("%d",i + 1), i * 16 + os);
    delete[] s;
  }
};
mu_1__type_3::~mu_1__type_3()
{
}
/*** end array declaration ***/
mu_1__type_3 mu_1__type_3_undefined_var;

class mu_1__type_4
{
 public:
  mu_1_NODE array[ 3 ];
 public:
  char *name;
  char longname[BUFFER_SIZE/4];
  void set_self( const char *n, int os);
  void set_self_2( const char *n, const char *n2, int os);
  void set_self_ar( const char *n, const char *n2, int os);
  mu_1__type_4 (const char *n, int os) { set_self(n, os); };
  mu_1__type_4 ( void ) {};
  virtual ~mu_1__type_4 ();
  mu_1_NODE& operator[] (int index) /* const */
  {
#ifndef NO_RUN_TIME_CHECKING
    if ( ( index >= 1 ) && ( index <= 3 ) )
      return array[ index - 1 ];
    else {
      if (index==UNDEFVAL) 
	Error.Error("Indexing to %s using an undefined value.", name);
      else
	Error.Error("%d not in index range of %s.", index, name);
      return array[0];
    }
#else
    return array[ index - 1 ];
#endif
  };
  mu_1__type_4& operator= (const mu_1__type_4& from)
  {
    for (int i = 0; i < 3; i++)
      array[i] = from.array[i];
    return *this;
  }

friend int CompareWeight(mu_1__type_4& a, mu_1__type_4& b)
  {
    int w;
    for (int i=0; i<3; i++) {
      w = CompareWeight(a.array[i], b.array[i]);
      if (w!=0) return w;
    }
    return 0;
  }
friend int Compare(mu_1__type_4& a, mu_1__type_4& b)
  {
    int w;
    for (int i=0; i<3; i++) {
      w = Compare(a.array[i], b.array[i]);
      if (w!=0) return w;
    }
    return 0;
  }
  virtual void Permute(PermSet& Perm, int i);
  virtual void SimpleCanonicalize(PermSet& Perm);
  virtual void Canonicalize(PermSet& Perm);
  virtual void SimpleLimit(PermSet& Perm);
  virtual void ArrayLimit(PermSet& Perm);
  virtual void Limit(PermSet& Perm);
  virtual void MultisetLimit(PermSet& Perm);
  virtual void MultisetSort()
  {
    for (int i=0; i<3; i++)
      array[i].MultisetSort();
  }
  void print_statistic()
  {
    for (int i=0; i<3; i++)
      array[i].print_statistic();
  }
  void clear() { for (int i = 0; i < 3; i++) array[i].clear(); };

  void undefine() { for (int i = 0; i < 3; i++) array[i].undefine(); };

  void reset() { for (int i = 0; i < 3; i++) array[i].reset(); };

  void to_state(state *thestate)
  {
    for (int i = 0; i < 3; i++)
      array[i].to_state(thestate);
  };

  void print()
  {
    for (int i = 0; i < 3; i++)
      array[i].print(); };

  void print_diff(state *prevstate)
  {
    for (int i = 0; i < 3; i++)
      array[i].print_diff(prevstate);
  };
};

  void mu_1__type_4::set_self_ar( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    int l1 = strlen(n1), l2 = strlen(n2);
    strcpy( longname, n1 );
    longname[l1] = '[';
    strcpy( longname+l1+1, n2 );
    longname[l1+l2+1] = ']';
    longname[l1+l2+2] = 0;
    set_self( longname, os );
  };
  void mu_1__type_4::set_self_2( const char *n1, const char *n2, int os ) {
    if (n1 == NULL) {set_self(NULL, 0); return;}
    strcpy( longname, n1 );
    strcat( longname, n2 );
    set_self( longname, os );
  };
void mu_1__type_4::set_self( const char *n, int os)
{
  char* s;
  name = (char *)n;
  for(int i = 0; i < 3; i++) {
    array[i].set_self_ar(n, s=tsprintf("%d",i + 1), i * 72 + os);
    delete[] s;
  }
};
mu_1__type_4::~mu_1__type_4()
{
}
/*** end array declaration ***/
mu_1__type_4 mu_1__type_4_undefined_var;

const int mu_NUM_NODE = 3;
const int mu_NUM_CACHE = 2;
const int mu_NUM_ADDR = 2;
const int mu_NUM_DATA = 2;
const int mu_NUM_LOCK = 1;
const int mu_INVALID = 1;
const int mu_DIRTY = 2;
const int mu_VALID = 3;
const int mu_NON = 4;
const int mu_REQUIRE = 5;
const int mu_RANDOM = 6;
const int mu_RANDINV = 7;
const int mu_DESIGNATED = 8;
const int mu_TOREP = 9;
const int mu_DONE = 10;
const int mu_NONE = 11;
const int mu_NLNCR = 12;
const int mu_NLNCW = 13;
const int mu_LNCFR = 14;
const int mu_LCFR = 15;
const int mu_LNCNFR = 16;
/*** Variable declaration ***/
mu_1__type_2 mu_memory("memory",0);

/*** Variable declaration ***/
mu_1__type_3 mu_lock("lock",16);

/*** Variable declaration ***/
mu_1__type_4 mu_node("node",32);

/*** Variable declaration ***/
mu_1_TYPE_NODE mu_curNode("curNode",248);

/*** Variable declaration ***/
mu_1_TYPE_CACHE mu_curCache("curCache",256);

/*** Variable declaration ***/
mu_1_TYPE_ADDR mu_curMemory("curMemory",264);

/*** Variable declaration ***/
mu_1_TYPE_DATA mu_curData("curData",272);

/*** Variable declaration ***/
mu_1_TYPE_LOCK mu_curLock("curLock",280);

/*** Variable declaration ***/
mu_1_REPLACE_STAGE mu_replace("replace",288);

/*** Variable declaration ***/
mu_1_REPLACE_RULE mu_repRule("repRule",296);





/********************
  The world
 ********************/
void world_class::clear()
{
  mu_memory.clear();
  mu_lock.clear();
  mu_node.clear();
  mu_curNode.clear();
  mu_curCache.clear();
  mu_curMemory.clear();
  mu_curData.clear();
  mu_curLock.clear();
  mu_replace.clear();
  mu_repRule.clear();
}
void world_class::undefine()
{
  mu_memory.undefine();
  mu_lock.undefine();
  mu_node.undefine();
  mu_curNode.undefine();
  mu_curCache.undefine();
  mu_curMemory.undefine();
  mu_curData.undefine();
  mu_curLock.undefine();
  mu_replace.undefine();
  mu_repRule.undefine();
}
void world_class::reset()
{
  mu_memory.reset();
  mu_lock.reset();
  mu_node.reset();
  mu_curNode.reset();
  mu_curCache.reset();
  mu_curMemory.reset();
  mu_curData.reset();
  mu_curLock.reset();
  mu_replace.reset();
  mu_repRule.reset();
}
void world_class::print()
{
  static int num_calls = 0; /* to ward off recursive calls. */
  if ( num_calls == 0 ) {
    num_calls++;
  mu_memory.print();
  mu_lock.print();
  mu_node.print();
  mu_curNode.print();
  mu_curCache.print();
  mu_curMemory.print();
  mu_curData.print();
  mu_curLock.print();
  mu_replace.print();
  mu_repRule.print();
    num_calls--;
}
}
void world_class::print_statistic()
{
  static int num_calls = 0; /* to ward off recursive calls. */
  if ( num_calls == 0 ) {
    num_calls++;
  mu_memory.print_statistic();
  mu_lock.print_statistic();
  mu_node.print_statistic();
  mu_curNode.print_statistic();
  mu_curCache.print_statistic();
  mu_curMemory.print_statistic();
  mu_curData.print_statistic();
  mu_curLock.print_statistic();
  mu_replace.print_statistic();
  mu_repRule.print_statistic();
    num_calls--;
}
}
void world_class::print_diff( state *prevstate )
{
  if ( prevstate != NULL )
  {
    mu_memory.print_diff(prevstate);
    mu_lock.print_diff(prevstate);
    mu_node.print_diff(prevstate);
    mu_curNode.print_diff(prevstate);
    mu_curCache.print_diff(prevstate);
    mu_curMemory.print_diff(prevstate);
    mu_curData.print_diff(prevstate);
    mu_curLock.print_diff(prevstate);
    mu_replace.print_diff(prevstate);
    mu_repRule.print_diff(prevstate);
  }
  else
print();
}
void world_class::to_state(state *newstate)
{
  mu_memory.to_state( newstate );
  mu_lock.to_state( newstate );
  mu_node.to_state( newstate );
  mu_curNode.to_state( newstate );
  mu_curCache.to_state( newstate );
  mu_curMemory.to_state( newstate );
  mu_curData.to_state( newstate );
  mu_curLock.to_state( newstate );
  mu_replace.to_state( newstate );
  mu_repRule.to_state( newstate );
}
void world_class::setstate(state *thestate)
{
}


/********************
  Rule declarations
 ********************/
/******************** RuleBase0 ********************/
class RuleBase0
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("Release, l:%s, i:%s", mu_l.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr5;
bool mu__boolexpr6;
bool mu__boolexpr7;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr7 = FALSE ;
  else {
  mu__boolexpr7 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr7)) mu__boolexpr6 = FALSE ;
  else {
  mu__boolexpr6 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr6)) mu__boolexpr5 = FALSE ;
  else {
  mu__boolexpr5 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
    return mu__boolexpr5;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 0;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 3 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr8;
bool mu__boolexpr9;
bool mu__boolexpr10;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr10 = FALSE ;
  else {
  mu__boolexpr10 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr10)) mu__boolexpr9 = FALSE ;
  else {
  mu__boolexpr9 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr9)) mu__boolexpr8 = FALSE ;
  else {
  mu__boolexpr8 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
	      if (mu__boolexpr8) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 0;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_lock[mu_l].mu_beUsed = mu_false;
mu_node[mu_i].mu_hasLock = mu_false;
  };

};
/******************** RuleBase1 ********************/
class RuleBase1
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("Acquire, l:%s, i:%s", mu_l.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr11;
bool mu__boolexpr12;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr12 = FALSE ;
  else {
  mu__boolexpr12 = (!(mu_node[mu_i].mu_hasLock)) ; 
}
  if (!(mu__boolexpr12)) mu__boolexpr11 = FALSE ;
  else {
  mu__boolexpr11 = (!(mu_lock[mu_l].mu_beUsed)) ; 
}
    return mu__boolexpr11;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 3;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 6 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr13;
bool mu__boolexpr14;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr14 = FALSE ;
  else {
  mu__boolexpr14 = (!(mu_node[mu_i].mu_hasLock)) ; 
}
  if (!(mu__boolexpr14)) mu__boolexpr13 = FALSE ;
  else {
  mu__boolexpr13 = (!(mu_lock[mu_l].mu_beUsed)) ; 
}
	      if (mu__boolexpr13) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 3;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_lock[mu_l].mu_beUsed = mu_true;
mu_lock[mu_l].mu_owner = mu_i;
mu_node[mu_i].mu_hasLock = mu_true;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
mu_node[mu_i].mu_firstRead[mu_j] = mu_true;
};
};
  };

};
/******************** RuleBase2 ********************/
class RuleBase2
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("LNCW, d:%s, l:%s, a:%s, i:%s", mu_d.Name(), mu_l.Name(), mu_a.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr15;
bool mu__boolexpr16;
bool mu__boolexpr17;
bool mu__boolexpr18;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr18 = FALSE ;
  else {
  mu__boolexpr18 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr18)) mu__boolexpr17 = FALSE ;
  else {
  mu__boolexpr17 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr17)) mu__boolexpr16 = FALSE ;
  else {
  mu__boolexpr16 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
  if (!(mu__boolexpr16)) mu__boolexpr15 = FALSE ;
  else {
bool mu__quant19; 
mu__quant19 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
bool mu__boolexpr20;
  if ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) mu__boolexpr20 = TRUE ;
  else {
  mu__boolexpr20 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) != (mu_a)) ; 
}
if ( !(mu__boolexpr20) )
  { mu__quant19 = FALSE; break; }
};
};
  mu__boolexpr15 = (mu__quant19) ; 
}
    return mu__boolexpr15;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 6;
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 18 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr21;
bool mu__boolexpr22;
bool mu__boolexpr23;
bool mu__boolexpr24;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr24 = FALSE ;
  else {
  mu__boolexpr24 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr24)) mu__boolexpr23 = FALSE ;
  else {
  mu__boolexpr23 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr23)) mu__boolexpr22 = FALSE ;
  else {
  mu__boolexpr22 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
  if (!(mu__boolexpr22)) mu__boolexpr21 = FALSE ;
  else {
bool mu__quant25; 
mu__quant25 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
bool mu__boolexpr26;
  if ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) mu__boolexpr26 = TRUE ;
  else {
  mu__boolexpr26 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) != (mu_a)) ; 
}
if ( !(mu__boolexpr26) )
  { mu__quant25 = FALSE; break; }
};
};
  mu__boolexpr21 = (mu__quant25) ; 
}
	      if (mu__boolexpr21) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 2;
	}
	else
	  what_rule += 2;
    r = what_rule - 6;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_memory[mu_a].mu_data = mu_d;
  };

};
/******************** RuleBase3 ********************/
class RuleBase3
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("LCW, d:%s, l:%s, a:%s, j:%s, i:%s", mu_d.Name(), mu_l.Name(), mu_a.Name(), mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr27;
bool mu__boolexpr28;
bool mu__boolexpr29;
bool mu__boolexpr30;
bool mu__boolexpr31;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr31 = FALSE ;
  else {
  mu__boolexpr31 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr31)) mu__boolexpr30 = FALSE ;
  else {
  mu__boolexpr30 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr30)) mu__boolexpr29 = FALSE ;
  else {
  mu__boolexpr29 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
  if (!(mu__boolexpr29)) mu__boolexpr28 = FALSE ;
  else {
  mu__boolexpr28 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) != (mu_INVALID)) ; 
}
  if (!(mu__boolexpr28)) mu__boolexpr27 = FALSE ;
  else {
  mu__boolexpr27 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) == (mu_a)) ; 
}
    return mu__boolexpr27;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 18;
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 42 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr32;
bool mu__boolexpr33;
bool mu__boolexpr34;
bool mu__boolexpr35;
bool mu__boolexpr36;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr36 = FALSE ;
  else {
  mu__boolexpr36 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr36)) mu__boolexpr35 = FALSE ;
  else {
  mu__boolexpr35 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr35)) mu__boolexpr34 = FALSE ;
  else {
  mu__boolexpr34 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
  if (!(mu__boolexpr34)) mu__boolexpr33 = FALSE ;
  else {
  mu__boolexpr33 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) != (mu_INVALID)) ; 
}
  if (!(mu__boolexpr33)) mu__boolexpr32 = FALSE ;
  else {
  mu__boolexpr32 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) == (mu_a)) ; 
}
	      if (mu__boolexpr32) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 2;
	}
	else
	  what_rule += 2;
    r = what_rule - 18;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_memory[mu_a].mu_data = mu_d;
mu_node[mu_i].mu_cache[mu_j].mu_data = mu_d;
mu_node[mu_i].mu_cache[mu_j].mu_state = mu_VALID;
  };

};
/******************** RuleBase4 ********************/
class RuleBase4
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("LNCNFRD, l:%s, a:%s, j:%s, i:%s", mu_l.Name(), mu_a.Name(), mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr37;
bool mu__boolexpr38;
bool mu__boolexpr39;
bool mu__boolexpr40;
bool mu__boolexpr41;
  if (!((mu_replace) == (mu_DONE))) mu__boolexpr41 = FALSE ;
  else {
  mu__boolexpr41 = ((mu_repRule) == (mu_LNCNFR)) ; 
}
  if (!(mu__boolexpr41)) mu__boolexpr40 = FALSE ;
  else {
  mu__boolexpr40 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr40)) mu__boolexpr39 = FALSE ;
  else {
  mu__boolexpr39 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr39)) mu__boolexpr38 = FALSE ;
  else {
  mu__boolexpr38 = ((mu_a) == (mu_curMemory)) ; 
}
  if (!(mu__boolexpr38)) mu__boolexpr37 = FALSE ;
  else {
  mu__boolexpr37 = ((mu_l) == (mu_curLock)) ; 
}
    return mu__boolexpr37;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 42;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 54 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr42;
bool mu__boolexpr43;
bool mu__boolexpr44;
bool mu__boolexpr45;
bool mu__boolexpr46;
  if (!((mu_replace) == (mu_DONE))) mu__boolexpr46 = FALSE ;
  else {
  mu__boolexpr46 = ((mu_repRule) == (mu_LNCNFR)) ; 
}
  if (!(mu__boolexpr46)) mu__boolexpr45 = FALSE ;
  else {
  mu__boolexpr45 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr45)) mu__boolexpr44 = FALSE ;
  else {
  mu__boolexpr44 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr44)) mu__boolexpr43 = FALSE ;
  else {
  mu__boolexpr43 = ((mu_a) == (mu_curMemory)) ; 
}
  if (!(mu__boolexpr43)) mu__boolexpr42 = FALSE ;
  else {
  mu__boolexpr42 = ((mu_l) == (mu_curLock)) ; 
}
	      if (mu__boolexpr42) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 42;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_node[mu_i].mu_cache[mu_j].mu_addr = mu_a;
mu_node[mu_i].mu_cache[mu_j].mu_data = mu_memory[mu_a].mu_data;
mu_node[mu_i].mu_cache[mu_j].mu_state = mu_VALID;
mu_replace = mu_NON;
mu_repRule = mu_NONE;
  };

};
/******************** RuleBase5 ********************/
class RuleBase5
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("LNCNFRR, l:%s, a:%s, i:%s", mu_l.Name(), mu_a.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr47;
bool mu__boolexpr48;
bool mu__boolexpr49;
bool mu__boolexpr50;
bool mu__boolexpr51;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr51 = FALSE ;
  else {
  mu__boolexpr51 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr51)) mu__boolexpr50 = FALSE ;
  else {
  mu__boolexpr50 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr50)) mu__boolexpr49 = FALSE ;
  else {
  mu__boolexpr49 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
  if (!(mu__boolexpr49)) mu__boolexpr48 = FALSE ;
  else {
  mu__boolexpr48 = (!(mu_node[mu_i].mu_firstRead[mu_a])) ; 
}
  if (!(mu__boolexpr48)) mu__boolexpr47 = FALSE ;
  else {
bool mu__quant52; 
mu__quant52 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
bool mu__boolexpr53;
  if ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) mu__boolexpr53 = TRUE ;
  else {
  mu__boolexpr53 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) != (mu_a)) ; 
}
if ( !(mu__boolexpr53) )
  { mu__quant52 = FALSE; break; }
};
};
  mu__boolexpr47 = (mu__quant52) ; 
}
    return mu__boolexpr47;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 54;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 60 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr54;
bool mu__boolexpr55;
bool mu__boolexpr56;
bool mu__boolexpr57;
bool mu__boolexpr58;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr58 = FALSE ;
  else {
  mu__boolexpr58 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr58)) mu__boolexpr57 = FALSE ;
  else {
  mu__boolexpr57 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr57)) mu__boolexpr56 = FALSE ;
  else {
  mu__boolexpr56 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
  if (!(mu__boolexpr56)) mu__boolexpr55 = FALSE ;
  else {
  mu__boolexpr55 = (!(mu_node[mu_i].mu_firstRead[mu_a])) ; 
}
  if (!(mu__boolexpr55)) mu__boolexpr54 = FALSE ;
  else {
bool mu__quant59; 
mu__quant59 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
bool mu__boolexpr60;
  if ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) mu__boolexpr60 = TRUE ;
  else {
  mu__boolexpr60 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) != (mu_a)) ; 
}
if ( !(mu__boolexpr60) )
  { mu__quant59 = FALSE; break; }
};
};
  mu__boolexpr54 = (mu__quant59) ; 
}
	      if (mu__boolexpr54) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 54;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_curNode = mu_i;
mu_curMemory = mu_a;
mu_curLock = mu_l;
mu_replace = mu_REQUIRE;
mu_repRule = mu_LNCNFR;
  };

};
/******************** RuleBase6 ********************/
class RuleBase6
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("LNCFRD, l:%s, a:%s, j:%s, i:%s", mu_l.Name(), mu_a.Name(), mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr61;
bool mu__boolexpr62;
bool mu__boolexpr63;
bool mu__boolexpr64;
bool mu__boolexpr65;
  if (!((mu_replace) == (mu_DONE))) mu__boolexpr65 = FALSE ;
  else {
  mu__boolexpr65 = ((mu_repRule) == (mu_LNCFR)) ; 
}
  if (!(mu__boolexpr65)) mu__boolexpr64 = FALSE ;
  else {
  mu__boolexpr64 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr64)) mu__boolexpr63 = FALSE ;
  else {
  mu__boolexpr63 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr63)) mu__boolexpr62 = FALSE ;
  else {
  mu__boolexpr62 = ((mu_a) == (mu_curMemory)) ; 
}
  if (!(mu__boolexpr62)) mu__boolexpr61 = FALSE ;
  else {
  mu__boolexpr61 = ((mu_l) == (mu_curLock)) ; 
}
    return mu__boolexpr61;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 60;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 72 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr66;
bool mu__boolexpr67;
bool mu__boolexpr68;
bool mu__boolexpr69;
bool mu__boolexpr70;
  if (!((mu_replace) == (mu_DONE))) mu__boolexpr70 = FALSE ;
  else {
  mu__boolexpr70 = ((mu_repRule) == (mu_LNCFR)) ; 
}
  if (!(mu__boolexpr70)) mu__boolexpr69 = FALSE ;
  else {
  mu__boolexpr69 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr69)) mu__boolexpr68 = FALSE ;
  else {
  mu__boolexpr68 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr68)) mu__boolexpr67 = FALSE ;
  else {
  mu__boolexpr67 = ((mu_a) == (mu_curMemory)) ; 
}
  if (!(mu__boolexpr67)) mu__boolexpr66 = FALSE ;
  else {
  mu__boolexpr66 = ((mu_l) == (mu_curLock)) ; 
}
	      if (mu__boolexpr66) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 60;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_node[mu_i].mu_cache[mu_j].mu_addr = mu_a;
mu_node[mu_i].mu_cache[mu_j].mu_data = mu_memory[mu_a].mu_data;
mu_node[mu_i].mu_cache[mu_j].mu_state = mu_VALID;
mu_node[mu_i].mu_firstRead[mu_a] = mu_false;
mu_replace = mu_NON;
mu_repRule = mu_NONE;
  };

};
/******************** RuleBase7 ********************/
class RuleBase7
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("LNCFRR, l:%s, a:%s, i:%s", mu_l.Name(), mu_a.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr71;
bool mu__boolexpr72;
bool mu__boolexpr73;
bool mu__boolexpr74;
bool mu__boolexpr75;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr75 = FALSE ;
  else {
  mu__boolexpr75 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr75)) mu__boolexpr74 = FALSE ;
  else {
  mu__boolexpr74 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr74)) mu__boolexpr73 = FALSE ;
  else {
  mu__boolexpr73 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
  if (!(mu__boolexpr73)) mu__boolexpr72 = FALSE ;
  else {
  mu__boolexpr72 = (mu_node[mu_i].mu_firstRead[mu_a]) ; 
}
  if (!(mu__boolexpr72)) mu__boolexpr71 = FALSE ;
  else {
bool mu__quant76; 
mu__quant76 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
bool mu__boolexpr77;
  if ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) mu__boolexpr77 = TRUE ;
  else {
  mu__boolexpr77 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) != (mu_a)) ; 
}
if ( !(mu__boolexpr77) )
  { mu__quant76 = FALSE; break; }
};
};
  mu__boolexpr71 = (mu__quant76) ; 
}
    return mu__boolexpr71;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 72;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 78 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr78;
bool mu__boolexpr79;
bool mu__boolexpr80;
bool mu__boolexpr81;
bool mu__boolexpr82;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr82 = FALSE ;
  else {
  mu__boolexpr82 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr82)) mu__boolexpr81 = FALSE ;
  else {
  mu__boolexpr81 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr81)) mu__boolexpr80 = FALSE ;
  else {
  mu__boolexpr80 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
  if (!(mu__boolexpr80)) mu__boolexpr79 = FALSE ;
  else {
  mu__boolexpr79 = (mu_node[mu_i].mu_firstRead[mu_a]) ; 
}
  if (!(mu__boolexpr79)) mu__boolexpr78 = FALSE ;
  else {
bool mu__quant83; 
mu__quant83 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
bool mu__boolexpr84;
  if ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) mu__boolexpr84 = TRUE ;
  else {
  mu__boolexpr84 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) != (mu_a)) ; 
}
if ( !(mu__boolexpr84) )
  { mu__quant83 = FALSE; break; }
};
};
  mu__boolexpr78 = (mu__quant83) ; 
}
	      if (mu__boolexpr78) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 72;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_curNode = mu_i;
mu_curMemory = mu_a;
mu_curLock = mu_l;
mu_replace = mu_REQUIRE;
mu_repRule = mu_LNCFR;
  };

};
/******************** RuleBase8 ********************/
class RuleBase8
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("LCFRD, l:%s, a:%s, j:%s, i:%s", mu_l.Name(), mu_a.Name(), mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr85;
bool mu__boolexpr86;
bool mu__boolexpr87;
bool mu__boolexpr88;
bool mu__boolexpr89;
  if (!((mu_replace) == (mu_DONE))) mu__boolexpr89 = FALSE ;
  else {
  mu__boolexpr89 = ((mu_repRule) == (mu_LCFR)) ; 
}
  if (!(mu__boolexpr89)) mu__boolexpr88 = FALSE ;
  else {
  mu__boolexpr88 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr88)) mu__boolexpr87 = FALSE ;
  else {
  mu__boolexpr87 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr87)) mu__boolexpr86 = FALSE ;
  else {
  mu__boolexpr86 = ((mu_a) == (mu_curMemory)) ; 
}
  if (!(mu__boolexpr86)) mu__boolexpr85 = FALSE ;
  else {
  mu__boolexpr85 = ((mu_l) == (mu_curLock)) ; 
}
    return mu__boolexpr85;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 78;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 90 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr90;
bool mu__boolexpr91;
bool mu__boolexpr92;
bool mu__boolexpr93;
bool mu__boolexpr94;
  if (!((mu_replace) == (mu_DONE))) mu__boolexpr94 = FALSE ;
  else {
  mu__boolexpr94 = ((mu_repRule) == (mu_LCFR)) ; 
}
  if (!(mu__boolexpr94)) mu__boolexpr93 = FALSE ;
  else {
  mu__boolexpr93 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr93)) mu__boolexpr92 = FALSE ;
  else {
  mu__boolexpr92 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr92)) mu__boolexpr91 = FALSE ;
  else {
  mu__boolexpr91 = ((mu_a) == (mu_curMemory)) ; 
}
  if (!(mu__boolexpr91)) mu__boolexpr90 = FALSE ;
  else {
  mu__boolexpr90 = ((mu_l) == (mu_curLock)) ; 
}
	      if (mu__boolexpr90) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 78;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_node[mu_i].mu_cache[mu_j].mu_data = mu_memory[mu_a].mu_data;
mu_node[mu_i].mu_cache[mu_j].mu_state = mu_VALID;
mu_node[mu_i].mu_firstRead[mu_a] = mu_false;
mu_replace = mu_NON;
mu_repRule = mu_NONE;
  };

};
/******************** RuleBase9 ********************/
class RuleBase9
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("LCFRR, l:%s, a:%s, j:%s, i:%s", mu_l.Name(), mu_a.Name(), mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr95;
bool mu__boolexpr96;
bool mu__boolexpr97;
bool mu__boolexpr98;
bool mu__boolexpr99;
bool mu__boolexpr100;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr100 = FALSE ;
  else {
  mu__boolexpr100 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr100)) mu__boolexpr99 = FALSE ;
  else {
  mu__boolexpr99 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr99)) mu__boolexpr98 = FALSE ;
  else {
  mu__boolexpr98 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
  if (!(mu__boolexpr98)) mu__boolexpr97 = FALSE ;
  else {
  mu__boolexpr97 = (mu_node[mu_i].mu_firstRead[mu_a]) ; 
}
  if (!(mu__boolexpr97)) mu__boolexpr96 = FALSE ;
  else {
  mu__boolexpr96 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) != (mu_INVALID)) ; 
}
  if (!(mu__boolexpr96)) mu__boolexpr95 = FALSE ;
  else {
  mu__boolexpr95 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) == (mu_a)) ; 
}
    return mu__boolexpr95;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 90;
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 102 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr101;
bool mu__boolexpr102;
bool mu__boolexpr103;
bool mu__boolexpr104;
bool mu__boolexpr105;
bool mu__boolexpr106;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr106 = FALSE ;
  else {
  mu__boolexpr106 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr106)) mu__boolexpr105 = FALSE ;
  else {
  mu__boolexpr105 = (mu_lock[mu_l].mu_beUsed) ; 
}
  if (!(mu__boolexpr105)) mu__boolexpr104 = FALSE ;
  else {
  mu__boolexpr104 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
  if (!(mu__boolexpr104)) mu__boolexpr103 = FALSE ;
  else {
  mu__boolexpr103 = (mu_node[mu_i].mu_firstRead[mu_a]) ; 
}
  if (!(mu__boolexpr103)) mu__boolexpr102 = FALSE ;
  else {
  mu__boolexpr102 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) != (mu_INVALID)) ; 
}
  if (!(mu__boolexpr102)) mu__boolexpr101 = FALSE ;
  else {
  mu__boolexpr101 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) == (mu_a)) ; 
}
	      if (mu__boolexpr101) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 90;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_LOCK mu_l;
    mu_l.value((r % 1) + 1);
    r = r / 1;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_curNode = mu_i;
mu_curCache = mu_j;
mu_curMemory = mu_a;
mu_curLock = mu_l;
mu_replace = mu_DESIGNATED;
mu_repRule = mu_LCFR;
  };

};
/******************** RuleBase10 ********************/
class RuleBase10
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("NLNCWD, d:%s, a:%s, j:%s, i:%s", mu_d.Name(), mu_a.Name(), mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr107;
bool mu__boolexpr108;
bool mu__boolexpr109;
bool mu__boolexpr110;
bool mu__boolexpr111;
  if (!((mu_replace) == (mu_DONE))) mu__boolexpr111 = FALSE ;
  else {
  mu__boolexpr111 = ((mu_repRule) == (mu_NLNCW)) ; 
}
  if (!(mu__boolexpr111)) mu__boolexpr110 = FALSE ;
  else {
  mu__boolexpr110 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr110)) mu__boolexpr109 = FALSE ;
  else {
  mu__boolexpr109 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr109)) mu__boolexpr108 = FALSE ;
  else {
  mu__boolexpr108 = ((mu_a) == (mu_curMemory)) ; 
}
  if (!(mu__boolexpr108)) mu__boolexpr107 = FALSE ;
  else {
  mu__boolexpr107 = ((mu_d) == (mu_curData)) ; 
}
    return mu__boolexpr107;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 102;
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 126 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr112;
bool mu__boolexpr113;
bool mu__boolexpr114;
bool mu__boolexpr115;
bool mu__boolexpr116;
  if (!((mu_replace) == (mu_DONE))) mu__boolexpr116 = FALSE ;
  else {
  mu__boolexpr116 = ((mu_repRule) == (mu_NLNCW)) ; 
}
  if (!(mu__boolexpr116)) mu__boolexpr115 = FALSE ;
  else {
  mu__boolexpr115 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr115)) mu__boolexpr114 = FALSE ;
  else {
  mu__boolexpr114 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr114)) mu__boolexpr113 = FALSE ;
  else {
  mu__boolexpr113 = ((mu_a) == (mu_curMemory)) ; 
}
  if (!(mu__boolexpr113)) mu__boolexpr112 = FALSE ;
  else {
  mu__boolexpr112 = ((mu_d) == (mu_curData)) ; 
}
	      if (mu__boolexpr112) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 102;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_node[mu_i].mu_cache[mu_j].mu_addr = mu_a;
mu_node[mu_i].mu_cache[mu_j].mu_data = mu_d;
mu_node[mu_i].mu_cache[mu_j].mu_state = mu_DIRTY;
mu_replace = mu_NON;
mu_repRule = mu_NONE;
  };

};
/******************** RuleBase11 ********************/
class RuleBase11
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("NLNCWR, d:%s, a:%s, i:%s", mu_d.Name(), mu_a.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr117;
bool mu__boolexpr118;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr118 = FALSE ;
  else {
  mu__boolexpr118 = (!(mu_node[mu_i].mu_hasLock)) ; 
}
  if (!(mu__boolexpr118)) mu__boolexpr117 = FALSE ;
  else {
bool mu__quant119; 
mu__quant119 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
bool mu__boolexpr120;
  if ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) mu__boolexpr120 = TRUE ;
  else {
  mu__boolexpr120 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) != (mu_a)) ; 
}
if ( !(mu__boolexpr120) )
  { mu__quant119 = FALSE; break; }
};
};
  mu__boolexpr117 = (mu__quant119) ; 
}
    return mu__boolexpr117;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 126;
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 138 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr121;
bool mu__boolexpr122;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr122 = FALSE ;
  else {
  mu__boolexpr122 = (!(mu_node[mu_i].mu_hasLock)) ; 
}
  if (!(mu__boolexpr122)) mu__boolexpr121 = FALSE ;
  else {
bool mu__quant123; 
mu__quant123 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
bool mu__boolexpr124;
  if ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) mu__boolexpr124 = TRUE ;
  else {
  mu__boolexpr124 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) != (mu_a)) ; 
}
if ( !(mu__boolexpr124) )
  { mu__quant123 = FALSE; break; }
};
};
  mu__boolexpr121 = (mu__quant123) ; 
}
	      if (mu__boolexpr121) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 2;
	}
	else
	  what_rule += 2;
    r = what_rule - 126;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_curNode = mu_i;
mu_curMemory = mu_a;
mu_curData = mu_d;
mu_replace = mu_REQUIRE;
mu_repRule = mu_NLNCW;
  };

};
/******************** RuleBase12 ********************/
class RuleBase12
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("NLCW, d:%s, a:%s, j:%s, i:%s", mu_d.Name(), mu_a.Name(), mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr125;
bool mu__boolexpr126;
bool mu__boolexpr127;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr127 = FALSE ;
  else {
  mu__boolexpr127 = (!(mu_node[mu_i].mu_hasLock)) ; 
}
  if (!(mu__boolexpr127)) mu__boolexpr126 = FALSE ;
  else {
  mu__boolexpr126 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) != (mu_INVALID)) ; 
}
  if (!(mu__boolexpr126)) mu__boolexpr125 = FALSE ;
  else {
  mu__boolexpr125 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) == (mu_a)) ; 
}
    return mu__boolexpr125;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 138;
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 162 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr128;
bool mu__boolexpr129;
bool mu__boolexpr130;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr130 = FALSE ;
  else {
  mu__boolexpr130 = (!(mu_node[mu_i].mu_hasLock)) ; 
}
  if (!(mu__boolexpr130)) mu__boolexpr129 = FALSE ;
  else {
  mu__boolexpr129 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) != (mu_INVALID)) ; 
}
  if (!(mu__boolexpr129)) mu__boolexpr128 = FALSE ;
  else {
  mu__boolexpr128 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) == (mu_a)) ; 
}
	      if (mu__boolexpr128) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 2;
	}
	else
	  what_rule += 2;
    r = what_rule - 138;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_node[mu_i].mu_cache[mu_j].mu_data = mu_d;
mu_node[mu_i].mu_cache[mu_j].mu_state = mu_DIRTY;
  };

};
/******************** RuleBase13 ********************/
class RuleBase13
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("NLNCRD, a:%s, j:%s, i:%s", mu_a.Name(), mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr131;
bool mu__boolexpr132;
bool mu__boolexpr133;
bool mu__boolexpr134;
  if (!((mu_replace) == (mu_DONE))) mu__boolexpr134 = FALSE ;
  else {
  mu__boolexpr134 = ((mu_repRule) == (mu_NLNCR)) ; 
}
  if (!(mu__boolexpr134)) mu__boolexpr133 = FALSE ;
  else {
  mu__boolexpr133 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr133)) mu__boolexpr132 = FALSE ;
  else {
  mu__boolexpr132 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr132)) mu__boolexpr131 = FALSE ;
  else {
  mu__boolexpr131 = ((mu_a) == (mu_curMemory)) ; 
}
    return mu__boolexpr131;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 162;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 174 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr135;
bool mu__boolexpr136;
bool mu__boolexpr137;
bool mu__boolexpr138;
  if (!((mu_replace) == (mu_DONE))) mu__boolexpr138 = FALSE ;
  else {
  mu__boolexpr138 = ((mu_repRule) == (mu_NLNCR)) ; 
}
  if (!(mu__boolexpr138)) mu__boolexpr137 = FALSE ;
  else {
  mu__boolexpr137 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr137)) mu__boolexpr136 = FALSE ;
  else {
  mu__boolexpr136 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr136)) mu__boolexpr135 = FALSE ;
  else {
  mu__boolexpr135 = ((mu_a) == (mu_curMemory)) ; 
}
	      if (mu__boolexpr135) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 162;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_node[mu_i].mu_cache[mu_j].mu_addr = mu_a;
mu_node[mu_i].mu_cache[mu_j].mu_data = mu_memory[mu_a].mu_data;
mu_node[mu_i].mu_cache[mu_j].mu_state = mu_VALID;
mu_replace = mu_NON;
mu_repRule = mu_NONE;
  };

};
/******************** RuleBase14 ********************/
class RuleBase14
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("NLNCRR, a:%s, i:%s", mu_a.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr139;
bool mu__boolexpr140;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr140 = FALSE ;
  else {
  mu__boolexpr140 = (!(mu_node[mu_i].mu_hasLock)) ; 
}
  if (!(mu__boolexpr140)) mu__boolexpr139 = FALSE ;
  else {
bool mu__quant141; 
mu__quant141 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
bool mu__boolexpr142;
  if ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) mu__boolexpr142 = TRUE ;
  else {
  mu__boolexpr142 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) != (mu_a)) ; 
}
if ( !(mu__boolexpr142) )
  { mu__quant141 = FALSE; break; }
};
};
  mu__boolexpr139 = (mu__quant141) ; 
}
    return mu__boolexpr139;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 174;
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 180 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr143;
bool mu__boolexpr144;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr144 = FALSE ;
  else {
  mu__boolexpr144 = (!(mu_node[mu_i].mu_hasLock)) ; 
}
  if (!(mu__boolexpr144)) mu__boolexpr143 = FALSE ;
  else {
bool mu__quant145; 
mu__quant145 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
bool mu__boolexpr146;
  if ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) mu__boolexpr146 = TRUE ;
  else {
  mu__boolexpr146 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) != (mu_a)) ; 
}
if ( !(mu__boolexpr146) )
  { mu__quant145 = FALSE; break; }
};
};
  mu__boolexpr143 = (mu__quant145) ; 
}
	      if (mu__boolexpr143) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 174;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_ADDR mu_a;
    mu_a.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_curNode = mu_i;
mu_curMemory = mu_a;
mu_replace = mu_REQUIRE;
mu_repRule = mu_NLNCR;
  };

};
/******************** RuleBase15 ********************/
class RuleBase15
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_ADDR mu_m;
    mu_m.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("Replace, m:%s, j:%s, i:%s", mu_m.Name(), mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_ADDR mu_m;
    mu_m.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr147;
bool mu__boolexpr148;
bool mu__boolexpr149;
  if (!((mu_replace) == (mu_TOREP))) mu__boolexpr149 = FALSE ;
  else {
  mu__boolexpr149 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr149)) mu__boolexpr148 = FALSE ;
  else {
  mu__boolexpr148 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr148)) mu__boolexpr147 = FALSE ;
  else {
  mu__boolexpr147 = ((mu_m) == (mu_node[mu_i].mu_cache[mu_j].mu_addr)) ; 
}
    return mu__boolexpr147;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 180;
    static mu_1_TYPE_ADDR mu_m;
    mu_m.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 192 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr150;
bool mu__boolexpr151;
bool mu__boolexpr152;
  if (!((mu_replace) == (mu_TOREP))) mu__boolexpr152 = FALSE ;
  else {
  mu__boolexpr152 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr152)) mu__boolexpr151 = FALSE ;
  else {
  mu__boolexpr151 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr151)) mu__boolexpr150 = FALSE ;
  else {
  mu__boolexpr150 = ((mu_m) == (mu_node[mu_i].mu_cache[mu_j].mu_addr)) ; 
}
	      if (mu__boolexpr150) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 180;
    mu_m.value((r % 2) + 1);
    r = r / 2;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_ADDR mu_m;
    mu_m.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_memory[mu_m].mu_data = mu_node[mu_i].mu_cache[mu_j].mu_data;
mu_node[mu_i].mu_cache[mu_j].mu_state = mu_INVALID;
mu_replace = mu_DONE;
  };

};
/******************** RuleBase16 ********************/
class RuleBase16
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("DCD, j:%s, i:%s", mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr153;
bool mu__boolexpr154;
bool mu__boolexpr155;
  if (!((mu_replace) == (mu_DESIGNATED))) mu__boolexpr155 = FALSE ;
  else {
  mu__boolexpr155 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr155)) mu__boolexpr154 = FALSE ;
  else {
  mu__boolexpr154 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr154)) mu__boolexpr153 = FALSE ;
  else {
  mu__boolexpr153 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_DIRTY)) ; 
}
    return mu__boolexpr153;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 192;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 198 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr156;
bool mu__boolexpr157;
bool mu__boolexpr158;
  if (!((mu_replace) == (mu_DESIGNATED))) mu__boolexpr158 = FALSE ;
  else {
  mu__boolexpr158 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr158)) mu__boolexpr157 = FALSE ;
  else {
  mu__boolexpr157 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr157)) mu__boolexpr156 = FALSE ;
  else {
  mu__boolexpr156 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_DIRTY)) ; 
}
	      if (mu__boolexpr156) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 192;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_replace = mu_TOREP;
  };

};
/******************** RuleBase17 ********************/
class RuleBase17
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("DCND, j:%s, i:%s", mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr159;
bool mu__boolexpr160;
bool mu__boolexpr161;
  if (!((mu_replace) == (mu_DESIGNATED))) mu__boolexpr161 = FALSE ;
  else {
  mu__boolexpr161 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr161)) mu__boolexpr160 = FALSE ;
  else {
  mu__boolexpr160 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr160)) mu__boolexpr159 = FALSE ;
  else {
  mu__boolexpr159 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) != (mu_DIRTY)) ; 
}
    return mu__boolexpr159;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 198;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 204 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr162;
bool mu__boolexpr163;
bool mu__boolexpr164;
  if (!((mu_replace) == (mu_DESIGNATED))) mu__boolexpr164 = FALSE ;
  else {
  mu__boolexpr164 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr164)) mu__boolexpr163 = FALSE ;
  else {
  mu__boolexpr163 = ((mu_j) == (mu_curCache)) ; 
}
  if (!(mu__boolexpr163)) mu__boolexpr162 = FALSE ;
  else {
  mu__boolexpr162 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) != (mu_DIRTY)) ; 
}
	      if (mu__boolexpr162) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 198;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_replace = mu_DONE;
  };

};
/******************** RuleBase18 ********************/
class RuleBase18
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_i;
    mu_i.value((r % 2) + 1);
    r = r / 2;
    return tsprintf("CRC, i:%s", mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_i;
    mu_i.value((r % 2) + 1);
    r = r / 2;
    return (mu_replace) == (mu_RANDOM);
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 204;
    static mu_1_TYPE_CACHE mu_i;
    mu_i.value((r % 2) + 1);
    r = r / 2;
    while (what_rule < 206 )
      {
	if ( ( TRUE  ) ) {
	      if ((mu_replace) == (mu_RANDOM)) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 2;
	}
	else
	  what_rule += 2;
    r = what_rule - 204;
    mu_i.value((r % 2) + 1);
    r = r / 2;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_i;
    mu_i.value((r % 2) + 1);
    r = r / 2;
mu_curCache = mu_i;
mu_replace = mu_DESIGNATED;
  };

};
/******************** RuleBase19 ********************/
class RuleBase19
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("RNI, i:%s", mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr165;
bool mu__boolexpr166;
  if (!((mu_replace) == (mu_REQUIRE))) mu__boolexpr166 = FALSE ;
  else {
  mu__boolexpr166 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr166)) mu__boolexpr165 = FALSE ;
  else {
bool mu__quant167; 
mu__quant167 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
if ( !((mu_node[mu_i].mu_cache[mu_j].mu_state) != (mu_INVALID)) )
  { mu__quant167 = FALSE; break; }
};
};
  mu__boolexpr165 = (mu__quant167) ; 
}
    return mu__boolexpr165;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 206;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 209 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr168;
bool mu__boolexpr169;
  if (!((mu_replace) == (mu_REQUIRE))) mu__boolexpr169 = FALSE ;
  else {
  mu__boolexpr169 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr169)) mu__boolexpr168 = FALSE ;
  else {
bool mu__quant170; 
mu__quant170 = TRUE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
if ( !((mu_node[mu_i].mu_cache[mu_j].mu_state) != (mu_INVALID)) )
  { mu__quant170 = FALSE; break; }
};
};
  mu__boolexpr168 = (mu__quant170) ; 
}
	      if (mu__boolexpr168) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 206;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_replace = mu_RANDOM;
  };

};
/******************** RuleBase20 ********************/
class RuleBase20
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("CRIC, j:%s, i:%s", mu_j.Name(), mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr171;
bool mu__boolexpr172;
  if (!((mu_replace) == (mu_RANDINV))) mu__boolexpr172 = FALSE ;
  else {
  mu__boolexpr172 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr172)) mu__boolexpr171 = FALSE ;
  else {
  mu__boolexpr171 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) ; 
}
    return mu__boolexpr171;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 209;
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 215 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr173;
bool mu__boolexpr174;
  if (!((mu_replace) == (mu_RANDINV))) mu__boolexpr174 = FALSE ;
  else {
  mu__boolexpr174 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr174)) mu__boolexpr173 = FALSE ;
  else {
  mu__boolexpr173 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) ; 
}
	      if (mu__boolexpr173) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 209;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_CACHE mu_j;
    mu_j.value((r % 2) + 1);
    r = r / 2;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_curCache = mu_j;
mu_replace = mu_DONE;
  };

};
/******************** RuleBase21 ********************/
class RuleBase21
{
public:
  int Priority()
  {
    return 0;
  }
  char * Name(unsigned r)
  {
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    return tsprintf("RI, i:%s", mu_i.Name());
  }
  bool Condition(unsigned r)
  {
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
bool mu__boolexpr175;
bool mu__boolexpr176;
  if (!((mu_replace) == (mu_REQUIRE))) mu__boolexpr176 = FALSE ;
  else {
  mu__boolexpr176 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr176)) mu__boolexpr175 = FALSE ;
  else {
bool mu__quant177; 
mu__quant177 = FALSE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
if ( ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) )
  { mu__quant177 = TRUE; break; }
};
};
  mu__boolexpr175 = (mu__quant177) ; 
}
    return mu__boolexpr175;
  }

  void NextRule(unsigned & what_rule)
  {
    unsigned r = what_rule - 215;
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    while (what_rule < 218 )
      {
	if ( ( TRUE  ) ) {
bool mu__boolexpr178;
bool mu__boolexpr179;
  if (!((mu_replace) == (mu_REQUIRE))) mu__boolexpr179 = FALSE ;
  else {
  mu__boolexpr179 = ((mu_i) == (mu_curNode)) ; 
}
  if (!(mu__boolexpr179)) mu__boolexpr178 = FALSE ;
  else {
bool mu__quant180; 
mu__quant180 = FALSE;
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
if ( ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_INVALID)) )
  { mu__quant180 = TRUE; break; }
};
};
  mu__boolexpr178 = (mu__quant180) ; 
}
	      if (mu__boolexpr178) {
		if ( ( TRUE  ) )
		  return;
		else
		  what_rule++;
	      }
	      else
		what_rule += 1;
	}
	else
	  what_rule += 1;
    r = what_rule - 215;
    mu_i.value((r % 3) + 1);
    r = r / 3;
    }
  }

  void Code(unsigned r)
  {
    static mu_1_TYPE_NODE mu_i;
    mu_i.value((r % 3) + 1);
    r = r / 3;
mu_replace = mu_RANDINV;
  };

};
class NextStateGenerator
{
  RuleBase0 R0;
  RuleBase1 R1;
  RuleBase2 R2;
  RuleBase3 R3;
  RuleBase4 R4;
  RuleBase5 R5;
  RuleBase6 R6;
  RuleBase7 R7;
  RuleBase8 R8;
  RuleBase9 R9;
  RuleBase10 R10;
  RuleBase11 R11;
  RuleBase12 R12;
  RuleBase13 R13;
  RuleBase14 R14;
  RuleBase15 R15;
  RuleBase16 R16;
  RuleBase17 R17;
  RuleBase18 R18;
  RuleBase19 R19;
  RuleBase20 R20;
  RuleBase21 R21;
public:
void SetNextEnabledRule(unsigned & what_rule)
{
  category = CONDITION;
  if (what_rule<3)
    { R0.NextRule(what_rule);
      if (what_rule<3) return; }
  if (what_rule>=3 && what_rule<6)
    { R1.NextRule(what_rule);
      if (what_rule<6) return; }
  if (what_rule>=6 && what_rule<18)
    { R2.NextRule(what_rule);
      if (what_rule<18) return; }
  if (what_rule>=18 && what_rule<42)
    { R3.NextRule(what_rule);
      if (what_rule<42) return; }
  if (what_rule>=42 && what_rule<54)
    { R4.NextRule(what_rule);
      if (what_rule<54) return; }
  if (what_rule>=54 && what_rule<60)
    { R5.NextRule(what_rule);
      if (what_rule<60) return; }
  if (what_rule>=60 && what_rule<72)
    { R6.NextRule(what_rule);
      if (what_rule<72) return; }
  if (what_rule>=72 && what_rule<78)
    { R7.NextRule(what_rule);
      if (what_rule<78) return; }
  if (what_rule>=78 && what_rule<90)
    { R8.NextRule(what_rule);
      if (what_rule<90) return; }
  if (what_rule>=90 && what_rule<102)
    { R9.NextRule(what_rule);
      if (what_rule<102) return; }
  if (what_rule>=102 && what_rule<126)
    { R10.NextRule(what_rule);
      if (what_rule<126) return; }
  if (what_rule>=126 && what_rule<138)
    { R11.NextRule(what_rule);
      if (what_rule<138) return; }
  if (what_rule>=138 && what_rule<162)
    { R12.NextRule(what_rule);
      if (what_rule<162) return; }
  if (what_rule>=162 && what_rule<174)
    { R13.NextRule(what_rule);
      if (what_rule<174) return; }
  if (what_rule>=174 && what_rule<180)
    { R14.NextRule(what_rule);
      if (what_rule<180) return; }
  if (what_rule>=180 && what_rule<192)
    { R15.NextRule(what_rule);
      if (what_rule<192) return; }
  if (what_rule>=192 && what_rule<198)
    { R16.NextRule(what_rule);
      if (what_rule<198) return; }
  if (what_rule>=198 && what_rule<204)
    { R17.NextRule(what_rule);
      if (what_rule<204) return; }
  if (what_rule>=204 && what_rule<206)
    { R18.NextRule(what_rule);
      if (what_rule<206) return; }
  if (what_rule>=206 && what_rule<209)
    { R19.NextRule(what_rule);
      if (what_rule<209) return; }
  if (what_rule>=209 && what_rule<215)
    { R20.NextRule(what_rule);
      if (what_rule<215) return; }
  if (what_rule>=215 && what_rule<218)
    { R21.NextRule(what_rule);
      if (what_rule<218) return; }
}
bool Condition(unsigned r)
{
  category = CONDITION;
  if (r<=2) return R0.Condition(r-0);
  if (r>=3 && r<=5) return R1.Condition(r-3);
  if (r>=6 && r<=17) return R2.Condition(r-6);
  if (r>=18 && r<=41) return R3.Condition(r-18);
  if (r>=42 && r<=53) return R4.Condition(r-42);
  if (r>=54 && r<=59) return R5.Condition(r-54);
  if (r>=60 && r<=71) return R6.Condition(r-60);
  if (r>=72 && r<=77) return R7.Condition(r-72);
  if (r>=78 && r<=89) return R8.Condition(r-78);
  if (r>=90 && r<=101) return R9.Condition(r-90);
  if (r>=102 && r<=125) return R10.Condition(r-102);
  if (r>=126 && r<=137) return R11.Condition(r-126);
  if (r>=138 && r<=161) return R12.Condition(r-138);
  if (r>=162 && r<=173) return R13.Condition(r-162);
  if (r>=174 && r<=179) return R14.Condition(r-174);
  if (r>=180 && r<=191) return R15.Condition(r-180);
  if (r>=192 && r<=197) return R16.Condition(r-192);
  if (r>=198 && r<=203) return R17.Condition(r-198);
  if (r>=204 && r<=205) return R18.Condition(r-204);
  if (r>=206 && r<=208) return R19.Condition(r-206);
  if (r>=209 && r<=214) return R20.Condition(r-209);
  if (r>=215 && r<=217) return R21.Condition(r-215);
Error.Notrace("Internal: NextStateGenerator -- checking condition for nonexisting rule.");
return 0;}
void Code(unsigned r)
{
  if (r<=2) { R0.Code(r-0); return; } 
  if (r>=3 && r<=5) { R1.Code(r-3); return; } 
  if (r>=6 && r<=17) { R2.Code(r-6); return; } 
  if (r>=18 && r<=41) { R3.Code(r-18); return; } 
  if (r>=42 && r<=53) { R4.Code(r-42); return; } 
  if (r>=54 && r<=59) { R5.Code(r-54); return; } 
  if (r>=60 && r<=71) { R6.Code(r-60); return; } 
  if (r>=72 && r<=77) { R7.Code(r-72); return; } 
  if (r>=78 && r<=89) { R8.Code(r-78); return; } 
  if (r>=90 && r<=101) { R9.Code(r-90); return; } 
  if (r>=102 && r<=125) { R10.Code(r-102); return; } 
  if (r>=126 && r<=137) { R11.Code(r-126); return; } 
  if (r>=138 && r<=161) { R12.Code(r-138); return; } 
  if (r>=162 && r<=173) { R13.Code(r-162); return; } 
  if (r>=174 && r<=179) { R14.Code(r-174); return; } 
  if (r>=180 && r<=191) { R15.Code(r-180); return; } 
  if (r>=192 && r<=197) { R16.Code(r-192); return; } 
  if (r>=198 && r<=203) { R17.Code(r-198); return; } 
  if (r>=204 && r<=205) { R18.Code(r-204); return; } 
  if (r>=206 && r<=208) { R19.Code(r-206); return; } 
  if (r>=209 && r<=214) { R20.Code(r-209); return; } 
  if (r>=215 && r<=217) { R21.Code(r-215); return; } 
}
int Priority(unsigned short r)
{
  if (r<=2) { return R0.Priority(); } 
  if (r>=3 && r<=5) { return R1.Priority(); } 
  if (r>=6 && r<=17) { return R2.Priority(); } 
  if (r>=18 && r<=41) { return R3.Priority(); } 
  if (r>=42 && r<=53) { return R4.Priority(); } 
  if (r>=54 && r<=59) { return R5.Priority(); } 
  if (r>=60 && r<=71) { return R6.Priority(); } 
  if (r>=72 && r<=77) { return R7.Priority(); } 
  if (r>=78 && r<=89) { return R8.Priority(); } 
  if (r>=90 && r<=101) { return R9.Priority(); } 
  if (r>=102 && r<=125) { return R10.Priority(); } 
  if (r>=126 && r<=137) { return R11.Priority(); } 
  if (r>=138 && r<=161) { return R12.Priority(); } 
  if (r>=162 && r<=173) { return R13.Priority(); } 
  if (r>=174 && r<=179) { return R14.Priority(); } 
  if (r>=180 && r<=191) { return R15.Priority(); } 
  if (r>=192 && r<=197) { return R16.Priority(); } 
  if (r>=198 && r<=203) { return R17.Priority(); } 
  if (r>=204 && r<=205) { return R18.Priority(); } 
  if (r>=206 && r<=208) { return R19.Priority(); } 
  if (r>=209 && r<=214) { return R20.Priority(); } 
  if (r>=215 && r<=217) { return R21.Priority(); } 
return 0;}
char * Name(unsigned r)
{
  if (r<=2) return R0.Name(r-0);
  if (r>=3 && r<=5) return R1.Name(r-3);
  if (r>=6 && r<=17) return R2.Name(r-6);
  if (r>=18 && r<=41) return R3.Name(r-18);
  if (r>=42 && r<=53) return R4.Name(r-42);
  if (r>=54 && r<=59) return R5.Name(r-54);
  if (r>=60 && r<=71) return R6.Name(r-60);
  if (r>=72 && r<=77) return R7.Name(r-72);
  if (r>=78 && r<=89) return R8.Name(r-78);
  if (r>=90 && r<=101) return R9.Name(r-90);
  if (r>=102 && r<=125) return R10.Name(r-102);
  if (r>=126 && r<=137) return R11.Name(r-126);
  if (r>=138 && r<=161) return R12.Name(r-138);
  if (r>=162 && r<=173) return R13.Name(r-162);
  if (r>=174 && r<=179) return R14.Name(r-174);
  if (r>=180 && r<=191) return R15.Name(r-180);
  if (r>=192 && r<=197) return R16.Name(r-192);
  if (r>=198 && r<=203) return R17.Name(r-198);
  if (r>=204 && r<=205) return R18.Name(r-204);
  if (r>=206 && r<=208) return R19.Name(r-206);
  if (r>=209 && r<=214) return R20.Name(r-209);
  if (r>=215 && r<=217) return R21.Name(r-215);
  return NULL;
}
};
const unsigned numrules = 218;

/********************
  parameter
 ********************/
#define RULES_IN_WORLD 218


/********************
  Startstate records
 ********************/
/******************** StartStateBase0 ********************/
class StartStateBase0
{
public:
  char * Name(unsigned short r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
    return tsprintf("Init, d:%s", mu_d.Name());
  }
  void Code(unsigned short r)
  {
    static mu_1_TYPE_DATA mu_d;
    mu_d.value((r % 2) + 1);
    r = r / 2;
{
for(int mu_i = 1; mu_i <= 3; mu_i++) {
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
mu_node[mu_i].mu_cache[mu_j].mu_state = mu_INVALID;
};
};
mu_node[mu_i].mu_hasLock = mu_false;
{
for(int mu_a = 1; mu_a <= 2; mu_a++) {
mu_node[mu_i].mu_firstRead[mu_a] = mu_true;
};
};
mu_curNode = mu_i;
};
};
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
mu_curCache = mu_j;
};
};
{
for(int mu_m = 1; mu_m <= 2; mu_m++) {
mu_memory[mu_m].mu_data = mu_d;
mu_curMemory = mu_d;
};
};
{
for(int mu_j = 1; mu_j <= 2; mu_j++) {
mu_curData = mu_j;
};
};
{
for(int mu_k = 1; mu_k <= 1; mu_k++) {
mu_lock[mu_k].mu_beUsed = mu_false;
mu_curLock = mu_k;
};
};
mu_replace = mu_NON;
mu_repRule = mu_NONE;
  };

};
class StartStateGenerator
{
  StartStateBase0 S0;
public:
void Code(unsigned short r)
{
  if (r<=1) { S0.Code(r-0); return; }
}
char * Name(unsigned short r)
{
  if (r<=1) return S0.Name(r-0);
  return NULL;
}
};
const rulerec startstates[] = {
{ NULL, NULL, NULL, FALSE},
};
unsigned short StartStateManager::numstartstates = 2;

/********************
  Invariant records
 ********************/
int mu__invariant_181( const mu_1_TYPE_ADDR &mu_a, const mu_1_TYPE_CACHE &mu_j, const mu_1_TYPE_NODE &mu_i) // Invariant "Coherence"
{
bool mu__boolexpr182;
bool mu__boolexpr183;
bool mu__boolexpr184;
bool mu__boolexpr185;
bool mu__boolexpr186;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr186 = FALSE ;
  else {
  mu__boolexpr186 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr186)) mu__boolexpr185 = FALSE ;
  else {
  mu__boolexpr185 = (!(mu_node[mu_i].mu_firstRead[mu_a])) ; 
}
  if (!(mu__boolexpr185)) mu__boolexpr184 = FALSE ;
  else {
  mu__boolexpr184 = ((mu_node[mu_i].mu_cache[mu_j].mu_state) == (mu_VALID)) ; 
}
  if (!(mu__boolexpr184)) mu__boolexpr183 = FALSE ;
  else {
  mu__boolexpr183 = ((mu_node[mu_i].mu_cache[mu_j].mu_addr) == (mu_a)) ; 
}
  if (!(mu__boolexpr183)) mu__boolexpr182 = TRUE ;
  else {
  mu__boolexpr182 = ((mu_node[mu_i].mu_cache[mu_j].mu_data) == (mu_memory[mu_a].mu_data)) ; 
}
return mu__boolexpr182;
};

bool mu__condition_190() // Condition for Rule "Coherence, a:1, j:1, i:1"
{
  return mu__invariant_181( 1, 1, 1 );
}

bool mu__condition_191() // Condition for Rule "Coherence, a:1, j:1, i:2"
{
  return mu__invariant_181( 1, 1, 2 );
}

bool mu__condition_192() // Condition for Rule "Coherence, a:1, j:1, i:3"
{
  return mu__invariant_181( 1, 1, 3 );
}

bool mu__condition_194() // Condition for Rule "Coherence, a:1, j:2, i:1"
{
  return mu__invariant_181( 1, 2, 1 );
}

bool mu__condition_195() // Condition for Rule "Coherence, a:1, j:2, i:2"
{
  return mu__invariant_181( 1, 2, 2 );
}

bool mu__condition_196() // Condition for Rule "Coherence, a:1, j:2, i:3"
{
  return mu__invariant_181( 1, 2, 3 );
}

bool mu__condition_199() // Condition for Rule "Coherence, a:2, j:1, i:1"
{
  return mu__invariant_181( 2, 1, 1 );
}

bool mu__condition_200() // Condition for Rule "Coherence, a:2, j:1, i:2"
{
  return mu__invariant_181( 2, 1, 2 );
}

bool mu__condition_201() // Condition for Rule "Coherence, a:2, j:1, i:3"
{
  return mu__invariant_181( 2, 1, 3 );
}

bool mu__condition_203() // Condition for Rule "Coherence, a:2, j:2, i:1"
{
  return mu__invariant_181( 2, 2, 1 );
}

bool mu__condition_204() // Condition for Rule "Coherence, a:2, j:2, i:2"
{
  return mu__invariant_181( 2, 2, 2 );
}

bool mu__condition_205() // Condition for Rule "Coherence, a:2, j:2, i:3"
{
  return mu__invariant_181( 2, 2, 3 );
}

/**** end rule declaration ****/

int mu__invariant_206( const mu_1_TYPE_NODE &mu_i) // Invariant "DeadlockFree"
{
bool mu__boolexpr207;
bool mu__boolexpr208;
  if (!((mu_replace) == (mu_NON))) mu__boolexpr208 = FALSE ;
  else {
  mu__boolexpr208 = (mu_node[mu_i].mu_hasLock) ; 
}
  if (!(mu__boolexpr208)) mu__boolexpr207 = TRUE ;
  else {
bool mu__boolexpr209;
bool mu__quant210; 
mu__quant210 = FALSE;
{
for(int mu_l = 1; mu_l <= 1; mu_l++) {
bool mu__boolexpr211;
  if (!(mu_lock[mu_l].mu_beUsed)) mu__boolexpr211 = FALSE ;
  else {
  mu__boolexpr211 = ((mu_lock[mu_l].mu_owner) == (mu_i)) ; 
}
if ( (mu__boolexpr211) )
  { mu__quant210 = TRUE; break; }
};
};
  if (!(mu__quant210)) mu__boolexpr209 = FALSE ;
  else {
bool mu__quant212; 
mu__quant212 = TRUE;
{
for(int mu_m = 1; mu_m <= 1; mu_m++) {
for(int mu_n = 1; mu_n <= 1; mu_n++) {
bool mu__boolexpr213;
bool mu__boolexpr214;
bool mu__boolexpr215;
bool mu__boolexpr216;
  if ((mu_m) == (mu_n)) mu__boolexpr216 = TRUE ;
  else {
  mu__boolexpr216 = (!(mu_lock[mu_m].mu_beUsed)) ; 
}
  if (mu__boolexpr216) mu__boolexpr215 = TRUE ;
  else {
  mu__boolexpr215 = (!(mu_lock[mu_n].mu_beUsed)) ; 
}
  if (mu__boolexpr215) mu__boolexpr214 = TRUE ;
  else {
  mu__boolexpr214 = ((mu_lock[mu_m].mu_owner) != (mu_i)) ; 
}
  if (mu__boolexpr214) mu__boolexpr213 = TRUE ;
  else {
  mu__boolexpr213 = ((mu_lock[mu_n].mu_owner) != (mu_i)) ; 
}
if ( !(mu__boolexpr213) )
  { mu__quant212 = FALSE; break; }
};
};
};
  mu__boolexpr209 = (mu__quant212) ; 
}
  mu__boolexpr207 = (mu__boolexpr209) ; 
}
return mu__boolexpr207;
};

bool mu__condition_218() // Condition for Rule "DeadlockFree, i:1"
{
  return mu__invariant_206( 1 );
}

bool mu__condition_219() // Condition for Rule "DeadlockFree, i:2"
{
  return mu__invariant_206( 2 );
}

bool mu__condition_220() // Condition for Rule "DeadlockFree, i:3"
{
  return mu__invariant_206( 3 );
}

/**** end rule declaration ****/

const rulerec invariants[] = {
{"DeadlockFree, i:3", &mu__condition_220, NULL, },
{"DeadlockFree, i:2", &mu__condition_219, NULL, },
{"DeadlockFree, i:1", &mu__condition_218, NULL, },
{"Coherence, a:2, j:2, i:3", &mu__condition_205, NULL, },
{"Coherence, a:2, j:2, i:2", &mu__condition_204, NULL, },
{"Coherence, a:2, j:2, i:1", &mu__condition_203, NULL, },
{"Coherence, a:2, j:1, i:3", &mu__condition_201, NULL, },
{"Coherence, a:2, j:1, i:2", &mu__condition_200, NULL, },
{"Coherence, a:2, j:1, i:1", &mu__condition_199, NULL, },
{"Coherence, a:1, j:2, i:3", &mu__condition_196, NULL, },
{"Coherence, a:1, j:2, i:2", &mu__condition_195, NULL, },
{"Coherence, a:1, j:2, i:1", &mu__condition_194, NULL, },
{"Coherence, a:1, j:1, i:3", &mu__condition_192, NULL, },
{"Coherence, a:1, j:1, i:2", &mu__condition_191, NULL, },
{"Coherence, a:1, j:1, i:1", &mu__condition_190, NULL, },
};
const unsigned short numinvariants = 15;

/********************
  Normal/Canonicalization for scalarset
 ********************/
/*
replace:NoScalarset
curData:NoScalarset
curCache:NoScalarset
node:NoScalarset
lock:NoScalarset
memory:NoScalarset
curNode:NoScalarset
curMemory:NoScalarset
curLock:NoScalarset
repRule:NoScalarset
*/

/********************
Code for symmetry
 ********************/

/********************
 Permutation Set Class
 ********************/
class PermSet
{
public:
  // book keeping
  enum PresentationType {Simple, Explicit};
  PresentationType Presentation;

  void ResetToSimple();
  void ResetToExplicit();
  void SimpleToExplicit();
  void SimpleToOne();
  bool NextPermutation();

  void Print_in_size()
  { int ret=0; for (int i=0; i<count; i++) if (in[i]) ret++; cout << "in_size:" << ret << "\n"; }


  /********************
   Simple and efficient representation
   ********************/
  bool AlreadyOnlyOneRemain;
  bool MoreThanOneRemain();


  /********************
   Explicit representation
  ********************/
  unsigned long size;
  unsigned long count;
  // in will be of product of factorial sizes for fast canonicalize
  // in will be of size 1 for reduced local memory canonicalize
  bool * in;

  // auxiliary for explicit representation

  // in/perm/revperm will be of factorial size for fast canonicalize
  // they will be of size 1 for reduced local memory canonicalize
  // second range will be size of the scalarset
  // procedure for explicit representation
  // General procedure
  PermSet();
  bool In(int i) const { return in[i]; };
  void Add(int i) { for (int j=0; j<i; j++) in[j] = FALSE;};
  void Remove(int i) { in[i] = FALSE; };
};
bool PermSet::MoreThanOneRemain()
{
  int i,j;
  if (AlreadyOnlyOneRemain)
    return FALSE;
  else {
  }
  AlreadyOnlyOneRemain = TRUE;
  return FALSE;
}
PermSet::PermSet()
: Presentation(Simple)
{
  int i,j,k;
  if (  args->sym_alg.mode == argsym_alg::Exhaustive_Fast_Canonicalize) {

  /********************
   declaration of class variables
  ********************/
  in = new bool[1];

    // Set perm and revperm

    // setting up combination of permutations
    // for different scalarset
    int carry;
    size = 1;
    count = 1;
    for (i=0; i<1; i++)
      {
        carry = 1;
        in[i]= TRUE;
    }
  }
  else
  {

  /********************
   declaration of class variables
  ********************/
  in = new bool[1];
  in[0] = TRUE;
  }
}
void PermSet::ResetToSimple()
{
  int i;

  AlreadyOnlyOneRemain = FALSE;
  Presentation = Simple;
}
void PermSet::ResetToExplicit()
{
  for (int i=0; i<1; i++) in[i] = TRUE;
  Presentation = Explicit;
}
void PermSet::SimpleToExplicit()
{
  int i,j,k;
  int start, class_size;

  // Setup range for mapping

  // To be In or not to be

  // setup explicit representation 
  // Set perm and revperm
  for (i=0; i<1; i++)
    {
      in[i] = TRUE;
    }
  Presentation = Explicit;
  if (args->test_parameter1.value==0) Print_in_size();
}
void PermSet::SimpleToOne()
{
  int i,j,k;
  int class_size;
  int start;


  // Setup range for mapping
  Presentation = Explicit;
}
bool PermSet::NextPermutation()
{
  bool nexted = FALSE;
  int start, end; 
  int class_size;
  int temp;
  int j,k;

  // algorithm
  // for each class
  //   if forall in the same class reverse_sorted, 
  //     { sort again; goto next class }
  //   else
  //     {
  //       nexted = TRUE;
  //       for (j from l to r)
  // 	       if (for all j+ are reversed sorted)
  // 	         {
  // 	           swap j, j+1
  // 	           sort all j+ again
  // 	           break;
  // 	         }
  //     }
if (!nexted) return FALSE;
  return TRUE;
}

/********************
 Symmetry Class
 ********************/
class SymmetryClass
{
  PermSet Perm;
  bool BestInitialized;
  state BestPermutedState;

  // utilities
  void SetBestResult(int i, state* temp);
  void ResetBestResult() {BestInitialized = FALSE;};

public:
  // initializer
  SymmetryClass() : Perm(), BestInitialized(FALSE) {};
  ~SymmetryClass() {};

  void Normalize(state* s);

  void Exhaustive_Fast_Canonicalize(state *s);
  void Heuristic_Fast_Canonicalize(state *s);
  void Heuristic_Small_Mem_Canonicalize(state *s);
  void Heuristic_Fast_Normalize(state *s);

  void MultisetSort(state* s);
};


/********************
 Symmetry Class Members
 ********************/
void SymmetryClass::MultisetSort(state* s)
{
        mu_replace.MultisetSort();
        mu_curData.MultisetSort();
        mu_curCache.MultisetSort();
        mu_node.MultisetSort();
        mu_lock.MultisetSort();
        mu_memory.MultisetSort();
        mu_curNode.MultisetSort();
        mu_curMemory.MultisetSort();
        mu_curLock.MultisetSort();
        mu_repRule.MultisetSort();
}
void SymmetryClass::Normalize(state* s)
{
  switch (args->sym_alg.mode) {
  case argsym_alg::Exhaustive_Fast_Canonicalize:
    Exhaustive_Fast_Canonicalize(s);
    break;
  case argsym_alg::Heuristic_Fast_Canonicalize:
    Heuristic_Fast_Canonicalize(s);
    break;
  case argsym_alg::Heuristic_Small_Mem_Canonicalize:
    Heuristic_Small_Mem_Canonicalize(s);
    break;
  case argsym_alg::Heuristic_Fast_Normalize:
    Heuristic_Fast_Normalize(s);
    break;
  default:
    Heuristic_Fast_Canonicalize(s);
  }
}

/********************
 Permute and Canonicalize function for different types
 ********************/
void mu_1_TYPE_NODE::Permute(PermSet& Perm, int i) {};
void mu_1_TYPE_NODE::SimpleCanonicalize(PermSet& Perm) {};
void mu_1_TYPE_NODE::Canonicalize(PermSet& Perm) {};
void mu_1_TYPE_NODE::SimpleLimit(PermSet& Perm) {};
void mu_1_TYPE_NODE::ArrayLimit(PermSet& Perm) {};
void mu_1_TYPE_NODE::Limit(PermSet& Perm) {};
void mu_1_TYPE_NODE::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for subrange type.\n"); };
void mu_1_TYPE_CACHE::Permute(PermSet& Perm, int i) {};
void mu_1_TYPE_CACHE::SimpleCanonicalize(PermSet& Perm) {};
void mu_1_TYPE_CACHE::Canonicalize(PermSet& Perm) {};
void mu_1_TYPE_CACHE::SimpleLimit(PermSet& Perm) {};
void mu_1_TYPE_CACHE::ArrayLimit(PermSet& Perm) {};
void mu_1_TYPE_CACHE::Limit(PermSet& Perm) {};
void mu_1_TYPE_CACHE::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for subrange type.\n"); };
void mu_1_TYPE_ADDR::Permute(PermSet& Perm, int i) {};
void mu_1_TYPE_ADDR::SimpleCanonicalize(PermSet& Perm) {};
void mu_1_TYPE_ADDR::Canonicalize(PermSet& Perm) {};
void mu_1_TYPE_ADDR::SimpleLimit(PermSet& Perm) {};
void mu_1_TYPE_ADDR::ArrayLimit(PermSet& Perm) {};
void mu_1_TYPE_ADDR::Limit(PermSet& Perm) {};
void mu_1_TYPE_ADDR::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for subrange type.\n"); };
void mu_1_TYPE_DATA::Permute(PermSet& Perm, int i) {};
void mu_1_TYPE_DATA::SimpleCanonicalize(PermSet& Perm) {};
void mu_1_TYPE_DATA::Canonicalize(PermSet& Perm) {};
void mu_1_TYPE_DATA::SimpleLimit(PermSet& Perm) {};
void mu_1_TYPE_DATA::ArrayLimit(PermSet& Perm) {};
void mu_1_TYPE_DATA::Limit(PermSet& Perm) {};
void mu_1_TYPE_DATA::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for subrange type.\n"); };
void mu_1_TYPE_LOCK::Permute(PermSet& Perm, int i) {};
void mu_1_TYPE_LOCK::SimpleCanonicalize(PermSet& Perm) {};
void mu_1_TYPE_LOCK::Canonicalize(PermSet& Perm) {};
void mu_1_TYPE_LOCK::SimpleLimit(PermSet& Perm) {};
void mu_1_TYPE_LOCK::ArrayLimit(PermSet& Perm) {};
void mu_1_TYPE_LOCK::Limit(PermSet& Perm) {};
void mu_1_TYPE_LOCK::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for subrange type.\n"); };
void mu_1_CACHE_STATE::Permute(PermSet& Perm, int i) {};
void mu_1_CACHE_STATE::SimpleCanonicalize(PermSet& Perm) {};
void mu_1_CACHE_STATE::Canonicalize(PermSet& Perm) {};
void mu_1_CACHE_STATE::SimpleLimit(PermSet& Perm) {};
void mu_1_CACHE_STATE::ArrayLimit(PermSet& Perm) {};
void mu_1_CACHE_STATE::Limit(PermSet& Perm) {};
void mu_1_CACHE_STATE::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for enum type.\n"); };
void mu_1_CACHE::Permute(PermSet& Perm, int i)
{
};
void mu_1_CACHE::SimpleCanonicalize(PermSet& Perm)
{ Error.Error("Internal: Simple Canonicalization of Record with no scalarset variable\n"); };
void mu_1_CACHE::Canonicalize(PermSet& Perm)
{
};
void mu_1_CACHE::SimpleLimit(PermSet& Perm){}
void mu_1_CACHE::ArrayLimit(PermSet& Perm){}
void mu_1_CACHE::Limit(PermSet& Perm)
{
};
void mu_1_CACHE::MultisetLimit(PermSet& Perm)
{
};
void mu_1_MEMORY::Permute(PermSet& Perm, int i)
{
};
void mu_1_MEMORY::SimpleCanonicalize(PermSet& Perm)
{ Error.Error("Internal: Simple Canonicalization of Record with no scalarset variable\n"); };
void mu_1_MEMORY::Canonicalize(PermSet& Perm)
{
};
void mu_1_MEMORY::SimpleLimit(PermSet& Perm){}
void mu_1_MEMORY::ArrayLimit(PermSet& Perm){}
void mu_1_MEMORY::Limit(PermSet& Perm)
{
};
void mu_1_MEMORY::MultisetLimit(PermSet& Perm)
{
};
void mu_1_LOCK::Permute(PermSet& Perm, int i)
{
};
void mu_1_LOCK::SimpleCanonicalize(PermSet& Perm)
{ Error.Error("Internal: Simple Canonicalization of Record with no scalarset variable\n"); };
void mu_1_LOCK::Canonicalize(PermSet& Perm)
{
};
void mu_1_LOCK::SimpleLimit(PermSet& Perm){}
void mu_1_LOCK::ArrayLimit(PermSet& Perm){}
void mu_1_LOCK::Limit(PermSet& Perm)
{
};
void mu_1_LOCK::MultisetLimit(PermSet& Perm)
{
};
void mu_1__type_0::Permute(PermSet& Perm, int i)
{
  static mu_1__type_0 temp("Permute_mu_1__type_0",-1);
  int j;
  for (j=0; j<2; j++)
    array[j].Permute(Perm, i);
};
void mu_1__type_0::SimpleCanonicalize(PermSet& Perm)
{ Error.Error("Internal: Simple Canonicalization of Scalarset Array\n"); };
void mu_1__type_0::Canonicalize(PermSet& Perm){};
void mu_1__type_0::SimpleLimit(PermSet& Perm){}
void mu_1__type_0::ArrayLimit(PermSet& Perm) {}
void mu_1__type_0::Limit(PermSet& Perm){}
void mu_1__type_0::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for scalarset array.\n"); };
void mu_1__type_1::Permute(PermSet& Perm, int i)
{
  static mu_1__type_1 temp("Permute_mu_1__type_1",-1);
  int j;
  for (j=0; j<2; j++)
    array[j].Permute(Perm, i);
};
void mu_1__type_1::SimpleCanonicalize(PermSet& Perm)
{ Error.Error("Internal: Simple Canonicalization of Scalarset Array\n"); };
void mu_1__type_1::Canonicalize(PermSet& Perm){};
void mu_1__type_1::SimpleLimit(PermSet& Perm){}
void mu_1__type_1::ArrayLimit(PermSet& Perm) {}
void mu_1__type_1::Limit(PermSet& Perm){}
void mu_1__type_1::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for scalarset array.\n"); };
void mu_1_NODE::Permute(PermSet& Perm, int i)
{
};
void mu_1_NODE::SimpleCanonicalize(PermSet& Perm)
{ Error.Error("Internal: Simple Canonicalization of Record with no scalarset variable\n"); };
void mu_1_NODE::Canonicalize(PermSet& Perm)
{
};
void mu_1_NODE::SimpleLimit(PermSet& Perm){}
void mu_1_NODE::ArrayLimit(PermSet& Perm){}
void mu_1_NODE::Limit(PermSet& Perm)
{
};
void mu_1_NODE::MultisetLimit(PermSet& Perm)
{
};
void mu_1_REPLACE_STAGE::Permute(PermSet& Perm, int i) {};
void mu_1_REPLACE_STAGE::SimpleCanonicalize(PermSet& Perm) {};
void mu_1_REPLACE_STAGE::Canonicalize(PermSet& Perm) {};
void mu_1_REPLACE_STAGE::SimpleLimit(PermSet& Perm) {};
void mu_1_REPLACE_STAGE::ArrayLimit(PermSet& Perm) {};
void mu_1_REPLACE_STAGE::Limit(PermSet& Perm) {};
void mu_1_REPLACE_STAGE::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for enum type.\n"); };
void mu_1_REPLACE_RULE::Permute(PermSet& Perm, int i) {};
void mu_1_REPLACE_RULE::SimpleCanonicalize(PermSet& Perm) {};
void mu_1_REPLACE_RULE::Canonicalize(PermSet& Perm) {};
void mu_1_REPLACE_RULE::SimpleLimit(PermSet& Perm) {};
void mu_1_REPLACE_RULE::ArrayLimit(PermSet& Perm) {};
void mu_1_REPLACE_RULE::Limit(PermSet& Perm) {};
void mu_1_REPLACE_RULE::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for enum type.\n"); };
void mu_1__type_2::Permute(PermSet& Perm, int i)
{
  static mu_1__type_2 temp("Permute_mu_1__type_2",-1);
  int j;
  for (j=0; j<2; j++)
    array[j].Permute(Perm, i);
};
void mu_1__type_2::SimpleCanonicalize(PermSet& Perm)
{ Error.Error("Internal: Simple Canonicalization of Scalarset Array\n"); };
void mu_1__type_2::Canonicalize(PermSet& Perm){};
void mu_1__type_2::SimpleLimit(PermSet& Perm){}
void mu_1__type_2::ArrayLimit(PermSet& Perm) {}
void mu_1__type_2::Limit(PermSet& Perm){}
void mu_1__type_2::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for scalarset array.\n"); };
void mu_1__type_3::Permute(PermSet& Perm, int i)
{
  static mu_1__type_3 temp("Permute_mu_1__type_3",-1);
  int j;
  for (j=0; j<1; j++)
    array[j].Permute(Perm, i);
};
void mu_1__type_3::SimpleCanonicalize(PermSet& Perm)
{ Error.Error("Internal: Simple Canonicalization of Scalarset Array\n"); };
void mu_1__type_3::Canonicalize(PermSet& Perm){};
void mu_1__type_3::SimpleLimit(PermSet& Perm){}
void mu_1__type_3::ArrayLimit(PermSet& Perm) {}
void mu_1__type_3::Limit(PermSet& Perm){}
void mu_1__type_3::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for scalarset array.\n"); };
void mu_1__type_4::Permute(PermSet& Perm, int i)
{
  static mu_1__type_4 temp("Permute_mu_1__type_4",-1);
  int j;
  for (j=0; j<3; j++)
    array[j].Permute(Perm, i);
};
void mu_1__type_4::SimpleCanonicalize(PermSet& Perm)
{ Error.Error("Internal: Simple Canonicalization of Scalarset Array\n"); };
void mu_1__type_4::Canonicalize(PermSet& Perm){};
void mu_1__type_4::SimpleLimit(PermSet& Perm){}
void mu_1__type_4::ArrayLimit(PermSet& Perm) {}
void mu_1__type_4::Limit(PermSet& Perm){}
void mu_1__type_4::MultisetLimit(PermSet& Perm)
{ Error.Error("Internal: calling MultisetLimit for scalarset array.\n"); };

/********************
 Auxiliary function for error trace printing
 ********************/
bool match(state* ns, StatePtr p)
{
  int i;
  static PermSet Perm;
  static state temp;
  StateCopy(&temp, ns);
  if (args->symmetry_reduction.value)
    {
      if (  args->sym_alg.mode == argsym_alg::Exhaustive_Fast_Canonicalize) {
        Perm.ResetToExplicit();
        for (i=0; i<Perm.count; i++)
          if (Perm.In(i))
            {
              if (ns != workingstate)
                  StateCopy(workingstate, ns);
              
              mu_replace.Permute(Perm,i);
              if (args->multiset_reduction.value)
                mu_replace.MultisetSort();
              mu_curData.Permute(Perm,i);
              if (args->multiset_reduction.value)
                mu_curData.MultisetSort();
              mu_curCache.Permute(Perm,i);
              if (args->multiset_reduction.value)
                mu_curCache.MultisetSort();
              mu_node.Permute(Perm,i);
              if (args->multiset_reduction.value)
                mu_node.MultisetSort();
              mu_lock.Permute(Perm,i);
              if (args->multiset_reduction.value)
                mu_lock.MultisetSort();
              mu_memory.Permute(Perm,i);
              if (args->multiset_reduction.value)
                mu_memory.MultisetSort();
              mu_curNode.Permute(Perm,i);
              if (args->multiset_reduction.value)
                mu_curNode.MultisetSort();
              mu_curMemory.Permute(Perm,i);
              if (args->multiset_reduction.value)
                mu_curMemory.MultisetSort();
              mu_curLock.Permute(Perm,i);
              if (args->multiset_reduction.value)
                mu_curLock.MultisetSort();
              mu_repRule.Permute(Perm,i);
              if (args->multiset_reduction.value)
                mu_repRule.MultisetSort();
            if (p.compare(workingstate)) {
              StateCopy(workingstate,&temp); return TRUE; }
          }
        StateCopy(workingstate,&temp);
        return FALSE;
      }
      else {
        Perm.ResetToSimple();
        Perm.SimpleToOne();
        if (ns != workingstate)
          StateCopy(workingstate, ns);

          mu_replace.Permute(Perm,0);
          if (args->multiset_reduction.value)
            mu_replace.MultisetSort();
          mu_curData.Permute(Perm,0);
          if (args->multiset_reduction.value)
            mu_curData.MultisetSort();
          mu_curCache.Permute(Perm,0);
          if (args->multiset_reduction.value)
            mu_curCache.MultisetSort();
          mu_node.Permute(Perm,0);
          if (args->multiset_reduction.value)
            mu_node.MultisetSort();
          mu_lock.Permute(Perm,0);
          if (args->multiset_reduction.value)
            mu_lock.MultisetSort();
          mu_memory.Permute(Perm,0);
          if (args->multiset_reduction.value)
            mu_memory.MultisetSort();
          mu_curNode.Permute(Perm,0);
          if (args->multiset_reduction.value)
            mu_curNode.MultisetSort();
          mu_curMemory.Permute(Perm,0);
          if (args->multiset_reduction.value)
            mu_curMemory.MultisetSort();
          mu_curLock.Permute(Perm,0);
          if (args->multiset_reduction.value)
            mu_curLock.MultisetSort();
          mu_repRule.Permute(Perm,0);
          if (args->multiset_reduction.value)
            mu_repRule.MultisetSort();
        if (p.compare(workingstate)) {
          StateCopy(workingstate,&temp); return TRUE; }

        while (Perm.NextPermutation())
          {
            if (ns != workingstate)
              StateCopy(workingstate, ns);
              
              mu_replace.Permute(Perm,0);
              if (args->multiset_reduction.value)
                mu_replace.MultisetSort();
              mu_curData.Permute(Perm,0);
              if (args->multiset_reduction.value)
                mu_curData.MultisetSort();
              mu_curCache.Permute(Perm,0);
              if (args->multiset_reduction.value)
                mu_curCache.MultisetSort();
              mu_node.Permute(Perm,0);
              if (args->multiset_reduction.value)
                mu_node.MultisetSort();
              mu_lock.Permute(Perm,0);
              if (args->multiset_reduction.value)
                mu_lock.MultisetSort();
              mu_memory.Permute(Perm,0);
              if (args->multiset_reduction.value)
                mu_memory.MultisetSort();
              mu_curNode.Permute(Perm,0);
              if (args->multiset_reduction.value)
                mu_curNode.MultisetSort();
              mu_curMemory.Permute(Perm,0);
              if (args->multiset_reduction.value)
                mu_curMemory.MultisetSort();
              mu_curLock.Permute(Perm,0);
              if (args->multiset_reduction.value)
                mu_curLock.MultisetSort();
              mu_repRule.Permute(Perm,0);
              if (args->multiset_reduction.value)
                mu_repRule.MultisetSort();
            if (p.compare(workingstate)) {
              StateCopy(workingstate,&temp); return TRUE; }
          }
        StateCopy(workingstate,&temp);
        return FALSE;
      }
    }
  if (!args->symmetry_reduction.value
      && args->multiset_reduction.value)
    {
      if (ns != workingstate)
          StateCopy(workingstate, ns);
      mu_replace.MultisetSort();
      mu_curData.MultisetSort();
      mu_curCache.MultisetSort();
      mu_node.MultisetSort();
      mu_lock.MultisetSort();
      mu_memory.MultisetSort();
      mu_curNode.MultisetSort();
      mu_curMemory.MultisetSort();
      mu_curLock.MultisetSort();
      mu_repRule.MultisetSort();
      if (p.compare(workingstate)) {
        StateCopy(workingstate,&temp); return TRUE; }
      StateCopy(workingstate,&temp);
      return FALSE;
    }
  return (p.compare(ns));
}

/********************
 Canonicalization by fast exhaustive generation of
 all permutations
 ********************/
void SymmetryClass::Exhaustive_Fast_Canonicalize(state* s)
{
  int i;
  static state temp;
  Perm.ResetToExplicit();

  StateCopy(&temp, workingstate);
  ResetBestResult();
  for (i=0; i<Perm.count; i++)
    if (Perm.In(i))
      {
        StateCopy(workingstate, &temp);
        mu_replace.Permute(Perm,i);
        if (args->multiset_reduction.value)
          mu_replace.MultisetSort();
        SetBestResult(i, workingstate);
      }
  StateCopy(workingstate, &BestPermutedState);

  StateCopy(&temp, workingstate);
  ResetBestResult();
  for (i=0; i<Perm.count; i++)
    if (Perm.In(i))
      {
        StateCopy(workingstate, &temp);
        mu_curData.Permute(Perm,i);
        if (args->multiset_reduction.value)
          mu_curData.MultisetSort();
        SetBestResult(i, workingstate);
      }
  StateCopy(workingstate, &BestPermutedState);

  StateCopy(&temp, workingstate);
  ResetBestResult();
  for (i=0; i<Perm.count; i++)
    if (Perm.In(i))
      {
        StateCopy(workingstate, &temp);
        mu_curCache.Permute(Perm,i);
        if (args->multiset_reduction.value)
          mu_curCache.MultisetSort();
        SetBestResult(i, workingstate);
      }
  StateCopy(workingstate, &BestPermutedState);

  StateCopy(&temp, workingstate);
  ResetBestResult();
  for (i=0; i<Perm.count; i++)
    if (Perm.In(i))
      {
        StateCopy(workingstate, &temp);
        mu_node.Permute(Perm,i);
        if (args->multiset_reduction.value)
          mu_node.MultisetSort();
        SetBestResult(i, workingstate);
      }
  StateCopy(workingstate, &BestPermutedState);

  StateCopy(&temp, workingstate);
  ResetBestResult();
  for (i=0; i<Perm.count; i++)
    if (Perm.In(i))
      {
        StateCopy(workingstate, &temp);
        mu_lock.Permute(Perm,i);
        if (args->multiset_reduction.value)
          mu_lock.MultisetSort();
        SetBestResult(i, workingstate);
      }
  StateCopy(workingstate, &BestPermutedState);

  StateCopy(&temp, workingstate);
  ResetBestResult();
  for (i=0; i<Perm.count; i++)
    if (Perm.In(i))
      {
        StateCopy(workingstate, &temp);
        mu_memory.Permute(Perm,i);
        if (args->multiset_reduction.value)
          mu_memory.MultisetSort();
        SetBestResult(i, workingstate);
      }
  StateCopy(workingstate, &BestPermutedState);

  StateCopy(&temp, workingstate);
  ResetBestResult();
  for (i=0; i<Perm.count; i++)
    if (Perm.In(i))
      {
        StateCopy(workingstate, &temp);
        mu_curNode.Permute(Perm,i);
        if (args->multiset_reduction.value)
          mu_curNode.MultisetSort();
        SetBestResult(i, workingstate);
      }
  StateCopy(workingstate, &BestPermutedState);

  StateCopy(&temp, workingstate);
  ResetBestResult();
  for (i=0; i<Perm.count; i++)
    if (Perm.In(i))
      {
        StateCopy(workingstate, &temp);
        mu_curMemory.Permute(Perm,i);
        if (args->multiset_reduction.value)
          mu_curMemory.MultisetSort();
        SetBestResult(i, workingstate);
      }
  StateCopy(workingstate, &BestPermutedState);

  StateCopy(&temp, workingstate);
  ResetBestResult();
  for (i=0; i<Perm.count; i++)
    if (Perm.In(i))
      {
        StateCopy(workingstate, &temp);
        mu_curLock.Permute(Perm,i);
        if (args->multiset_reduction.value)
          mu_curLock.MultisetSort();
        SetBestResult(i, workingstate);
      }
  StateCopy(workingstate, &BestPermutedState);

  StateCopy(&temp, workingstate);
  ResetBestResult();
  for (i=0; i<Perm.count; i++)
    if (Perm.In(i))
      {
        StateCopy(workingstate, &temp);
        mu_repRule.Permute(Perm,i);
        if (args->multiset_reduction.value)
          mu_repRule.MultisetSort();
        SetBestResult(i, workingstate);
      }
  StateCopy(workingstate, &BestPermutedState);

};

/********************
 Canonicalization by fast simple variable canonicalization,
 fast simple scalarset array canonicalization,
 fast restriction on permutation set with simple scalarset array of scalarset,
 and fast exhaustive generation of
 all permutations for other variables
 ********************/
void SymmetryClass::Heuristic_Fast_Canonicalize(state* s)
{
  int i;
  static state temp;

  Perm.ResetToSimple();

};

/********************
 Canonicalization by fast simple variable canonicalization,
 fast simple scalarset array canonicalization,
 fast restriction on permutation set with simple scalarset array of scalarset,
 and fast exhaustive generation of
 all permutations for other variables
 and use less local memory
 ********************/
void SymmetryClass::Heuristic_Small_Mem_Canonicalize(state* s)
{
  unsigned long cycle;
  static state temp;

  Perm.ResetToSimple();

};

/********************
 Normalization by fast simple variable canonicalization,
 fast simple scalarset array canonicalization,
 fast restriction on permutation set with simple scalarset array of scalarset,
 and for all other variables, pick any remaining permutation
 ********************/
void SymmetryClass::Heuristic_Fast_Normalize(state* s)
{
  int i;
  static state temp;

  Perm.ResetToSimple();

};

/********************
  Include
 ********************/
#include "mu_epilog.hpp"
