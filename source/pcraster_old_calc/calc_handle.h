#ifndef INCLUDED_CALC_HANDLE
#define INCLUDED_CALC_HANDLE

namespace calc {
//! handle template with automatic clean up
/*! Copied from Stroustrup sect 25.7. <BR>
 *  Pass ptr of allocated object to constructor
 *  and the handle will clean up the object if
 *  there are no more handles to the object present.
 */
template<class X>class Handle {
  X*   d_rep;
  int*  d_ptrCount;
public:
  X* operator->()      { return d_rep; }
  const X* operator->() const  { return d_rep; }
  const X* get_rep() const { return d_rep; }

/* MSCPI C2738 NOT NEEDED? SEEMS TO COMPILE FINE
  template<class subClassOfX>Handle<X>::operator
    Handle<subClassOfX>()  { return Handle<subClassOfX>(d_rep); }
 */

  //! A 0-ptr for pp is illegal
  Handle(X * pp): d_rep(pp),d_ptrCount(new int(1))
    {
#ifdef DEBUG_DEVELOP
      POSTCOND(pp);
#endif
    }

  Handle(const Handle& r): d_rep(r.d_rep),d_ptrCount(r.d_ptrCount)
    { (*d_ptrCount)++; }

  //! Is this the only handle that is alive ?
  bool isOnlyHandle() const { return (*d_ptrCount) == 1; }

  Handle& operator=(const Handle& r)
  {
    if (d_rep == r.d_rep)
      return *this;
    if (--(*d_ptrCount) == 0) {
      delete d_rep;
      delete d_ptrCount;
    }
    d_rep      = r.d_rep;
    d_ptrCount = r.d_ptrCount;
    (*d_ptrCount)++;
    return *this;
  }

  ~Handle()  {
    if (--(*d_ptrCount) == 0)
      { delete d_rep; delete d_ptrCount; }
  }
};

}

#endif
