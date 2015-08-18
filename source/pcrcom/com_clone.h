#ifndef INCLUDED_COM_CLONE
#define INCLUDED_COM_CLONE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace com {
  // clone declarations.
}



namespace com {


/*!
 * \file
 *  templates to manage class hierarchy using the createClone
 *  construct for virtual copying.
 * See style guide for createClone()
 */
//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

template<class Container>
  void copyClone(const Container& src,Container& dest)
  {
    for(typename Container::const_iterator i=src.begin(); i!=src.end(); ++i)
      dest.push_back((*i)->createClone());
  }

template<class Container>
  void deleteCloneContainer(const Container& src)
  {
    for(typename Container::const_iterator i=src.begin(); i!=src.end(); ++i)
      delete *i;
  }

template<class Container>
  void clearClone(Container& src)
  {
    deleteCloneContainer(src);
    src.clear();
  }

//! return src->createClone() or 0 if src is 0
template<class O>
 O *non0Clone(const O* src) {
   if (!src)
     return 0;
   return src->createClone();
 }

//! delete \a dest and set to src->createClone() or 0 if  \a src is 0
template<class O>
 void resetClone(O*& dest, const O* src) {
   delete dest;
   dest=non0Clone(src);
 }

//! return <b>new O(*src)</b> or 0 if src is 0 (not a clone thing)
template<class O>
 O *non0CopyCtor(const O* src) {
   if (!src)
     return 0;
   return new O(*src);
 }
} // namespace com

#endif
