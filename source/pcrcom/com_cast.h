#ifndef INCLUDED_COM_CAST
#define INCLUDED_COM_CAST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace com {
  // cast declarations.
}

/*!
  \file
  This file contains the implementation special cast templates.
*/


namespace com {

//! cast a non 0 \a ptr from UpType to DownType and assume it will succeed
/*!
 * this is a debug wrapper around dynamic_cast, the term down_cast is 
 * used as proposed by [BS,C++,15.4].
 * down_cast check two assertions that ptr is not 0 and that the
 * cast succeeds; the result ptr is a DownType.
 */
template <class DownType, class UpType>
    DownType down_cast(UpType ptr) {
#   ifdef DEBUG_DEVELOP
        // cast must succeed
        PRECOND(ptr);
        PRECOND(dynamic_cast<DownType>(ptr));
#   endif
        return dynamic_cast<DownType>(ptr);
    }

//! cast a \a ptr from UpType to DownType and assume it will succeed
/*!
 * as in down_cast but a 0 ptr is returned if \a ptr is 0.
 */
template <class DownType, class UpType>
    DownType down_cast_0to0(UpType ptr) {
      if (ptr) {
#   ifdef DEBUG_DEVELOP
        // cast must succeed
        PRECOND(dynamic_cast<DownType>(ptr));
#   endif
        return dynamic_cast<DownType>(ptr);
      }
      return 0;
    }


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
