#ifndef INCLUDED_CALC_DVAUTOPTR
#define INCLUDED_CALC_DVAUTOPTR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_DATAVALUE
#include "calc_datavalue.h"
#define INCLUDED_CALC_DATAVALUE
#endif


namespace calc {
  // DVAutoPtr declarations.
}



namespace calc {

/*! use DVAutoPtr on DataValue-based object instead of std::auto_ptr
 */
template <class T>
 class DVAutoPtr : boost::noncopyable {
   private:
     T* d_ap;
   public:
     DVAutoPtr(T* ap): d_ap(ap) {};
    ~DVAutoPtr()                { deleteFromPcrme(d_ap); }

    T* get()                    { return d_ap; }
    T* operator->()             { return d_ap; }
    T* release()                { T*p=d_ap;d_ap=0;return p;}
 };



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
