#ifndef INCLUDED_CALC_DVAUTOPTR
#define INCLUDED_CALC_DVAUTOPTR

#include "stddefx.h"
#include "calc_datavalue.h"


namespace calc {
  // DVAutoPtr declarations.
}



namespace calc {

/*! use DVAutoPtr on DataValue-based object instead of std::auto_ptr
 */
template <class T>
 class DVAutoPtr {
   private:
     T* d_ap;
   public:
     DVAutoPtr(T* ap): d_ap(ap) {}
     DVAutoPtr(const DVAutoPtr& other) = delete;
     DVAutoPtr& operator=(const DVAutoPtr& other) = delete;
    ~DVAutoPtr()                { deleteFromPcrme(d_ap); }

    T* get()                    { return d_ap; }
    T* operator->()             { return d_ap; }
    T* release()                { T*p=d_ap;d_ap=nullptr;return p;}
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
