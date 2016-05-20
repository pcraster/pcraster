#ifndef INCLUDED_CALC_ICELLITERATOR
#define INCLUDED_CALC_ICELLITERATOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // ICellIterator declarations.
}



namespace calc {



//! Interface to a "cell iterator"
/*!
   This is not an iterator in the classic sense, it something between a iterator and
   a visitor. It only exposes a few methods when running an lddrouting or cellfocus.
*/
class ICellIterator
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ICellIterator&           operator=           (ICellIterator const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ICellIterator               (ICellIterator const& rhs);

protected:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ICellIterator               ();

     virtual       ~ICellIterator              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
public:
  //! return current cell index
  virtual size_t   current                      () const=0;
  virtual size_t   lddDownstream                () const;

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
