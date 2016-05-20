#ifndef INCLUDED_CALC_MAPSTACKTYPE
#define INCLUDED_CALC_MAPSTACKTYPE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.



namespace calc {
  // MapStackType declarations.
}



namespace calc {



//! types for StackInput to distinguish between timeinput(|sparse|modulo)
/*!
 * a map stack can only be used in a single type of timeinput* operation
 */
class MapStackType
{
public:
  //! the use types, only a single type is allowed
  enum Use { Unknown, Full, Sparse, Modulo, Lookup };
private:

  // Assignment operator. DEFAULT
  // MapStackType&   operator=           (MapStackType const& rhs);

  //  Copy constructor. DEFAULT
  //               MapStackType               (MapStackType const& rhs);

  /*! \brief if a StackInput must be complete; not sparse. d_vs == VS_MAPSTACK
   *
   * if d_fullMapStack then this symbol is at least once used as
   * the argument of timeinput(). If only used in timeinputsparse() ops
   * then this is false;
   */
  Use              d_use;


  //! timeinputmodulo argument, 1 to d_highestTimestepAvailable must be available
  size_t           d_highestTimestepAvailable;

  static std::string      operationName       (Use u);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MapStackType               ();

  /* virtual */    ~MapStackType              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setUse                      (const Use use);
  void             setHighestTimestepAvailable (size_t highestTimestepAvailable);
  void             update                      (const MapStackType& use);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  Use              use                          () const;
  size_t           highestTimestepAvailable     () const;

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
