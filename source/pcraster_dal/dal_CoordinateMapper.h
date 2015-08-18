#ifndef INCLUDED_DAL_COORDINATEMAPPER
#define INCLUDED_DAL_COORDINATEMAPPER



// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif



namespace dal {
  // CoordinateMapper declarations.
  class DataSpace;
  class DataSpaceAddress;
}



namespace dal {



//! Base class for coordinate mappers which map coordinates from one range to another.
/*!
  This class can be used for dummy coordinate mappers which do nothing
  to translate coordinates.

  A coordinate mapper maps coordinates from one range (the source range) to
  another (the destination range). Furthermore a string representation
  can be created for an address.

  Coordinate mappers can be implemented for all types of dimensions,
  whatever the type of the coordinates. All values passed in and returned
  by the member functions are encapsulated by the DataSpaceAddress class.

  Coordinate mappers are used by the DataSpaceAddressMapper class.
*/
class PCR_DAL_DECL CoordinateMapper
{

  friend class CoordinateMapperTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CoordinateMapper    ();

  virtual          ~CoordinateMapper   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  virtual void     mapToDestination    (DataSpace const& space,
                                        DataSpaceAddress& address,
                                        size_t index) const;

  virtual void     mapToSource         (DataSpace const& space,
                                        DataSpaceAddress& address,
                                        size_t index) const;

  virtual std::string toString         (DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        size_t index) const;

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



} // namespace dal

#endif
