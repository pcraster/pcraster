#ifndef INCLUDED_DAL_TYPES
#define INCLUDED_DAL_TYPES



// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_TYPE
#include "dal_Type.h"
#define INCLUDED_DAL_TYPE
#endif



namespace dal {
  // Types declarations.
  class Type;
}



namespace dal {



/*!
   implementation resides in dal_type.cc
   \todo rename to NumericTypes ?
*/
class Types : private std::vector<const Type *>
{

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Types               ();

  /* virtual */    ~Types              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  TypeId           idOfSmallestType    (std::string const& string) const;

  TypeId           idOfLargestType     (TypeId id1,
                                        TypeId id2) const;

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
