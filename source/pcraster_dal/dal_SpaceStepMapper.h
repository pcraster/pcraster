#ifndef INCLUDED_DAL_SPACESTEPMAPPER
#define INCLUDED_DAL_SPACESTEPMAPPER



// External headers.
#ifndef INCLUDED_CSTDDEF
#include <cstddef>
#define INCLUDED_CSTDDEF
#endif

#ifdef DEBUG_DEVELOP
  #ifndef INCLUDED_IOSTREAM
  #include <iostream>
  #define INCLUDED_IOSTREAM
  #endif
#endif

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif



namespace dal {
  // SpaceStepMapper declarations.
}



namespace dal {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class PCR_DAL_DECL SpaceStepMapper
{

  friend class SpaceStepMapperTest;

private:

  //! Index of cell.
  size_t d_index;

  //! Coordinate of cell.
  double d_coordinate;

  //! Size of cell.
  double d_cellSize;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SpaceStepMapper     ();

                   SpaceStepMapper     (size_t index,
                                        double coordinate,
                                        double cellSize);

  virtual          ~SpaceStepMapper    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  SpaceStepMapper& operator|=          (SpaceStepMapper const& rhs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isValid             () const;

  size_t           index               () const;

  double           coordinate          () const;

  double           cellSize            () const;

  double           destination         (double index) const;

  double           source              (double coordinate) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

#ifdef DEBUG_DEVELOP
std::ostream&      operator<<          (std::ostream& stream,
                                        SpaceStepMapper const& mapper);
#endif



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif
