#ifndef INCLUDED_DAL_SPACESTEPCOORDINATEMAPPER
#define INCLUDED_DAL_SPACESTEPCOORDINATEMAPPER



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_COORDINATEMAPPER
#include "dal_CoordinateMapper.h"
#define INCLUDED_DAL_COORDINATEMAPPER
#endif

#ifndef INCLUDED_DAL_SPACESTEPMAPPER
#include "dal_SpaceStepMapper.h"
#define INCLUDED_DAL_SPACESTEPMAPPER
#endif



namespace dal {
  // SpaceStepCoordinateMapper declarations.
}



namespace dal {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class PCR_DAL_DECL SpaceStepCoordinateMapper: public CoordinateMapper,
                                              public SpaceStepMapper
{

  friend class SpaceStepCoordinateMapperTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SpaceStepCoordinateMapper(size_t index,
                                        double coordinate,
                                        double cellSize);

                   SpaceStepCoordinateMapper(
                                        SpaceStepMapper const& mapper);

  /* virtual */    ~SpaceStepCoordinateMapper();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  std::string      toString            (DataSpace const& space,
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
