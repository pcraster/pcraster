#ifndef INCLUDED_DAL_TIMESTEPCOORDINATEMAPPER
#define INCLUDED_DAL_TIMESTEPCOORDINATEMAPPER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_COORDINATEMAPPER
#include "dal_CoordinateMapper.h"
#define INCLUDED_DAL_COORDINATEMAPPER
#endif

#ifndef INCLUDED_DAL_TIMESTEPMAPPER
#include "dal_TimeStepMapper.h"
#define INCLUDED_DAL_TIMESTEPMAPPER
#endif



namespace dal {
  // TimeStepCoordinateMapper declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class PCR_DAL_DECL TimeStepCoordinateMapper: public CoordinateMapper,
                                             public TimeStepMapper
{

  friend class TimeStepCoordinateMapperTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TimeStepCoordinateMapper(
                                        double index,
                                        boost::posix_time::ptime const& time,
                                        boost::posix_time::time_duration const& duration);

                   TimeStepCoordinateMapper(
                                        TimeStepMapper const& mapper);

  /* virtual */    ~TimeStepCoordinateMapper();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  std::string      toString            (DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        size_t index) const;

  bool             equals              (TimeStepCoordinateMapper const& mapper) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

PCR_DAL_DECL bool  operator==          (TimeStepCoordinateMapper const& lhs,
                                        TimeStepCoordinateMapper const& rhs);

PCR_DAL_DECL bool  operator!=          (TimeStepCoordinateMapper const& lhs,
                                        TimeStepCoordinateMapper const& rhs);



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
