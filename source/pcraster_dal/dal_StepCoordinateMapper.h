#ifndef INCLUDED_DAL_STEPCOORDINATEMAPPER
#define INCLUDED_DAL_STEPCOORDINATEMAPPER



// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// PCRaster library headers.
#ifndef INCLUDED_DEV_COMPILER
#include "dev_Compiler.h"
#define INCLUDED_DEV_COMPILER
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_COORDINATEMAPPER
#include "dal_CoordinateMapper.h"
#define INCLUDED_DAL_COORDINATEMAPPER
#endif

#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif

#ifndef INCLUDED_DAL_STEPMAPPER
#include "dal_StepMapper.h"
#define INCLUDED_DAL_STEPMAPPER
#endif



namespace dal {
  // StepCoordinateMapper declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class PCR_DAL_DECL StepCoordinateMapper: private boost::noncopyable,
                                         public CoordinateMapper,
                                         public StepMapper
{

  friend class DisplayStepMapperTest;

private:

  MissingDataStrategy d_missingDataStrategy;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   StepCoordinateMapper(double sourceFirstStep,
                                        double sourceLastStep,
                                        double destinationFirstStep,
                                        double destinationLastStep,
                                        MissingDataStrategy strategy);

                   StepCoordinateMapper(StepMapper const& mapper,
                                        MissingDataStrategy strategy);

  /* virtual */    ~StepCoordinateMapper();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             mapToDestination    (DataSpace const& space,
                                        DataSpaceAddress& address,
                                        size_t index) const;

  void             mapToSource         (DataSpace const& space,
                                        DataSpaceAddress& address,
                                        size_t index) const;

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
