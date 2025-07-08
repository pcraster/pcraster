#ifndef INCLUDED_DAL_STEPCOORDINATEMAPPER
#define INCLUDED_DAL_STEPCOORDINATEMAPPER



// Library headers.

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

#include <string>


namespace dal {
  // StepCoordinateMapper declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class PCR_DAL_DECL StepCoordinateMapper: public CoordinateMapper,
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

                   StepCoordinateMapper(StepCoordinateMapper const& other) = delete;

  StepCoordinateMapper& operator=      (StepCoordinateMapper const& other) = delete;

  /* virtual */    ~StepCoordinateMapper() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             mapToDestination    (DataSpace const& space,
                                        DataSpaceAddress& address,
                                        size_t index) const override;

  void             mapToSource         (DataSpace const& space,
                                        DataSpaceAddress& address,
                                        size_t index) const override;

  std::string      toString            (DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        size_t index) const override;

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
