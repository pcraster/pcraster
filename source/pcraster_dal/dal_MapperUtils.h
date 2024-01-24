#ifndef INCLUDED_DAL_MAPPERUTILS
#define INCLUDED_DAL_MAPPERUTILS



// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_STEPMAPPER
#include "dal_StepMapper.h"
#define INCLUDED_DAL_STEPMAPPER
#endif

#include <tuple>


namespace dal {
  // MapperUtils declarations.
  class Dimension;
  class SpaceStepMapper;
  class TimeStepMapper;
}



namespace dal {

// typedef std::tuple<
//          Dimension,
//          boost::posix_time::ptime,
//          boost::posix_time::time_duration> TimeMapping;

typedef std::tuple<
         Dimension,
         TimeStepMapper const*> DimensionTimeStepMapping;

typedef std::tuple<
         Dimension,
         SpaceStepMapper const*> DimensionSpaceStepMapping;

typedef std::tuple<
         Dimension,
         StepMapper const*> DimensionStepMapping;

PCR_DAL_DECL void  stepMap             (
                             std::vector<DimensionStepMapping> const& mappings,
                             Dimension* dimension,
                             StepMapper* mapper,
                             std::vector<StepMapper>* stepMappers);

PCR_DAL_DECL void  spaceStepMap        (
                             std::vector<DimensionSpaceStepMapping> const& mappings,
                             Dimension* dimension,
                             SpaceStepMapper* mapper,
                             std::vector<StepMapper>* stepMappers);

PCR_DAL_DECL void  timeStepMap         (
                        std::vector<DimensionTimeStepMapping> const& mappings,
                        Dimension* dimension,
                        TimeStepMapper* mapper,
                        std::vector<StepMapper>* stepMappers);

// void               timeStepMap         (std::vector<TimeMapping> const& mappings,
//                                         Dimension* dimension,
//                                         boost::posix_time::time_duration* duration,
//                                         std::vector<StepMapper>* stepMappers);

} // namespace dal

#endif
