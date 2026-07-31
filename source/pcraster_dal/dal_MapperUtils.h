#ifndef INCLUDED_DAL_MAPPERUTILS
#define INCLUDED_DAL_MAPPERUTILS

#include "dal_Configure.h"
#include "dal_StepMapper.h"

#include <tuple>
#include <vector>

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

using DimensionTimeStepMapping = std::tuple<Dimension, const TimeStepMapper *>;

using DimensionSpaceStepMapping = std::tuple<Dimension, const SpaceStepMapper *>;

using DimensionStepMapping = std::tuple<Dimension, const StepMapper *>;

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
