#ifndef INCLUDED_DAL_MAPPERUTILS
#include "dal_MapperUtils.h"
#define INCLUDED_DAL_MAPPERUTILS
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DIMENSION
#include "dal_Dimension.h"
#define INCLUDED_DAL_DIMENSION
#endif

#ifndef INCLUDED_DAL_SPACESTEPMAPPER
#include "dal_SpaceStepMapper.h"
#define INCLUDED_DAL_SPACESTEPMAPPER
#endif

#ifndef INCLUDED_DAL_STEPMAPPER
#include "dal_StepMapper.h"
#define INCLUDED_DAL_STEPMAPPER
#endif

#ifndef INCLUDED_DAL_TIMESTEPMAPPER
#include "dal_TimeStepMapper.h"
#define INCLUDED_DAL_TIMESTEPMAPPER
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



namespace dal {

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   The meaning of \a dimension is set to Space. This is OK for now
             but might need adjustment if this function is called for other
             dimensions than space dimensions. Maybe leave the meaning
             unchanged?
  \sa        .
*/
PCR_DAL_DECL void stepMap(
         std::vector<DimensionStepMapping> const& mappings,
         Dimension* dimension,
         StepMapper* mapper,
         std::vector<StepMapper>* stepMappers)
{
  if(!mappings.empty()) {

    for(size_t i = 0; i < mappings.size(); ++i) {
      Dimension dimension = boost::get<0>(mappings[i]);
      StepMapper const* aMapper = boost::get<1>(mappings[i]);

      // Create overall mapper by merging all input mappers.
      if(i == 0) {
        *mapper = *aMapper;
      }
      else {
        *mapper |= *aMapper;
      }
    }

    // -------------------------------------------------------------------------
    // Configure the space dimension which contains all mappings.
    // double firstIndex(mapper->source(min));
    // double lastIndex(mapper->source(max));
    double firstIndex(mapper->sourceFirstStep());
    double lastIndex(mapper->sourceLastStep());
    assert(comparable(std::fmod(firstIndex, 1.0), 0.0));
    assert(comparable(std::fmod(lastIndex, 1.0), 0.0));
    assert(greaterOrComparable(firstIndex, 0.0));
    assert(greaterOrComparable(lastIndex, 0.0));

    std::vector<size_t> values;
    values.push_back(round<double, size_t>(firstIndex));
    values.push_back(round<double, size_t>(lastIndex));
    values.push_back(1);
    *dimension = Dimension(Space, values);

    // -------------------------------------------------------------------------
    // Configure step mappers for all mappings.
    stepMappers->clear();
    stepMappers->reserve(mappings.size());

    size_t firstStep, lastStep;
    double newFirstStep, newLastStep;
    double first, last;
    StepMapper const* aMapper;

    for(size_t i = 0; i < mappings.size(); ++i) {
      Dimension const& dimension = boost::get<0>(mappings[i]);
      aMapper = boost::get<1>(mappings[i]);

      firstStep = dimension.value<size_t>(0);
      lastStep = dimension.value<size_t>(1);

      first = aMapper->destination(firstStep);
      last = aMapper->destination(lastStep);

      newFirstStep = mapper->source(first);
      newLastStep = mapper->source(last);

      stepMappers->push_back(StepMapper(newFirstStep, newLastStep,
         firstStep, lastStep));
    }
  }
}



PCR_DAL_DECL void spaceStepMap(
         std::vector<DimensionSpaceStepMapping> const& mappings,
         Dimension* dimension,
         SpaceStepMapper* mapper,
         std::vector<StepMapper>* stepMappers)
{
  if(!mappings.empty()) {
    double lowestCoordinate = 0.0;
    double highestCoordinate = 0.0;

    // -------------------------------------------------------------------------
    // Determine extreme coordinates.
    // Merge mappers.
    for(size_t i = 0; i < mappings.size(); ++i) {
      Dimension dimension = boost::get<0>(mappings[i]);
      size_t lowestIndex = dimension.value<size_t>(0);
      size_t highestIndex = dimension.value<size_t>(1);
      SpaceStepMapper const* aMapper = boost::get<1>(mappings[i]);

      if(i == 0) {
        lowestCoordinate = aMapper->destination(lowestIndex);
        highestCoordinate = aMapper->destination(highestIndex);
        *mapper = *aMapper;
      }
      else {
        lowestCoordinate = std::min<double>(lowestCoordinate,
              aMapper->destination(lowestIndex));
        highestCoordinate = std::max<double>(highestCoordinate,
              aMapper->destination(highestIndex));
        *mapper |= *aMapper;
      }
    }

    // -------------------------------------------------------------------------
    // Configure the dimension which contains all mappings.
    double firstIndex(mapper->source(lowestCoordinate));
    double lastIndex(mapper->source(highestCoordinate));

    // Here we test whether we get float results which are convertable to
    // integers without much loss. This should be the case.
    // Comparing floating point numbers happens by checking whether the
    // difference of two values is smaller than x percent. Because
    // fmod(index) computed below is normally very small, the call to comparable
    // fails easely. Because of that, we add 1.0 to make this check pass in
    // these still valid cases.
    assert(comparable(std::fmod(firstIndex, 1.0) + 1.0, 0.0 + 1.0));
    assert(comparable(std::fmod(lastIndex, 1.0) + 1.0, 0.0 + 1.0));
    assert(greaterOrComparable(firstIndex, 0.0));
    assert(greaterOrComparable(lastIndex, 0.0));

    std::vector<size_t> values;
    values.push_back(round<double, size_t>(firstIndex));
    values.push_back(round<double, size_t>(lastIndex));
    values.push_back(1);
    *dimension = Dimension(Space, values);

    // -------------------------------------------------------------------------
    // Configure step mappers for all mappings.
    stepMappers->clear();
    stepMappers->reserve(mappings.size());

    for(size_t i = 0; i < mappings.size(); ++i) {
      Dimension dimension = boost::get<0>(mappings[i]);
      size_t lowestIndex = dimension.value<size_t>(0);
      size_t highestIndex = dimension.value<size_t>(1);
      SpaceStepMapper const* aMapper = boost::get<1>(mappings[i]);

      lowestCoordinate = aMapper->destination(lowestIndex);
      highestCoordinate = aMapper->destination(highestIndex);

      double newFirstStep = mapper->source(lowestCoordinate);
      double newLastStep = mapper->source(highestCoordinate);

      stepMappers->push_back(StepMapper(newFirstStep, newLastStep,
         lowestIndex, highestIndex));
    }
  }
}



PCR_DAL_DECL void timeStepMap(
         std::vector<DimensionTimeStepMapping> const& mappings,
         Dimension* dimension,
         TimeStepMapper* mapper,
         std::vector<StepMapper>* stepMappers)
{
  if(!mappings.empty()) {
    boost::posix_time::ptime firstTime, lastTime;

    // -------------------------------------------------------------------------
    // Determine first and last real time moment.
    // Merge time step mappers.
    for(size_t i = 0; i < mappings.size(); ++i) {
      Dimension dimension = boost::get<0>(mappings[i]);
      size_t firstStep = dimension.value<size_t>(0);
      size_t lastStep = dimension.value<size_t>(1);
      TimeStepMapper const* aMapper = boost::get<1>(mappings[i]);

      if(i == 0) {
        firstTime = aMapper->destination(firstStep);
        lastTime = aMapper->destination(lastStep);
        *mapper = *aMapper;
      }
      else {
        firstTime = std::min<boost::posix_time::ptime>(firstTime,
              aMapper->destination(firstStep));
        lastTime = std::max<boost::posix_time::ptime>(lastTime,
              aMapper->destination(lastStep));
        *mapper |= *aMapper;
      }
    }

    // -------------------------------------------------------------------------
    // Configure the time dimension which contains all mappings.
    double firstIndex(mapper->source(firstTime));
    double lastIndex(mapper->source(lastTime));
    assert(comparable(std::fmod(firstIndex, 1.0), 0.0));
    assert(comparable(std::fmod(lastIndex, 1.0), 0.0));
    assert(greaterOrComparable(firstIndex, 0.0));
    assert(greaterOrComparable(lastIndex, 0.0));

    std::vector<size_t> values;
    values.push_back(round<double, size_t>(firstIndex));
    values.push_back(round<double, size_t>(lastIndex));
    values.push_back(1);
    *dimension = Dimension(Time, values);

    // -------------------------------------------------------------------------
    // Configure step mappers for all mappings.
    stepMappers->clear();
    stepMappers->reserve(mappings.size());

    for(size_t i = 0; i < mappings.size(); ++i) {
      Dimension dimension = boost::get<0>(mappings[i]);
      size_t firstStep = dimension.value<size_t>(0);
      size_t lastStep = dimension.value<size_t>(1);
      TimeStepMapper const* aMapper = boost::get<1>(mappings[i]);

      firstTime = aMapper->destination(firstStep);
      lastTime = aMapper->destination(lastStep);

      double newFirstStep = mapper->source(firstTime);
      double newLastStep = mapper->source(lastTime);

      stepMappers->push_back(StepMapper(newFirstStep, newLastStep,
         firstStep, lastStep));
    }
  }
}



// //!
// /*!
//   \param     .
//   \return    .
//   \exception .
//   \warning   .
//   \sa        .
//   \todo      Check input dimensions.
// 
//   Nothing happens when mappings is empty.
// */
// void timeStepMap(
//          std::vector<TimeMapping> const& mappings,
//          Dimension* dimension,
//          boost::posix_time::time_duration* duration,
//          std::vector<StepMapper>* stepMappers)
// {
//   if(!mappings.empty()) {
//     // -------------------------------------------------------------------------
//     // Create time step mappers for each mapping and keep track of the first
//     // and last real time moment.
//     TimeMapping const& mapping(mappings[0]);
//     Dimension const& aDimension(boost::get<0>(mapping));
//     double startIndex(aDimension.value<size_t>(0));
//     double endIndex(aDimension.value<size_t>(1));
//     boost::posix_time::ptime firstTime(boost::get<1>(mapping));
//     boost::posix_time::time_duration aDuration(boost::get<2>(mapping));
//     assert(startIndex <= endIndex);
// 
//     // Time step mappers for each mapping.
//     std::vector<TimeStepMapper> timeStepMappers;
//     timeStepMappers.reserve(mappings.size());
//     timeStepMappers.push_back(TimeStepMapper(startIndex, firstTime, aDuration));
//     TimeStepMapper mapper(timeStepMappers.back());
//     boost::posix_time::ptime lastTime(
//          firstTime + aDuration * (aDimension.nrCoordinates() - 1));
// 
//     for(size_t i = 1; i < mappings.size(); ++i) {
//       TimeMapping const& mapping(mappings[i]);
//       Dimension const& aDimension(boost::get<0>(mapping));
//       size_t startIndex(aDimension.value<size_t>(0));
//       size_t endIndex(aDimension.value<size_t>(1));
//       boost::posix_time::ptime startTime(boost::get<1>(mapping));
//       boost::posix_time::time_duration aDuration(boost::get<2>(mapping));
//       assert(startIndex <= endIndex);
// 
//       timeStepMappers.push_back(
//          TimeStepMapper(startIndex, startTime, aDuration));
//       mapper |= timeStepMappers.back();
// 
//       firstTime = std::min(firstTime, startTime);
//       lastTime = std::max(lastTime,
//          firstTime + aDuration * (aDimension.nrCoordinates() - 1));
//     }
// 
//     *duration = mapper.duration();
// 
//     // -------------------------------------------------------------------------
//     // Configure the time dimension which contains all mappings.
//     double firstIndex(mapper.source(firstTime));
//     double lastIndex(mapper.source(lastTime));
//     assert(comparable(std::fmod(firstIndex, 1.0), 0.0));
//     assert(comparable(std::fmod(lastIndex, 1.0), 0.0));
//     assert(greaterOrComparable(firstIndex, 0.0));
//     assert(greaterOrComparable(lastIndex, 0.0));
// 
//     std::vector<size_t> values;
//     values.push_back(static_cast<size_t>(rint(firstIndex)));
//     values.push_back(static_cast<size_t>(rint(lastIndex)));
//     values.push_back(1);
//     *dimension = Dimension(Time, RegularDiscretisation, values);
// 
//     // -------------------------------------------------------------------------
//     // Configure step mappers for all mappings.
//     stepMappers->clear();
//     stepMappers->reserve(mappings.size());
// 
//     for(size_t i = 0; i < mappings.size(); ++i) {
//       TimeMapping const& mapping(mappings[i]);
//       Dimension const& aDimension(boost::get<0>(mapping));
//       size_t startIndex(aDimension.value<size_t>(0));
//       size_t endIndex(aDimension.value<size_t>(1));
//       boost::posix_time::ptime startTime(boost::get<1>(mapping));
//       boost::posix_time::time_duration aDuration(boost::get<2>(mapping));
//       boost::posix_time::ptime endTime(
//          startTime + aDuration * (aDimension.nrCoordinates() - 1));
// 
//       double newStartIndex = mapper.source(startTime);
//       double newEndIndex = mapper.source(endTime);
//       stepMappers->push_back(StepMapper(newStartIndex, newEndIndex,
//          startIndex, endIndex));
//     }
//   }
// }

} // namespace dal

