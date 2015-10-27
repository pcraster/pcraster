#define BOOST_TEST_MODULE pcraster dal mapper_utils
#include <boost/test/unit_test.hpp>
#include "dal_Dimension.h"
#include "dal_MapperUtils.h"
#include "dal_TimeStepMapper.h"


BOOST_AUTO_TEST_CASE(step_map)
{
  using namespace dal;

  {
    std::vector<DimensionStepMapping> mappings;

    std::vector<size_t> values;
    values.push_back(1);
    values.push_back(80);
    values.push_back(1);

    dal::StepMapper aMapper(1.0, 80.0, 182145.0, 182935.0);

    mappings.push_back(DimensionStepMapping(Dimension(Space, values),
         &aMapper));

    /// Dimension dimension;
    /// dal::StepMapper mapper;
    /// std::vector<StepMapper> stepMappers;

    /// stepMap(mappings, &dimension, &mapper, &stepMappers);

    /// BOOST_CHECK_EQUAL(dimension.meaning(), Space);
    /// BOOST_CHECK_EQUAL(dimension.coordinateType(), NumericalCoordinates);
    /// BOOST_CHECK_EQUAL(dimension.discretisation(), RegularDiscretisation);
    /// BOOST_CHECK_EQUAL(dimension.nrValues(), size_t(3));
    /// BOOST_CHECK_EQUAL(dimension.value<size_t>(0), size_t(1));
    /// BOOST_CHECK_EQUAL(dimension.value<size_t>(1), size_t(80));
    /// BOOST_CHECK_EQUAL(dimension.value<size_t>(2), size_t(1));

    /// BOOST_CHECK(comparable(mapper.sourceFirstStep(), 1.0));
    /// BOOST_CHECK(comparable(mapper.sourceLastStep(), 80.0));
    /// BOOST_CHECK(comparable(mapper.destinationFirstStep(), 182145.0));
    /// BOOST_CHECK(comparable(mapper.destinationLastStep(), 182935.0));

    /// BOOST_CHECK_EQUAL(stepMappers.size(), size_t(1));
    /// BOOST_CHECK(comparable(stepMappers[0].destination(1.0), 1.0));
    /// BOOST_CHECK(comparable(stepMappers[0].destination(80.0), 80.0));

    /// // Ad the same mapping again. Should give equal results.
    /// mappings.push_back(mappings.front());

    /// stepMap(mappings, &dimension, &mapper, &stepMappers);

    /// mappings.push_back(DimensionStepMapping(
    ///      Dimension(Space, values), &aMapper));

    /// BOOST_CHECK_EQUAL(dimension.meaning(), Space);
    /// BOOST_CHECK_EQUAL(dimension.discretisation(), RegularDiscretisation);
    /// BOOST_CHECK_EQUAL(dimension.coordinateType(), NumericalCoordinates);
    /// BOOST_CHECK_EQUAL(dimension.nrValues(), size_t(3));
    /// BOOST_CHECK_EQUAL(dimension.value<size_t>(0), size_t(1));
    /// BOOST_CHECK_EQUAL(dimension.value<size_t>(1), size_t(80));
    /// BOOST_CHECK_EQUAL(dimension.value<size_t>(2), size_t(1));

    /// BOOST_CHECK(comparable(mapper.sourceFirstStep(), 1.0));
    /// BOOST_CHECK(comparable(mapper.sourceLastStep(), 80.0));
    /// BOOST_CHECK(comparable(mapper.destinationFirstStep(), 182145.0));
    /// BOOST_CHECK(comparable(mapper.destinationLastStep(), 182935.0));

    /// BOOST_CHECK_EQUAL(stepMappers.size(), size_t(2));
    /// BOOST_CHECK(comparable(stepMappers[0].destination(1.0), 1.0));
    /// BOOST_CHECK(comparable(stepMappers[0].destination(80.0), 80.0));
    /// BOOST_CHECK(comparable(stepMappers[1].destination(1.0), 1.0));
    /// BOOST_CHECK(comparable(stepMappers[1].destination(80.0), 80.0));
  }

  // {
  //   std::vector<DimensionStepMapping> mappings;

  //   std::vector<size_t> values;
  //   values.push_back(1);
  //   values.push_back(100);
  //   values.push_back(1);

  //   // Y coordinates range from large values to small values.
  //   dal::StepMapper aMapper(1.0, 100.0, 327875.0, 326885.0);

  //   mappings.push_back(DimensionStepMapping(Dimension(Space, values),
  //        &aMapper));
  //   mappings.push_back(mappings.front());

  //   Dimension dimension;
  //   dal::StepMapper mapper;
  //   std::vector<StepMapper> stepMappers;

  //   stepMap(mappings, &dimension, &mapper, &stepMappers);

  //   BOOST_CHECK_EQUAL(dimension.meaning(), Space);
  //   BOOST_CHECK_EQUAL(dimension.discretisation(), RegularDiscretisation);
  //   BOOST_CHECK_EQUAL(dimension.coordinateType(), NumericalCoordinates);
  //   BOOST_CHECK_EQUAL(dimension.nrValues(), size_t(3));
  //   BOOST_CHECK_EQUAL(dimension.value<size_t>(0), size_t(1));
  //   BOOST_CHECK_EQUAL(dimension.value<size_t>(1), size_t(100));
  //   BOOST_CHECK_EQUAL(dimension.value<size_t>(2), size_t(1));

  //   BOOST_CHECK(comparable(mapper.sourceFirstStep(), 1.0));
  //   BOOST_CHECK(comparable(mapper.sourceLastStep(), 100.0));
  //   BOOST_CHECK(comparable(mapper.destinationFirstStep(), 327875.0));
  //   BOOST_CHECK(comparable(mapper.destinationLastStep(), 326885.0));

  //   BOOST_CHECK_EQUAL(stepMappers.size(), size_t(2));
  //   BOOST_CHECK(comparable(stepMappers[0].destination(1.0), 1.0));
  //   BOOST_CHECK(comparable(stepMappers[0].destination(100.0), 100.0));
  //   BOOST_CHECK(comparable(stepMappers[1].destination(1.0), 1.0));
  //   BOOST_CHECK(comparable(stepMappers[1].destination(100.0), 100.0));
  // }
}


// BOOST_AUTO_TEST_CASE(time_step_map)
// {
//   using namespace dal;
// 
//   namespace bp = boost::posix_time;
//   namespace bg = boost::gregorian;
// 
//   {
//     // Dataset A:
//     // 1 - 28
//     // 20060201 - 20060228 (days)
//     //
//     // Dataset B:
//     // 1 - 365
//     // 20060101 - 20061231 (days)
//     //
//     // Common dimension:
//     // 20060101 - 20061231 (days)
//     std::vector<DimensionTimeStepMapping> mappings;
//     std::vector<size_t> values;
//     values.push_back(1);
//     values.push_back(28);
//     values.push_back(1);
// 
//     dal::TimeStepMapper mapperA(1,
//          bp::ptime(bg::date(2006, boost::gregorian::Feb, 1),
//          bp::time_duration(0, 0, 0, 0)), bp::time_duration(24, 0, 0, 0));
//     dal::TimeStepMapper mapperB(1,
//          bp::ptime(bg::date(2006, boost::gregorian::Jan, 1),
//          bp::time_duration(0, 0, 0, 0)), bp::time_duration(24, 0, 0, 0));
// 
//     mappings.push_back(DimensionTimeStepMapping(Dimension(Time, values),
//          &mapperA));
//     values[1] = 365;
//     mappings.push_back(DimensionTimeStepMapping(Dimension(Time, values),
//          &mapperB));
// 
//     Dimension dimension;
//     dal::TimeStepMapper mapper;
//     std::vector<StepMapper> stepMappers;
// 
//     timeStepMap(mappings, &dimension, &mapper, &stepMappers);
// 
//     BOOST_CHECK_EQUAL(dimension.meaning(), Time);
//     BOOST_CHECK_EQUAL(dimension.coordinateType(), NumericalCoordinates);
//     BOOST_CHECK_EQUAL(dimension.discretisation(), RegularDiscretisation);
//     BOOST_CHECK_EQUAL(dimension.nrValues(), size_t(3));
//     BOOST_CHECK_EQUAL(dimension.value<size_t>(0), size_t(1));
//     BOOST_CHECK_EQUAL(dimension.value<size_t>(1), size_t(365));
//     BOOST_CHECK_EQUAL(dimension.value<size_t>(2), size_t(1));
// 
//     BOOST_CHECK(comparable(mapper.index(), 1.0));
//     BOOST_CHECK(mapper.time() ==
//          bp::ptime(bg::date(2006, boost::gregorian::Jan, 1),
//                    bp::time_duration(0, 0, 0, 0)));
//     BOOST_CHECK(mapper.duration() == bp::time_duration(24, 0, 0, 0));
// 
//     BOOST_CHECK_EQUAL(stepMappers.size(), size_t(2));
//     BOOST_CHECK(comparable(stepMappers[0].destination(1.0), -30.0));
//     BOOST_CHECK(comparable(stepMappers[0].destination(32.0), 1.0));
//     BOOST_CHECK(comparable(stepMappers[0].destination(59.0), 28.0));
// 
//     BOOST_CHECK(comparable(stepMappers[1].destination(1.0), 1.0));
//     BOOST_CHECK(comparable(stepMappers[1].destination(365.0), 365.0));
//   }
// 
//   {
//     // Dataset temperature:
//     // 1 - 10
//     // 20050210 10 stappen van een dag.
//     //
//     // Dataset discharge:
//     // 1 - 40
//     // 20050210 40 stappen van 6 uur.
// 
//     // EFAS voorbeeld zowel: dag als 6 uurs data te vinden:
//     // - begin  2005-02-10T00:00:00
//     // - temp temperature op dag basis in 10 tijdstappen (map-stack).
//     // - Q    discharge op 6 uur basis in 40 tijdstappen (map-stack).
//     // - geef temperature en discharge op  2005-02-13T18:00:00
//     std::vector<DimensionTimeStepMapping> mappings;
//     std::vector<size_t> values;
//     values.push_back(1);
//     values.push_back(10);
//     values.push_back(1);
// 
//     dal::TimeStepMapper mapperA(1,
//          bp::ptime(bg::date(2005, boost::gregorian::Feb, 11),
//          bp::time_duration(0, 0, 0, 0)), bp::time_duration(24, 0, 0, 0));
//     dal::TimeStepMapper mapperB(1,
//          bp::ptime(bg::date(2005, boost::gregorian::Feb, 10),
//          bp::time_duration(6, 0, 0, 0)), bp::time_duration(6, 0, 0, 0));
// 
//     mappings.push_back(DimensionTimeStepMapping(Dimension(Time, values),
//          &mapperA));
//     values[1] = 40;
//     mappings.push_back(DimensionTimeStepMapping(Dimension(Time, values),
//          &mapperB));
// 
//     Dimension dimension;
//     dal::TimeStepMapper mapper;
//     std::vector<StepMapper> stepMappers;
// 
//     timeStepMap(mappings, &dimension, &mapper, &stepMappers);
// 
//     BOOST_CHECK_EQUAL(dimension.meaning(), Time);
//     BOOST_CHECK_EQUAL(dimension.coordinateType(), NumericalCoordinates);
//     BOOST_CHECK_EQUAL(dimension.discretisation(), RegularDiscretisation);
//     BOOST_CHECK_EQUAL(dimension.nrValues(), size_t(3));
//     BOOST_CHECK_EQUAL(dimension.value<size_t>(0), size_t(1));
//     BOOST_CHECK_EQUAL(dimension.value<size_t>(1), size_t(40));
//     BOOST_CHECK_EQUAL(dimension.value<size_t>(2), size_t(1));
// 
//     BOOST_CHECK(comparable(mapper.index(), 1.0));
//     BOOST_CHECK(mapper.time() ==
//          bp::ptime(bg::date(2005, boost::gregorian::Feb, 10),
//                    bp::time_duration(6, 0, 0, 0)));
//     BOOST_CHECK(mapper.duration() == bp::time_duration(6, 0, 0, 0));
// 
//     BOOST_CHECK_EQUAL(stepMappers.size(), size_t(2));
//     BOOST_CHECK(comparable(stepMappers[0].destination( 1.0),  0.25));
//     BOOST_CHECK(comparable(stepMappers[0].destination( 4.0),  1.0));
//     BOOST_CHECK(comparable(stepMappers[0].destination(40.0), 10.0));
// 
//     BOOST_CHECK(comparable(stepMappers[1].destination( 1.0),  1.0));
//     BOOST_CHECK(comparable(stepMappers[1].destination(40.0), 40.0));
//   }
// 
//   {
//     // 20060101, 1 - 100
//     // 20060201, 1 - 100
// 
//     std::vector<DimensionTimeStepMapping> mappings;
//     std::vector<size_t> values;
//     values.push_back(1);
//     values.push_back(100);
//     values.push_back(1);
// 
//     dal::TimeStepMapper mapperA(1,
//          bp::ptime(bg::date(2006, boost::gregorian::Jan, 1),
//          bp::time_duration(0, 0, 0, 0)), bp::time_duration(24, 0, 0, 0));
//     dal::TimeStepMapper mapperB(1,
//          bp::ptime(bg::date(2006, boost::gregorian::Feb, 1),
//          bp::time_duration(0, 0, 0, 0)), bp::time_duration(24, 0, 0, 0));
// 
//     mappings.push_back(DimensionTimeStepMapping(Dimension(Time, values),
//          &mapperA));
//     mappings.push_back(DimensionTimeStepMapping(Dimension(Time, values),
//          &mapperB));
// 
//     Dimension dimension;
//     dal::TimeStepMapper mapper;
//     std::vector<StepMapper> stepMappers;
// 
//     timeStepMap(mappings, &dimension, &mapper, &stepMappers);
// 
//     BOOST_CHECK_EQUAL(dimension.meaning(), Time);
//     BOOST_CHECK_EQUAL(dimension.coordinateType(), NumericalCoordinates);
//     BOOST_CHECK_EQUAL(dimension.discretisation(), RegularDiscretisation);
//     BOOST_CHECK_EQUAL(dimension.nrValues(), size_t(3));
//     BOOST_CHECK_EQUAL(dimension.value<size_t>(0), size_t(1));
//     BOOST_CHECK_EQUAL(dimension.value<size_t>(1), size_t(131));
//     BOOST_CHECK_EQUAL(dimension.value<size_t>(2), size_t(1));
// 
//     BOOST_CHECK(comparable(mapper.index(), 1.0));
//     BOOST_CHECK(mapper.time() ==
//          bp::ptime(bg::date(2006, boost::gregorian::Jan, 1),
//                    bp::time_duration(0, 0, 0, 0)));
//     BOOST_CHECK(mapper.duration() == bp::time_duration(24, 0, 0, 0));
// 
//     BOOST_CHECK_EQUAL(stepMappers.size(), size_t(2));
//     BOOST_CHECK(comparable(stepMappers[0].destination(  1.0),   1.0));
//     BOOST_CHECK(comparable(stepMappers[0].destination(100.0), 100.0));
// 
//     BOOST_CHECK(comparable(stepMappers[1].destination(32.0),  1.0));
//   }
// }
