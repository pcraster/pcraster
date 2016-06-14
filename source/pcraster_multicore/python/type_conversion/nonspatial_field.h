#pragma once


namespace calc {
  class Field;
}


namespace pcraster_multicore {
namespace python {



calc::Field* newNonSpatialScalar(const double value);

calc::Field* newNonSpatialNominal(const int value);

calc::Field* newNonSpatialBoolean(const bool value);



} // namespace python
} // namespace pcraster_multicore
