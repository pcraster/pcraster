#pragma once


namespace calc {
  class Field;
}



namespace pcraster_multicore::python {



calc::Field* newNonSpatialScalar(const double value);

calc::Field* newNonSpatialNominal(const int value);

calc::Field* newNonSpatialBoolean(const bool value);



} // namespace pcraster_multicore::python

