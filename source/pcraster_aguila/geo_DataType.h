#ifndef INCLUDED_GEO_DATATYPE
#define INCLUDED_GEO_DATATYPE

#include <string>




namespace geo {

  using DataType = enum DataType {
    DT_INVALID,
    STACK,
    FEATURE,
    VECTOR,
    BLOCK,
    TIMESERIES,
    MODELSCRIPT
  };

  // geo::DataType    dataType            (const com::PathName& pathName);

  bool             isSpatial           (DataType dataType);

  bool             isTimeSeries        (DataType dataType);

  bool             isModelScript       (DataType dataType);

  std::string      dataTypeToStr       (const DataType& dataType);

}

#endif
