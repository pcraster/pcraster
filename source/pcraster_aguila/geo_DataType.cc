#include "geo_DataType.h"

// External headers.
#include <cassert>

// Project headers.

// Module headers.



namespace geo {

// Code that is private to this module.
namespace detail {

} // namespace detail



// DataType dataType(const com::PathName& pathName)
// {
//   DataType dataType = DT_INVALID;
// 
//   if(isCSFStack(pathName)) {
//     dataType = STACK;
//   }
//   else if(isTimeSeriesFile(pathName)) {
//     dataType = TIMESERIES;
//   }
//   else if(isModelScriptFile(pathName)) {
//     dataType = MODELSCRIPT;
//   }
//   else if(isColumnFile(pathName)) {
//     dataType = POINTS;
//   }
//   else if(isBlock(pathName)) {
//     dataType = BLOCK;
//   }
// 
//   return dataType;
// }



bool isSpatial(
         DataType dataType)
{
  return dataType == STACK || dataType == FEATURE;
}



bool isTimeSeries(DataType dataType)
{
  return dataType == TIMESERIES;
}



bool isModelScript(DataType dataType)
{
  return dataType == MODELSCRIPT;
}



std::string dataTypeToStr(const DataType& dataType)
{
  std::string result;

  switch(dataType) {
    case STACK: {
      result = "STACK";
      break;
    }
    case BLOCK: {
      result = "BLOCK";
      break;
    }
    case TIMESERIES: {
      result = "TIMESERIES";
      break;
    }
    case MODELSCRIPT: {
      result = "MODELSCRIPT";
      break;
    }
    case FEATURE: {
      result = "FEATURE";
      break;
    }
    case VECTOR: {
      result = "VECTOR";
      break;
    }
    case DT_INVALID: {
      result = "INVALID";
      break;
    }
    default: {
      assert(false);
      result = "UNKOWN";        // Never reached.
      break;
    }
  }

  return result;
}

} // namespace geo

