#ifndef INCLUDED_CALC_XMLDATATYPE
#define INCLUDED_CALC_XMLDATATYPE

#include "stddefx.h"
#include "calc_datatype.h"



namespace pcrxml {
  class DataType;
  class SpatialType;
  class FieldValueOrType;
  class FieldType;
  class FieldTypeOfValue;
}



namespace calc {

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

pcrxml::DataType    toXMLDataType(DataType const& dt);
pcrxml::FieldTypeOfValue toXMLFieldTypeOfValue(DataType const& dt);
pcrxml::FieldType        toXMLFieldType(DataType const& dt);
DataType            xml2DataType(pcrxml::FieldValueOrType const& f);
DataType            xml2DataType(pcrxml::FieldType        const& f);
pcrxml::SpatialType st2XML(ST st);

} // namespace calc

#endif
