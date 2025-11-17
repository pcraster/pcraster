#ifndef INCLUDED_PCRXML_NEWDATASUBTYPE
#define INCLUDED_PCRXML_NEWDATASUBTYPE

#include "stddefx.h"
#include "csftypes.h"


namespace pcrxml {

class DataTypeDTD;

//! create pcrxml::DataTypeEnum single CSF_VS value
DataTypeDTD* newDataType(CSF_VS vs);

//! create sub element of Data
template<class T> T* newDataSubType(CSF_VS vs) {
  T* n = new T();
  n->dataTypeDTD = newDataType(vs);
  return n;
}

} // namespace pcrxml

#endif
