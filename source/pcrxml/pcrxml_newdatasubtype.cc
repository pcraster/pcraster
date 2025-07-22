#include "stddefx.h"
#include "pcrxml_newdatasubtype.h"
#include "pcrgenxml_datatypedtd.h"
#include "pcrxml_csfvs2datatype.h"

pcrxml::DataTypeDTD* pcrxml::newDataType(CSF_VS vs)
{
  auto *n= new DataTypeDTD();
  n->value= csfVs2DataType(vs);
  return n;
}
