#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_NEWDATASUBTYPE
#include "pcrxml_newdatasubtype.h"
#define INCLUDED_PCRXML_NEWDATASUBTYPE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_PCRGENXML_DATATYPEDTD
#include "pcrgenxml_datatypedtd.h"
#define INCLUDED_PCRGENXML_DATATYPEDTD
#endif
#ifndef INCLUDED_PCRXML_CSFVS2DATATYPE
#include "pcrxml_csfvs2datatype.h"
#define INCLUDED_PCRXML_CSFVS2DATATYPE
#endif

pcrxml::DataTypeDTD* pcrxml::newDataType(CSF_VS vs)
{
  DataTypeDTD *n= new DataTypeDTD();
  n->value= csfVs2DataType(vs);
  return n;
}
