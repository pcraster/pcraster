#ifndef INCLUDED_PCRXML_CSFVS2DATATYPE
#define INCLUDED_PCRXML_CSFVS2DATATYPE

#include "stddefx.h"
#include "pcrdatatype.h"
#include "pcrgenxml_datatypeenum.h"


namespace pcrxml {


//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

DataTypeEnum pcrVs2DataType(PCR_VS vs);
PCR_VS       dataType2PcrVs(const DataTypeEnum& d);


} // namespace pcrxml

#endif
