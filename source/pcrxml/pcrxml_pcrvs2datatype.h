#ifndef INCLUDED_PCRXML_CSFVS2DATATYPE
#define INCLUDED_PCRXML_CSFVS2DATATYPE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRDATATYPE
#include "pcrdatatype.h"
#define INCLUDED_PCRDATATYPE
#endif

// Module headers.
#ifndef INCLUDED_PCRGENXML_DATATYPEENUM
#include "pcrgenxml_datatypeenum.h"
#define INCLUDED_PCRGENXML_DATATYPEENUM
#endif


namespace pcrxml {


//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

DataTypeEnum pcrVs2DataType(PCR_VS vs);
PCR_VS       dataType2PcrVs(DataTypeEnum d);


} // namespace pcrxml

#endif
