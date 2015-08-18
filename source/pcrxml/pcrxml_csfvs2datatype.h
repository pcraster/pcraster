#ifndef INCLUDED_PCRXML_CSFVS2DATATYPE
#define INCLUDED_PCRXML_CSFVS2DATATYPE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
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

pcrxml::DataTypeEnum csfVs2DataType(CSF_VS vs);

} // namespace pcrxml

#endif
