#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_CSFVS2DATATYPE
#include "pcrxml_csfvs2datatype.h"
#define INCLUDED_PCRXML_CSFVS2DATATYPE
#endif

// Library headers.

// PCRaster library headers.
// Module headers.
#ifndef INCLUDED_PCRGENXML_DATATYPEENUM
#include "pcrgenxml_datatypeenum.h"
#define INCLUDED_PCRGENXML_DATATYPEENUM
#endif



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//! translates CSF_VS to pcrxml::DataTypeEnum
pcrxml::DataTypeEnum pcrxml::csfVs2DataType(CSF_VS vs)
{
  switch(vs) {
    case VS_BOOLEAN       : return pcrxml::DataTypeEnum::Boolean;
    case VS_NOMINAL       : return pcrxml::DataTypeEnum::Nominal;
    case VS_ORDINAL       : return pcrxml::DataTypeEnum::Ordinal;
    case VS_SCALAR        : return pcrxml::DataTypeEnum::Scalar;
    case VS_DIRECTION     : return pcrxml::DataTypeEnum::Directional;
    case VS_LDD           : return pcrxml::DataTypeEnum::Ldd;
    // start praying
    case VS_CONTINUOUS    : return pcrxml::DataTypeEnum::Scalar;
    case VS_CLASSIFIED    : return pcrxml::DataTypeEnum::Nominal;
    default               : return pcrxml::DataTypeEnum::Unknown;
  }
}
