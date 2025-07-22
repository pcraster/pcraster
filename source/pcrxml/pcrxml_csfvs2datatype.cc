#include "stddefx.h"
#include "pcrxml_csfvs2datatype.h"
#include "pcrgenxml_datatypeenum.h"



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
