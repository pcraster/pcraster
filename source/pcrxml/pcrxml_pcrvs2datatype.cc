#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_PCRVS2DATATYPE
#include "pcrxml_pcrvs2datatype.h"
#define INCLUDED_PCRXML_PCRVS2DATATYPE
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

//! translates PCR_VS to pcrxml::DataTypeEnum
pcrxml::DataTypeEnum pcrxml::pcrVs2DataType(PCR_VS vs)
{
  switch(vs) {
    case VS_B     : return pcrxml::DataTypeEnum::Boolean;
    case VS_N     : return pcrxml::DataTypeEnum::Nominal;
    case VS_O     : return pcrxml::DataTypeEnum::Ordinal;
    case VS_S     : return pcrxml::DataTypeEnum::Scalar;
    case VS_D     : return pcrxml::DataTypeEnum::Directional;
    case VS_L     : return pcrxml::DataTypeEnum::Ldd;
    default       : return pcrxml::DataTypeEnum::Unknown;
  }
}

//! translates PCR_VS to pcrxml::DataTypeEnum
PCR_VS pcrxml::dataType2PcrVs(pcrxml::DataTypeEnum d)
{
  switch(d()) {
    case pcrxml::DataTypeEnum::Boolean: return VS_B;
    case pcrxml::DataTypeEnum::Nominal: return VS_N;
    case pcrxml::DataTypeEnum::Ordinal: return VS_O;
    case pcrxml::DataTypeEnum::Scalar: return VS_S;
    case pcrxml::DataTypeEnum::Directional: return VS_D;
    case pcrxml::DataTypeEnum::Ldd:        return VS_L;
    default:              PRECOND(FALSE);
                            return VS_UNKNOWN;
  }
}

