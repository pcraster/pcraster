#ifndef INCLUDED_CALC_NEWXMLDATASUBTYPE
#define INCLUDED_CALC_NEWXMLDATASUBTYPE

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

#ifndef INCLUDED_PCRGENXML_DATA
#include "pcrgenxml_data.h"
#define INCLUDED_PCRGENXML_DATA
#endif

#ifndef INCLUDED_PCRXML_NEWDATASUBTYPE
#include "pcrxml_newdatasubtype.h"
#define INCLUDED_PCRXML_NEWDATASUBTYPE
#endif

// Module headers.

#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif

namespace calc {

//! create new xml data sub info with DataType node
template<class T> T* newDataSubType(VS vs) {
  return pcrxml::newDataSubType<T>(vs2CsfVs(vs));
 }

}

#endif
