#ifndef INCLUDED_OLDCALC_NEWXMLDATASUBTYPE
#define INCLUDED_OLDCALC_NEWXMLDATASUBTYPE

#include "stddefx.h"
#include "pcrgenxml_data.h"
#include "pcrxml_newdatasubtype.h"
#include "calc_map2csf.h"



namespace calc {

//! create new xml data sub info with DataType node
template<class T> T* newDataSubType(VS vs) {
  return pcrxml::newDataSubType<T>(vs2CsfVs(vs));
 }

}

#endif
