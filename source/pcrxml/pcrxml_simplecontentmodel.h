#ifndef INCLUDED_PCRXML_SIMPLECONTENTMODEL
#define INCLUDED_PCRXML_SIMPLECONTENTMODEL



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Standard library headers.

// Library headers.
#ifndef INCLUDED_PCRXML_ELEMENT
#include "pcrxml_element.h"
#define INCLUDED_PCRXML_ELEMENT
#endif

#ifndef INCLUDED_PCRXML_PCDATAELEMENT
#include "pcrxml_pcdataelement.h"
#define INCLUDED_PCRXML_PCDATAELEMENT
#endif

class QDomNode;

namespace pcrxml {

//! The Simple Content Model from XML Schema: no child elements, only plain text
typedef PCDATAElement SimpleContentModel;

//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace pcrxml

#endif
