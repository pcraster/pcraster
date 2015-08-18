#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_CALC_MODELLINK
#include "calc_modellink.h"
#define INCLUDED_CALC_MODELLINK
#endif

// Module headers.
#ifndef INCLUDED_PT_MOCLINK
#include "pt_MOCLink.h"
#define INCLUDED_PT_MOCLINK
#endif



//------------------------------------------------------------------------------

namespace pt {

static calc::ModelLink* createMOCLink()
{
  return new MOCLink();
}

} // namespace pt


//------------------------------------------------------------------------------
static calc::PCR_EXTERNAL_MODELLINK_SYNOPSIS MOCSynopsis = {
  "moc",
  &pt::createMOCLink
};

static calc::PCR_EXTERNAL_MODELLINK_LIST modelLinkList;

extern "C" PCR_DLL_C calc::PCR_EXTERNAL_MODELLINK_LIST*  GetModelLinkList(void)
{
  modelLinkList.apiVersionNr = 0;
  modelLinkList.nrModelLinks = 1;
  modelLinkList.modelLinkSynopsisList[0] = MOCSynopsis;

  return &modelLinkList;
}
