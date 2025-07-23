#include "stddefx.h"
#include "calc_modellink.h"
#include "pt_MOCLink.h"



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
