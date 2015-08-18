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
#ifndef INCLUDED_MLDD_MLDDLINK
#include "mldd_mlddlink.h"
#define INCLUDED_MLDD_MLDDLINK
#endif



//------------------------------------------------------------------------------

namespace mldd {

static calc::ModelLink* createMlddLink()
{
  return new MlddLink();
}

} // namespace mldd


//------------------------------------------------------------------------------
static calc::PCR_EXTERNAL_MODELLINK_SYNOPSIS MLDDSynopsis = {
  "mldd",
  &mldd::createMlddLink
};

static calc::PCR_EXTERNAL_MODELLINK_LIST modelLinkList;

extern "C" PCR_DLL_FUNC(calc::PCR_EXTERNAL_MODELLINK_LIST*) GetModelLinkList(void)
{
  modelLinkList.apiVersionNr = 0;
  modelLinkList.nrModelLinks = 1;
  modelLinkList.modelLinkSynopsisList[0] = MLDDSynopsis;

  return &modelLinkList;
}
