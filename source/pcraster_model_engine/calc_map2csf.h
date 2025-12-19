#ifndef INCLUDED_CALC_MAP2CSF
#define INCLUDED_CALC_MAP2CSF

#include "csftypes.h"
#include "pcraster_model_engine_export.h"
#include "calc_types.h"
#include "calc_vs.h"


namespace calc {
PCR_ME_EXPORT VS csfVs2vs(CSF_VS vs);
CSF_CR biggestCellRepr(VS vsSet);
PCR_ME_EXPORT CSF_VS vs2CsfVs(VS vs);
size_t bytesPerCell(VS vs);

}

#endif
