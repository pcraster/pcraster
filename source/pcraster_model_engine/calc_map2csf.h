#ifndef INCLUDED_CALC_MAP2CSF

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

#include "pcraster_model_engine_export.h"

#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

namespace calc {
PCR_ME_EXPORT VS csfVs2vs(CSF_VS vs);
CSF_CR biggestCellRepr(VS vsSet);
PCR_ME_EXPORT CSF_VS vs2CsfVs(VS vs);
size_t bytesPerCell(VS vs);

}

#endif
