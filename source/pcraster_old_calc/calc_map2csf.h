#ifndef INCLUDED_CALC_MAP2CSF

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

#ifndef INCLUDED_CALCTYPES
#include "calctypes.h"
#define INCLUDED_CALCTYPES
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

namespace calc {
VS csfVs2vs(CSF_VS vs);
CSF_CR biggestCellRepr(VS vsSet);
CSF_VS vs2CsfVs(VS vs);
size_t bytesPerCell(VS vs);

}

#endif
