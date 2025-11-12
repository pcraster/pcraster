#ifndef INCLUDED_CALC_MAP2CSF

#include "csftypes.h"
#include "calctypes.h"
#include "calc_vs.h"



namespace calc {
VS csfVs2vs(CSF_VS vs);
CSF_CR biggestCellRepr(VS vsSet);
CSF_VS vs2CsfVs(VS vs);
size_t bytesPerCell(VS vs);

}

#endif
