#include "pcrlinkin.h"

static int    nrRows=0;
static int    nrCols=0;
static double cellSize;
static double xLowerLeftCorner;
static double yLowerLeftCorner;
static int    timeStep;

PCR_DLL_FUNC(void) pcr_LinkInRunContext(
    int    am_nrRows,
    int    am_nrCols,
    double am_cellSize,
    double am_xLowerLeftCorner,
    double am_yLowerLeftCorner,
    int    am_timeStep)
{
 nrRows           =am_nrRows;
 nrCols           =am_nrCols;
 cellSize         =am_cellSize;
 xLowerLeftCorner =am_xLowerLeftCorner;
 yLowerLeftCorner =am_yLowerLeftCorner;
 timeStep         =am_timeStep;
}

PCR_DLL_FUNC (const char *) pcr_LinkInExecute(
      const char *xmlNotUsed,
      LinkInTransferArray linkInTransferArray)
{
  unsigned char *result=(unsigned char *)linkInTransferArray[0];
  unsigned char value;
  int r,c,cell=0;
  for(r=0;r<nrRows; r++) {
   value=r%2;
   for(c=0;c<nrCols; c++) {
     result[cell]=value;
     cell++;
     value=!value;
   }
  }
  /* no error */
  return 0;
}
