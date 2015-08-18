#include "pcrlinkin.h"
#include "csftypes.h"

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
  float *result =  (float *)linkInTransferArray[0];

  const float *spatial    =(const float *)linkInTransferArray[1];
  const float nonSpatial  =((const float *)linkInTransferArray[2])[0];

  int nrCells=nrRows*nrCols;
  int c;
  for(c=0; c < nrCells; c++) {
    if (IS_MV_REAL4(spatial+c))
      SET_MV_REAL4(result+c);
    else
      result[c]=spatial[c]+nonSpatial;
  }
  /* no error */
  return 0;
}
