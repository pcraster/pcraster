#include "stddefx.h"

#include "app.h"
#include "api.h"
#include "api_p.h"

#include <math.h>

/* only used and in calc::calcapi.c
 */
void BootTestApi(
		 double cellSize, int doesYincT2B)
{
	/* init stuff */
	REAL8 x  = cellSize;
	if (!appUnitTrue)
		x = (REAL8)1;

/* 	testApiNrRows = (int)nrRows; */
/* 	testApiNrCols = (int)nrCols; */


	testApiSide     = x;
	testApiArea     = x * x;
	testApiDiagonal = sqrt(2.0) * x;
	testApiYproj    = doesYincT2B ? 1 : -1;
  testApiInit     = 1;
}
