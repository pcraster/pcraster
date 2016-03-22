#include <assert.h>
#include "stddefx.h"

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "csf.h"
#include <math.h>

/* apps. called */
#include "app.h"

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/*********************/
/* LOCAL DEFINITIONS */
/*********************/
#define LDD_MAX 9

/******************/
/* IMPLEMENTATION */
/******************/

/* Checks a value on being classified.
 * Returns TRUE if value scale is classified, FALSE otherwise.
 */
 BOOL AppIsClassified(CSF_VS vs)	/* value scale to check */
 {
 	return(vs == VS_LDD || vs == VS_BOOLEAN || vs == VS_NOMINAL ||
 		vs == VS_ORDINAL || vs == VS_CLASSIFIED);
 }

/* Checks a value on being valid for given valuescale and cellRepresentation.
 * The value is tested on being in the domain of the valueScale
 * and CellRepresentation. If valueScale equals VS_UNDEFINED then a test
 * is preformed whether it is a valid double.
 * Prints error message with ErrorNested call containing v and
 * the name of the valuescale.
 * Returns 1 in case of an illegal value for valueScale, 0 otherwise.
 */
 int AppCheckVal(
 	const char *v,			/* token value to read */
 	CSF_VS valueScale,		/* type 2 value scale or VS_UNDEFINED */
 	CSF_CR cellRepr)		/* cell representation, AppDefaultCellRepr()
 	                                 * is used if this argument has value
 	                                 * CR_UNDEFINED
 	                                 */
{
    	UINT1 uint1Val;
    	INT4  int4Val;
    	REAL4 real4Val;
    	REAL8 real8Val;
    	int     result;

    	if (cellRepr == CR_UNDEFINED)
    		cellRepr = AppDefaultCellRepr(valueScale);

 	switch(valueScale)
 	{
 		case VS_BOOLEAN:
 			result = (CnvrtUINT1(&uint1Val, v) &&
 			         (uint1Val == 0 || uint1Val == 1));
 			break;
 		case VS_NOMINAL:
 		case VS_ORDINAL:
 		          if (appLarge)
 	                   result = CnvrtINT4(&int4Val, v); 
 		          else
 			   result = CnvrtUINT1(&uint1Val, v); 
 			break;
 		case VS_LDD:
 			result = (CnvrtUINT1(&uint1Val, v) &&
 			          (UINT1)0 < uint1Val && uint1Val <= (UINT1)LDD_MAX);
 			break;
#  ifdef NEVER
/* VECTOR is not supported */
 		case VS_VECTOR:
 			if(CnvrtREAL8(&real8Val, v))
#   endif
 		case VS_SCALAR:
 		case VS_DIRECTION:
 		        if (appDouble)
 			 result = CnvrtREAL8(&real8Val, v);
 			else
 			 result = CnvrtREAL4(&real4Val, v);
 			break;
 		case VS_UNDEFINED:
 			 result = CnvrtREAL8(&real8Val, v);
 			break;
  default:{
    assert(0);  // Shut up compiler
  }
 	}
	if (!result)
	{
		const char *s = "";
		switch(valueScale) {
 		case VS_SCALAR:
 		case VS_DIRECTION:
 		        if (!appDouble && CnvrtREAL8(&real8Val, v))
 		         /* legal value but not in single */
 		          s = " in single cell representation";
 		        break;
  case VS_NOMINAL:
  case VS_ORDINAL:
    if (!appLarge && CnvrtINT4(&int4Val, v)){
      /* legal value but not in small */
      s = " in small cell representation";
    }
    break;
  default:{
    assert(0);  // Shut up compiler
  }
 		}
 		if (valueScale != VS_UNDEFINED)
		  ErrorNested("'%s' is not a legal %s value %s",
		     v,RstrValueScale(valueScale),s);
		else
		  ErrorNested("'%s' is not a legal numerical value",v);
	}
	return !result;
}

/* Checks a value on being valid for given valuescale and cellRepresentation.
 * The value is tested on being in the domain of the valueScale
 * and CellRepresentation. If valueScale equals VS_UNDEFINED then a test
 * is preformed whether it is a valid double.
 * Prints error message with ErrorNested call containing v and
 * the name of the valuescale.
 * Returns 1 in case of an illegal value for valueScale, 0 otherwise.
 */
 int AppCheckValNum(
 	REAL8     v,			/* token value to read */
 	CSF_VS valueScale,		/* type 2 value scale or VS_UNDEFINED */
 	CSF_CR cellRepr)		        /* cell representation, AppDefaultCellRepr()
 	                                 * is used if this argument has value
 	                                 * CR_UNDEFINED
 	                                 */
{
    	char buf[128];
    	(void)sprintf(buf,"%g",v);
	return AppCheckVal(buf, valueScale, cellRepr);
}
