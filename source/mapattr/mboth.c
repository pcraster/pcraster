#include <assert.h>
#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <math.h>
#include <string.h>
#include "csf.h"
#include "app.h"
#include "misc.h"
#include "currmenu.h"		/* Curr... */

/* apps. of this module called */
#include  "mapattr.h"
#include  "mboth.h"

/*************/
/* EXTERNALS */
/*************/

ATTRIBUTES *currAttr;

const char *itemNames[12] = {
 "number of rows      ",
 "number of columns   ",
 "data type           ",
 "cell representation ",
 "projection          ",
 "x upperleft corner  ",
 "y upperleft corner  ",
 "cell length         ",
 "angle (degrees)     ",
 "file id             ",
 "minimum legend value",
 "maximum legend value"
/*
  12345678901234567890
 */
};

const int itemNamesLen = 20;

/**********************/
/* LOCAL DECLARATIONS */
/**********************/
#define NOT_SET "*NOT SET*"
#define NOT_APPL "*NOT APPLICABLE*"

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */
/******************/

static void PrintFloat(
	char *buf,
	REAL8 v)
{
	if (floor(v) == v)
	 sprintf(buf, "%d.0",(int)v);
	else
	 sprintf(buf, "%g",v);
}

const char *AttrStr(
	const ATTRIBUTES *a,
	ATTR_NRS i)
{
   static char buf[32];
   switch(i) {
	case ATTR_nrRows:
	                 if (a->nrRows == MV_UINT4)
	                 	return NOT_SET;
	                 sprintf(buf,"%lu",(long unsigned int)a->nrRows); break;
	case ATTR_nrCols:
	                 if (a->nrCols == MV_UINT4)
	                 	return NOT_SET;
			 sprintf(buf,"%lu",(long unsigned int)a->nrCols); break;
	case ATTR_valueScale: sprintf(buf,"%s",RstrValueScale(a->valueScale)); break;
	case ATTR_cellRepr: sprintf(buf,"%s",RstrCellRepr(a->cellRepr)); break;
	case ATTR_projection: sprintf(buf,"%s",MstrProjection(a->projection)); break;
	case ATTR_xUL: PrintFloat(buf,a->xUL); break;
	case ATTR_yUL: PrintFloat(buf,a->yUL); break;
	case ATTR_cellSize: PrintFloat(buf,a->cellSize); break;
	case ATTR_angle: PrintFloat(buf,a->angle); break;
	case ATTR_gisFileId:
			PRECOND(a->gisFileId != MV_UINT4);
	                sprintf(buf,"%lu",(long unsigned int)a->gisFileId); break;
	case ATTR_minVal:
			 if (IS_MV_REAL8(&(a->minVal)))
			   return NOT_APPL;
			 sprintf(buf,"%g",a->minVal); break;
	case ATTR_maxVal:
			 if (IS_MV_REAL8(&(a->maxVal)))
			   return NOT_APPL;
	                 sprintf(buf,"%g",a->maxVal); break;
        default: PRECOND(FALSE);
      }
      return (const char *)buf;
}

void GetAttrDouble(
        double *v,
	const ATTRIBUTES *a,
	ATTR_NRS i)
{
   switch(i) {
	case ATTR_nrRows: *v = a->nrRows;break;
	case ATTR_nrCols: *v = a->nrCols;break;
	case ATTR_valueScale: *v = a->valueScale;break;
	case ATTR_cellRepr: *v = a->cellRepr;break;
	case ATTR_projection: *v = a->projection;break;
	case ATTR_xUL: *v = a->xUL;break;
	case ATTR_yUL: *v = a->yUL;break;
	case ATTR_cellSize: *v = a->cellSize;break;
	case ATTR_angle: *v = a->angle;break;
	case ATTR_gisFileId: *v = a->gisFileId;break;
	case ATTR_minVal: COPY_REAL8(v,&(a->minVal)); break;
	case ATTR_maxVal: COPY_REAL8(v,&(a->maxVal)); break;
        default: PRECOND(FALSE);
      }
}

void SetAttrDouble(
	ATTRIBUTES *a,
        const double *v,
	ATTR_NRS i)
{
   switch(i) {
	case ATTR_nrRows: a->nrRows = (UINT4)*v;break;
	case ATTR_nrCols: a->nrCols = (UINT4)*v;break;
	case ATTR_valueScale: a->valueScale = (CSF_VS)*v;break;
	case ATTR_cellRepr: a->cellRepr = (CSF_CR)*v;break;
	case ATTR_projection: a->projection = (CSF_PT)*v;break;
	case ATTR_xUL: a->xUL = *v;break;
	case ATTR_yUL: a->yUL = *v;break;
	case ATTR_cellSize: a->cellSize = *v;break;
	case ATTR_angle: a->angle = *v;break;
	case ATTR_gisFileId: a->gisFileId = (UINT4)*v;break;
	case ATTR_minVal: COPY_REAL8(&(a->minVal),v); break;
	case ATTR_maxVal: COPY_REAL8(&(a->maxVal),v); break;
        default: PRECOND(FALSE);
      }
}

static BOOL CheckFileId(
	const char *s)
{
	INT4  v;
	return  CnvrtINT4(&v,s) && v >= 0;
}

static BOOL CheckRowCols(
	const char *s)
{
	INT4  v;
	return  CnvrtINT4(&v,s) && v > 0
#ifdef EVAL_VERSION
        && v <= 60
#endif
	;
}

static BOOL CheckUL(
	const char *s)
{
	REAL8  v;
	return CnvrtREAL8(&v,s);
}

static BOOL CheckCellSize(
	const char *s)
{
	double  v;
	return CnvrtREAL8(&v,s) && v > 0;
}

static BOOL CheckAngle(
	const char *s)
{
	double  v;
	return CnvrtREAL8(&v,s) && v >= -90 && v <= 90;
}

static BOOL CheckMinVal(
	const char *s)
{
	double v;
	return CnvrtREAL8(&v,s) && v <= currAttr->maxVal;
}

static BOOL CheckMaxVal(
	const char *s)
{
	double v;
	return CnvrtREAL8(&v,s) && v >= currAttr->minVal;
}

static int EditProjection(
	double *editValue, /* read-write */
	int yStart,
	int xStart)
{
	char op0[128],op1[128];
	char *options[2];
	int  i = (int)*editValue;
	options[0] = op0;
	options[1] = op1;
	(void)strcpy(options[0], MstrProjection(i));
	(void)strcpy(options[1], MstrProjection(!i));
	i = CurrGetRadioSelection((const char**)options, 2, yStart,xStart);
	if (i) /* the other one */
	    *editValue = i;
	return i == 1; /* the other one is selected, not 0 or ESC */
}

static int EditCellRepr(
	double *editValue, /* read-write */
	int yStart,
	int xStart)
{
	char op0[128],op1[128];
	int i;
	CSF_CR cr[2];
	char *options[2];
	options[0] = op0;
	options[1] = op1;
  switch(currAttr->valueScale) {
    case VS_BOOLEAN   :
    case VS_LDD       :
    case VS_SCALAR    :
    case VS_DIRECTION : return TRUE; /* keep it */
    case VS_NOMINAL   :
    case VS_ORDINAL   :
      cr[0] = (CSF_CR)*editValue;
      cr[1] = (cr[0] == CR_UINT1) ? CR_INT4 : CR_UINT1;
      break;
    default: {
      assert(0);  // Shut up compiler
    }
	};
	(void)strcpy(options[0], RstrCellRepr(cr[0]));
	(void)strcpy(options[1], RstrCellRepr(cr[1]));
	i = CurrGetRadioSelection((const char**)options, 2, yStart,xStart);
	if (i) /* the other one */
	{
	    *editValue = cr[1];
	    return TRUE;
	}
	return FALSE;
}

static int EditValueScale(
	double *editValue, /* read-write */
	int yStart,
	int xStart)
{
	char op0[128],op1[128];
	char op2[128],op3[128];
	char op4[128],op5[128];
	int i,j;
        CSF_VS setVs[6],vs[6] ={ VS_BOOLEAN, VS_NOMINAL, VS_ORDINAL,
			     VS_SCALAR,  VS_LDD, VS_DIRECTION };
	char *options[6];
	options[0] = op0;
	options[1] = op1;
	options[2] = op2;
	options[3] = op3;
	options[4] = op4;
	options[5] = op5;
	(void)strcpy(options[0], RstrValueScale((CSF_VS)*editValue));
	for (i= 0,j=1; i < 6; i++)
	 if (vs[i] != (CSF_VS)*editValue)
	 {
	   setVs[j] = vs[i];
	   (void)strcpy(options[j++], RstrValueScale(vs[i]));
	 }
	i = CurrGetRadioSelection((const char**)options, 6, yStart,xStart);
	if (i) /* the other one */
	{
	    *editValue = setVs[i];
	    currAttr->cellRepr = AppDefaultCellRepr(setVs[i]);
	    return TRUE;
	}
	return FALSE;
}

/* change an attribute
 * returns TRUE if an item correctly changed
 * FALSE in no change was made or an incorrect change
 * (which is canceled) was made.
 */
int EditItem(
	double *editValue, /* read-write */
	char *editString,
	ATTR_NRS i,  /* item */
	int yStart,
	int xStart)
{
   switch(i) {
    case ATTR_nrRows:
    case ATTR_nrCols:
	 if ( CurrGetStringCheck(editString,strlen(NOT_SET)+1, yStart, xStart, CheckRowCols))
	 {
	 	INT4 v;
	 	(void)CnvrtINT4(&v,editString);
	 	*editValue = v;
	 	return TRUE;
	 }
	 return FALSE;
    case ATTR_valueScale:
	return EditValueScale(editValue, yStart,xStart);
    case ATTR_cellRepr:
	return EditCellRepr(editValue, yStart,xStart);
    case ATTR_projection:
	return EditProjection(editValue, yStart,xStart);
    case ATTR_xUL:
    case ATTR_yUL:
	 if ( CurrGetStringCheck(editString,strlen(NOT_APPL)+1, yStart, xStart, CheckUL))
	 {
	 	(void)CnvrtREAL8(editValue,editString);
	 	return TRUE;
	 }
	 return FALSE;
    case ATTR_cellSize:
	 if ( CurrGetStringCheck(editString,strlen(NOT_APPL)+1, yStart, xStart, CheckCellSize))
	 {
	 	(void)CnvrtREAL8(editValue,editString);
	 	return TRUE;
	 }
	 return FALSE;
    case ATTR_angle:
	 if ( CurrGetStringCheck(editString,strlen(NOT_APPL)+1, yStart, xStart, CheckAngle))
	 {
	 	(void)CnvrtREAL8(editValue,editString);
	 	return TRUE;
	 }
	 return FALSE;
     case ATTR_gisFileId:
	 if ( CurrGetStringCheck(editString,strlen(NOT_SET)+1, yStart, xStart, CheckFileId))
	 {
	 	INT4 v;
	 	(void)CnvrtINT4(&v,editString);
	 	*editValue = v;
	 	return TRUE;
	 }
	 return FALSE;
    case ATTR_minVal:
	 if ( CurrGetStringCheck(editString,strlen(NOT_APPL)+1, yStart, xStart, CheckMinVal))
	 {
	 	(void)CnvrtREAL8(editValue,editString);
	 	return TRUE;
	 }
	 return FALSE;
    case ATTR_maxVal:
	 if ( CurrGetStringCheck(editString,strlen(NOT_APPL)+1, yStart, xStart, CheckMaxVal))
	 {
	 	(void)CnvrtREAL8(editValue,editString);
	 	return TRUE;
	 }
	 return FALSE;
    default: PRECOND(FALSE);
    }
    PRECOND(FALSE);
    return FALSE;
}
