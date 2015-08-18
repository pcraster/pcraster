#include "stddefx.h" 

/* TODO
 *
 * resample --clone clone.map bla.map bla.map
 * levert geen foutmelding maar wel onzin data op
 * tweede naam moet ongelijk eerste zijn blijkbaar
 *
 * oplossing gebruik misc/filesets.c 
 */


/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include "mathx.h" /* M_2PI */
#include "csf.h"
#include "misc.h"
#include "app.h"
#include "table.h"
#include <ctype.h>

/* apps. called */
#include "sample.h"	/* Sample */


/*************/
/* EXTERNALS */
/*************/
int opPer=0;		/* option for percentage */
int opMax=0;		/* option for maximum value */
BOOL opR=FALSE;		/* option for resample factor */
BOOL opMV=FALSE;	/* option for non-MV border */
BOOL opB=FALSE;		/* option for border */
int optionAcc;		/* option for accuracy */
size_t rasterSize;		/* rasterSize for cover raster */
int nrOpFields;		/* number of option fields */

#ifdef DEBUG
 extern int nrSearchTables;
#endif


/**********************/
/* LOCAL DECLARATIONS */
/**********************/

#define NO_CLONE_NEEDED	(opR || opMV || opB)


#define USAGE \
     "USAGE resample InputMap(s) OutputMap\n"\
     " e $ error percentage\n"\
     " b # border\n"\
     " c # non-MV border\n"\
     " m   maximal value calculated\n"\
     " p $ percentage (default 0)\n"\
     " r $ resample factor\n"\
     " B   same as -b 0\n"\
     " C   same as -c 0\n"\
     " R   same as -r 1\n"\
     " a   contract map when rounding to cells\n"\
     " x   expand map when rounding to cells (default)\n"\
     " k   keep minumum and maximum of input \n"\
     " --clone clone.map resample to this size\n"

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/
#define INITSIZE 20
#define MAXSIZE 200 
#define EPSILON 0.00000001

/******************/
/* IMPLEMENTATION */ 
/******************/
/* Determines the boundaries and calculates X0, Y0, nrRows and nrCols.
 * Returns nothing but changes the X0, Y0, nrRows and nrCols.
 */
 static void CalcBound(
	REAL8 *X0,		/* write-only X0 */
	REAL8 *Y0,		/* write-only Y0 */
	size_t *nrRows,		/* write-only nr of rows */
	size_t *nrCols,		/* write-only nr of columns */
 	POINT2D *leftU,		/* read-write maximum x-coordinate */
 	POINT2D *rightU,		/* read-write maximum y-coordinate */
 	POINT2D *leftL,		/* read-write minimum x-coordinate */
 	POINT2D *rightL,		/* read-write minimum y-coordinate */
	int borderValue,	/* bordervalue */
 	double cellSize,	/* cellSize */
 	double angle,		/* angle of output map */
	CSF_PT projection,	/* projection of output map */
	BOOL contract)		/* to contract the map */
 {
	double nrR, nrC;

	/* Adjust boundaries according to border value */
	leftU->x -= borderValue;
	rightU->x += borderValue;
	leftL->x -= borderValue;
	rightL->x += borderValue;
	if(projection == PT_YINCT2B)
	{
		leftU->y -= borderValue;
		rightU->y -= borderValue;
		leftL->y += borderValue;
		rightL->y += borderValue;
	}
	else
	{
		leftU->y += borderValue;
		rightU->y += borderValue;
		leftL->y -= borderValue;
		rightL->y -= borderValue;
	}

	/* Calculate the number of rows and columns */
	nrR = (rightL->y - rightU->y) / cellSize;
	nrC = (rightU->x - leftU->x) / cellSize;

	if(nrR < 0)
		nrR = -nrR;
	if(nrC < 0)
		nrC = -nrC;

	if(contract)
	{
		*nrRows = floor((REAL4) nrR);
		*nrCols = floor((REAL4) nrC);
	}
	else
	{
		*nrRows = ceil((REAL4) nrR);
		*nrCols = ceil((REAL4) nrC);
	}

	/* Rotate points back to original position */
	if(angle != 0)
	{
		(void)RotPoint(leftU, M_2PI - angle);
		(void)RotPoint(rightU, M_2PI - angle);
		(void)RotPoint(leftL, M_2PI - angle);
		(void)RotPoint(rightL, M_2PI - angle);
	}

	*X0 = leftU->x;
	*Y0 = leftU->y;

	return;
 }

/* Checks whether all input maps have the same attributes as given.
 * Returns 1 in case of a difference, 0 otherwise.
 */
static int CheckInputMaps(
	MAP **in,		/* input maps to check */
	size_t nrMaps,		/* number of input maps */
	CSF_PT projection,	/* projection to satisfy */
	REAL8 angle,		/* angle to satisfy */
	REAL8 cellSize)		/* cell size to satisfy */
{
	size_t i;

	/* Check all input maps */
	for(i = 0; i < nrMaps; i++)
	{
		MAP *X = in[i];
		if(angle != RgetAngle(X))
		{
			ErrorNested(
			"all input map should have the same angle.");
			return 1;
		}

		if(projection != MgetProjection(X))
		{
			ErrorNested(
			"all input map should have the same projection.");
			return 1;
		}

		if(cellSize != RgetCellSize(X))
		{
			ErrorNested(
			"all input map should have the same cell size.");
			return 1;
		}
	}
	return 0;		/* all maps have same attributes */
}

/* Determines the smallest fitting rectangle around input maps. 
 * The bordervalue is added.
 * Returns x0, y0, nrRows and nrCols for out.
 */
static int SmallestFittingRectangle(
	REAL8 *X0out,		/* write-only X0 */
	REAL8 *Y0out,		/* write-only Y0 */
	size_t *nrRows,		/* write-only nr of rows */
	size_t *nrCols,		/* write-only nr of columns */
	MAP **in,  		/* read-only pointer to input maps */
	int borderValue,	/* bordervalue */
	size_t nrMaps,		/* number of input maps */
	REAL8 cellSize,		/* cell size of output map */
	REAL8 angle,		/* angle of output map */
	CSF_PT projection,	/* projection of output map */
	BOOL contract)		/* map should be contracted */
{
	size_t	i;
	REAL8	upperB=0, leftB=0, rightB=0, belowB=0;
	   /* ^- shut up about use before def */
	POINT2D	leftUpperC, rightUpperC, leftLowerC, rightLowerC;

	/* determine the boundaries for every map */
	for(i = 0; i < nrMaps; i++)
	{
		MAP	*X = in[i];
		int	c;
		POINT2D	polygon[4];		/* rectangle */
		REAL8	X0 = RgetX0(X);
		REAL8	Y0 = RgetY0(X);
		REAL8	nrR = (REAL8) RgetNrRows(X);
		REAL8	nrC = (REAL8) RgetNrCols(X);

		/* transform corners of map into x- and y-coordinates */
		polygon[0].x = X0;
		polygon[0].y = Y0;
		RrowCol2Coords(X, 0.0, nrC - EPSILON, &polygon[1].x,
				&polygon[1].y);
		RrowCol2Coords(X, nrR - EPSILON, nrC - EPSILON,
				&polygon[2].x, &polygon[2].y);
		RrowCol2Coords(X, nrR - EPSILON, 0.0, &polygon[3].x,
			&polygon[3].y);

		/* Rotate all corners of map */	
		if(angle != 0)
			for(c = 0; c < 4; c++)
			    RotPoint(polygon + c, angle);

		/* Determine boundaries of rotated output map */
		for(c = 0; c < 4; c++)
		{
			if((i==0&&c==0)||polygon[c].y > belowB)
				belowB = polygon[c].y;
			if((i==0&&c==0)||polygon[c].y < upperB)
				upperB = polygon[c].y;
			if((i==0&&c==0)||polygon[c].x > rightB)
				rightB = polygon[c].x;
			if((i==0&&c==0)||polygon[c].x < leftB)
				leftB = polygon[c].x;
		}
	}

	/* Put boundaries in corners of the rotated map */
	leftUpperC.x = leftB;
	rightUpperC.x = rightB;
	leftLowerC.x = leftB;
	rightLowerC.x = rightB;

	if(projection == PT_YINCT2B)
	{
		leftUpperC.y = upperB;
		rightUpperC.y = upperB;
		leftLowerC.y = belowB;
		rightLowerC.y = belowB;
	}
	else
	{
		leftUpperC.y = belowB;
		rightUpperC.y = belowB;
		leftLowerC.y = upperB;
		rightLowerC.y = upperB;
	}

	/* calculate the boundary of the output map */
	CalcBound(X0out, Y0out, nrRows, nrCols, &leftUpperC, &rightUpperC,
	 	&leftLowerC, &rightLowerC, borderValue, cellSize, angle,
	 	projection, contract);
	return 0;
}

/* Determines smallest rectangle around nonMv values.
 * The border value can be added around this rectangle.
 * Returns x0, y0 , nrRows and nrCols for out.
 */
static int SmallestNonMVRect(
	REAL8 *X0out,		/* write-only X0 */
	REAL8 *Y0out,		/* write-only Y0 */
	size_t *nrRows,		/* write-only nr of rows */
	size_t *nrCols,		/* write-only nr of columns */
	MAP **in,  		/* read-only pointer in maps */
	int borderValue,	/* border value */
	size_t nrMaps,		/* number of input maps */
	CSF_VS valueScale,	/* value scale of output map */
	REAL8 cellSize,		/* cellSize of map */
	REAL8 angle,		/* angle of output map */
	CSF_PT projection,	/* projection of output map */
	BOOL contract)		/* map should be contracted */
{
	size_t 	i;
	POINT2D 	leftUpperC, leftLowerC, rightUpperC, rightLowerC;
	REAL8 	upperB=0, leftB=0, rightB=0, belowB=0;
 	   /* ^- shut up about use before def */

	for(i = 0; i < nrMaps; i++)
	{
		MAP	*X = in[i];
		BOOL	first = TRUE;
		POINT2D	polygon[4];
		size_t 	r, c;
		size_t 	nrR = RgetNrRows(X);
		size_t 	nrC = RgetNrCols(X);

		for(r = 0; r < nrR; r++)
		 for(c = 0; c < nrC; c++)
		 {
	 		INT4 int4Val;
	 		REAL8 real8Val;

	 		if((AppIsClassified(valueScale) &&
	 		  RgetCell(in[i], r, c, &int4Val) &&
	 		  int4Val != MV_INT4) ||
	 		  (!AppIsClassified(valueScale) && 
	 		  RgetCell(in[i], r, c, &real8Val) &&
	 		  (IsMV(in[i], &real8Val) == FALSE)))
	 		{
				if(first || c < leftB)
					leftB = c;
				if(first || c > rightB)
					rightB = c;
				if(first || r > belowB)
					belowB = r;
				if(first || r < upperB)
					upperB = r;
				first = FALSE;
			}
		}

		/* Get coordinates of corners */
		RrowCol2Coords(X, upperB, leftB, &polygon[0].x,
			&polygon[0].y);
		RrowCol2Coords(X, upperB, rightB + 1 - EPSILON,
			&polygon[1].x, &polygon[1].y);
		RrowCol2Coords(X, belowB + 1 - EPSILON, leftB,
			&polygon[2].x, &polygon[2].y);
		RrowCol2Coords(X, belowB + 1 - EPSILON,
		    rightB + 1 - EPSILON, &polygon[3].x, &polygon[3].y);

		/* Rotate all corners of map */	
		if(angle != 0)
		{
			for(c = 0; c < 4; c++)
		   	    polygon[c] = *RotPoint(polygon + c, angle);
		}

		/* Determine boundaries of rotated output map */
		for(c = 0; c < 4; c++)
		{
			if(polygon[c].y > belowB || (i == 0 && c == 0))
				belowB = polygon[c].y;
			if(polygon[c].y < upperB || (i == 0 && c == 0))
				upperB = polygon[c].y;
			if(polygon[c].x > rightB || (i == 0 && c == 0))
				rightB = polygon[c].x;
			if(polygon[c].x < leftB || (i == 0 && c == 0))
				leftB = polygon[c].x;
		}
	}

	leftUpperC.x = leftB;
	rightUpperC.x = rightB;
	leftLowerC.x = leftB;
	rightLowerC.x = rightB;

	if(projection == PT_YINCT2B)
	{
		leftUpperC.y = upperB;
		rightUpperC.y = upperB;
		leftLowerC.y = belowB;
		rightLowerC.y = belowB;
	}
	else
	{
		leftUpperC.y = belowB;
		rightUpperC.y = belowB;
		leftLowerC.y = upperB;
		rightLowerC.y = upperB;
	}

	CalcBound(X0out, Y0out, nrRows, nrCols, &leftUpperC, &rightUpperC,
	 	&leftLowerC, &rightLowerC, borderValue, cellSize, angle,
	 	projection, contract);
	return 0;
}

/* Deallocates all input maps
 * Return nothing.
 */
static void FreeMaps(
		MAP **in,	/* write-only pointer to input maps */
		size_t nrMaps)	/* number of input maps */
{
	size_t i;
	for(i = 0; i < nrMaps; i++)	/* Close all maps */
	{
		MAP *tmp = in[i];
		if(Mclose(tmp))
			MperrorExit(MgetFileName(tmp), 1);
	}	
	Free(in);			/* Deallocate memory */
}

/* Function to determine the raster size.
 * If all input maps have the same angle as the output map and
 * all distances and cell sizes are a multiple of the smallest
 * cell size than the rastersize may be less than the INITSIZE.
 * Returns 1 in case of error, 0 otherwise.
 */
 static int DetRasterSize(
 	const MAP *out,		/* write-only output map */
 	MAP **in,		/* read-only input maps */
 	size_t nrMaps,		/* number of input maps */
 	double errFactor)	/* maximum error */
 {
 	REAL8 minCellSize = REAL8_MAX;
	size_t i;
	CSF_PT projIn, projOut;
	REAL8 cellSize, n, X0, Y0, Xout, Yout, angleIn, angleOut;
	rasterSize = INITSIZE;

 	/* If option -a set -> rastersize depends on wanted accuracy */
	if(optionAcc)
	{
		if(errFactor != 0)
			rasterSize = ceil ((double) 50 / errFactor);
		else
			rasterSize = MAXSIZE;
	}

 	/* Determine the output angle */
	angleOut = RgetAngle(out);
	projOut = MgetProjection(out);
		
 	/* Determine the minimum cell size */
 	for(i = 0; i < nrMaps; i++)
 	{
 		MAP *X = in[i];
 		angleIn = RgetAngle(X);
 		projIn = MgetProjection(X);
 		if(angleIn != angleOut || projIn != projOut)
 			return 0;		/* different angles */
 		cellSize = RgetCellSize(X);
 		if(cellSize <= 0)		/* illegal cell size */
 			return 1;
 		if(cellSize < minCellSize)
 			minCellSize = cellSize;	/* minimum cell size */
 	}
 	cellSize = RgetCellSize(out);
 	if(cellSize <= 0)
 		return 1;			/* illegal cell size */
 	if(cellSize < minCellSize)
 		minCellSize = cellSize;		/* minimum cell size */
 	
 	/* Determine whether all cell size are N * min(cellsize) */
 	for(i = 0; i < nrMaps; i++)
 	{
 		MAP *X = in[i];
 		cellSize = RgetCellSize(X);
 		n = (REAL8) cellSize / minCellSize;
 		if((REAL8)(int) n < n - EPSILON ||
		  n + EPSILON < (REAL8)(int) n)
			return 0;	/* RASTERSIZE NOT MODIFIED */
	}
	cellSize = RgetCellSize(out);
 	n = (REAL8) cellSize / minCellSize;

 	if((REAL8)(int) n < n - EPSILON ||
	   n + EPSILON < (REAL8)(int) n)
 		return 0;

 	/* Determine whether the distances are N * min(cell size) */
 	Xout = RgetX0(out);
 	Yout = RgetY0(out);
 	for(i = 0; i < nrMaps; i++)
 	{
		X0 = RgetX0(in[i]);
		Y0 = RgetY0(in[i]);
		n = (X0 - Xout) / minCellSize;
 		if((REAL8)(int) n < n - EPSILON ||
		   n + EPSILON < (REAL8)(int) n)
			return 0;
		n = (Y0 - Yout) / minCellSize;
 		if((REAL8)(int) n < n - EPSILON ||
		   n + EPSILON < (REAL8)(int) n)
			return 0;
	}
	cellSize = RgetCellSize(out);
	rasterSize = (int) (cellSize / minCellSize);
 	return 0;
 }

/* Deallocates the array of input maps.
 * Closes the input and output maps.
 */
static void EndResample(
	MAP **in,		/* input maps to free */
	size_t nrMaps,		/* number of input maps */
	MAP *out)		/* output map to free */
{
	/* Deallocate and close the input maps */
	FreeMaps(in, nrMaps);

	if(Mclose(out))			/* Close the output map */
		MperrorExit(MgetFileName(out), 1);

	AppEnd();
}

/* Function for resampling  N input maps into 1 output map.
 * Assumes a map "clone.map" and N "input.map"s present. Checks on
 * options for percentage and maximum value. 
 * Determines type and characteristics of output map.
 * Returns nothing, exits with 1 in case of error.
 */
int main(int argc,		/* number of arguments */
	char *argv[])		/* list of arguments */
{
     	MAP 	*clone, *out, *tmp, **in;
     	char 	*outputName, *cloneName;	
     	int 	c, borderval;	
     	size_t  nrMaps,i;
	REAL8 	X0, Y0, cellSize, angleIn, angleOut;
	size_t 	nrRows, nrCols;
	CSF_PT 	projection;
	CSF_CR  cellRepr;
	CSF_VS  valueScale;
	double 	percent = 0, errFactor = 2.5, resampleN = 0.0;
	BOOL	aligned = TRUE;
	BOOL    keepInputMinMax = FALSE;
	REAL8	minAllInput=0, maxAllInput=0;
	BOOL	onlyReal4 = TRUE, contract = FALSE;
	BOOL	onlyUint1 = TRUE;

	if(InstallArgs(argc, argv,"axmp$r$c#b#e$RBCk", "resample", __DATE__))
		exit(1);

     	while((c = GetOpt()) != 0)
     	{
     	    switch(c)
     	    {
     	    	case 'b': opB = TRUE;
     	    		borderval = *((int *) OptArg);
     	    		break;
     	    	case 'B': opB = TRUE;
     	    		borderval = 0;
     	    		break;
     	    	case 'C': opMV = TRUE;
     	    		borderval = 0;
     	    		break;
     	    	case 'c': opMV = TRUE;
     	    		borderval = *((int *) OptArg);
     	    		break;
     	    	case 'a':contract = TRUE;
     	    		break;
     	    	case 'x':contract = FALSE;
     	    		break;
     	    	case 'm':opMax = 1;
     	    		break;
     	    	case 'p':opPer = 1;
     	    		percent = *((double*) OptArg);
     	    		if(percent < 0 || 100 < percent)
     	    		{
     	    			Error("illegal percentage");
     	    			exit(1);
     	    		}
     	    		break;
     	    	case 'R':opR = 1;
     	    		resampleN = 1;
     	    		break;
     	    	case 'r':opR = 1;
     	    		resampleN = *((double*) OptArg);
     	    		break;
     	    	case 'e':optionAcc = 1;
     	    		errFactor = *((double*) OptArg);
     	    		break;
     	    	case 'k': keepInputMinMax = TRUE;
     	    		break;
     	    }
     	}

	argv = ArgArguments(&argc);
	if (AppArgCountCheck(argc,3,-1,USAGE))
		exit(1);

	outputName = argv[argc-1];
  	nrMaps  = argc-2;

	/* Read the desired specifics out of the clone map 
	 * or use first input as clone map
	 */
	cloneName = NO_CLONE_NEEDED ? argv[1] : NULL;
	if ( (clone = AppOpenClone(&cloneName,cloneName)) == NULL)
		exit(1);

	/* Determine the valueScale out of 1st input map */
	tmp = Mopen(argv[1], M_READ);
	if(tmp == NULL)
		MperrorExit(argv[1], 1);

	/* all input maps have same value scale */
	valueScale = RgetValueScale(tmp);
	if(valueScale == VS_LDD && !opMV)
	{
		Error("can not do this type of resampling on map '%s' with type ldd", argv[1]);
		exit(1);
	}
	/* adjust old ones */
	if(valueScale == VS_CLASSIFIED)
		valueScale = VS_ORDINAL;
	if(valueScale == VS_CONTINUOUS)
		valueScale = VS_SCALAR;

	/* get location attributes of clone or of 1st input map */
	projection = MgetProjection(clone);
	nrRows = RgetNrRows(clone);
	nrCols = RgetNrCols(clone);
	X0 = RgetX0(clone);
	Y0 = RgetY0(clone);
	cellRepr = RgetCellRepr(clone);
	angleOut = RgetAngle(clone); 

	/* resample option -> cell size(inputmap) * factor 
	 * Number of rows and columns are divided by resample
	 * factor.
	 */
	if(opR == 1)
	{
		/* setting for unit */
		if(!appUnitTrue)
		{
			cellSize = resampleN;
			resampleN /= (double) RgetCellSize(tmp);
		}
		else
			cellSize = RgetCellSize(tmp) * resampleN;
		if(contract)
		{
			nrRows = floor((double) nrRows / 
					(double) resampleN);
			nrCols = floor((double) nrCols / 
					(double) resampleN);

			/* Prevent an illegal map */
			if(nrRows == 0)
				nrRows = 1;
			if(nrCols == 0)
				nrCols = 1;
		}
		else
		{
			nrRows = ceil((double) nrRows / 
					(double) resampleN);
			nrCols = ceil((double) nrCols / 
					(double) resampleN);
		}
	}
	else
		cellSize = RgetCellSize(clone);

	/* Allocate memory for the input map pointers */
	in = (MAP **)ChkMalloc(sizeof(MAP *) * nrMaps);
	if(in == NULL)
	{
		AppEnd();
		exit(1);
	}

	/* Read all input maps with desired cell representation */
	for(i = 0; i < nrMaps; i++)
	{
		REAL8	tmpMin, tmpMax;

		tmp = Mopen(argv[1 + i], M_READ);
		angleIn = RgetAngle(tmp);
		if(angleIn != 0)
			aligned = FALSE;
		if(tmp == NULL)
			MperrorExit(argv[1 + i], 1);

		if(!RvalueScaleIs(tmp, valueScale))
		{
			Error("%s has illegal data type: '%s'\n",
				argv[1 + i], RstrValueScale(valueScale));
			exit(1);
		}

		in[i] = tmp;

		/* Determine which cell representation should be used */
		onlyReal4 = RgetCellRepr(in[i]) == CR_REAL4;
		onlyUint1 = RgetCellRepr(in[i]) == CR_UINT1;


		RuseAs(in[i], CR_REAL8);
		RgetMinVal(tmp, &tmpMin);
		RgetMaxVal(tmp, &tmpMax);
		if (i==0)
		 {minAllInput = tmpMin; maxAllInput = tmpMax; }
		minAllInput = MIN(minAllInput,tmpMin);
		maxAllInput = MAX(maxAllInput,tmpMax);

		if(AppIsClassified(valueScale))
			RuseAs(in[i], CR_INT4);
		else
			RuseAs(in[i], CR_REAL8);
	}

	if(opB == 1 || opMV == 1)
	{
		if(CheckInputMaps(in, nrMaps, projection, angleIn, cellSize))
		{
			Error("");
			FreeMaps(in, nrMaps);
			exit(1);
		}

		if(opB == 1)
		{
			if(SmallestFittingRectangle(&X0, &Y0, &nrRows,
			  &nrCols, in, borderval, nrMaps,
			  cellSize, angleIn, projection, contract))
			{
				FreeMaps(in, nrMaps);
				AppEnd();
				exit(1);
			}
		}
		else
		{
		  	if(SmallestNonMVRect(&X0, &Y0, &nrRows, &nrCols, in,
		  	borderval, nrMaps, valueScale, cellSize,
		  	angleIn, projection, contract))
			{	
				FreeMaps(in, nrMaps);
				AppEnd();
				exit(1);
			}
		}
	}

	/* Create output map with suitable cell representation */ 
	/* NOTE ! Create map with smallest representation possible */
	out = Rcreate(outputName, nrRows, nrCols, 
	               AppIsClassified(valueScale) ?
		               (onlyUint1 ? CR_UINT1 : CR_INT4) :
		               (onlyReal4 ? CR_REAL4 : CR_REAL8),
		               valueScale, projection, X0, Y0, 
		               angleOut, cellSize);
	if(out == NULL)
	{
		FreeMaps(in, nrMaps);
		Error("can not create output map '%s': %s", 
		      argv[1], MstrError());
		exit(1);
	}
	RuseAs(out, AppIsClassified(valueScale) ? CR_INT4 : CR_REAL8);

	if(angleOut != 0)
		aligned = FALSE;


	/* determine raster size according wanted accuracy */
	if(opB != 1 && opMV != 1)
	{
		if(DetRasterSize(out, in, nrMaps, errFactor))
		{
			Error("Illegal cell size\n");
			exit(1);
		}
	}
	else
		rasterSize = 1;

	if(nrMaps > 1 && percent > 0)
		AppProgress("rasterSize: %d\n", rasterSize);
	else
		AppProgress("No raster used\n");

	/* Call function */
	if(AppIsClassified(valueScale))
	{	/* Call resample function for classified maps */

		if(SampleClass(out, in, percent, nrMaps, nrRows, nrCols,
		   aligned, angleOut))
		{	
			EndResample(in, nrMaps, out);
			exit(1);	/* Memory allocation failed */
		}	
	}		
	else
	{	/* Call resample function for continuous maps */
		if(SampleCont(out, in, percent, nrMaps, nrRows, nrCols,
		   aligned, angleOut))
		{	
			EndResample(in, nrMaps, out);
			exit(1);	/* Memory allocation failed */
		}	
	}		

	/* End of call */
	if (keepInputMinMax) {
		RuseAs(out, CR_REAL8);
		RputMinVal(out, &minAllInput);
		RputMaxVal(out, &maxAllInput);
	}
	EndResample(in, nrMaps, out);
	
	exit(0);  			/* Successful exit */
	return 0; 			/* Never reached */
} /* main */
