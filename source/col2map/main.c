#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "csf.h"
#include "app.h"	/* GetOpt, InstallArgs, ArgArguments */
#include <ctype.h>

/* apps. called */
#include "col2map.h"

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

#define USAGE \
 "USAGE: \n col2map ColumnInputFile MapOutputFile\n"\
 "BLNOSDV   data type for map file\n"\
 "m s       MV value in input file (default 1e31)\n"\
 "s c       separator character (default , )\n"\
 "x #       column number of x-coordinate in input file (default 1)\n"\
 "y #       column number of y-coordinate in input file (default 2)\n"\
 "v #       column number of value in input file (default 3)\n"\
 "a         average, scalar/directional (default)\n"\
 "t         total,   scalar\n"\
 "H         maximum, scalar/ordinal\n"\
 "M         minimum, scalar/ordinal\n"\
 "h         majority, nom/ord/ldd/bool (default)\n"\
 "l         minority, nom/ord/ldd/bool\n"\
 "--clone f clone file\n"

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */ 
/******************/

static void CompCellError(
	COMP_CELL compCell, 
	CSF_VS      vs)
{
 const char *flag = "aaHhMl";
 Error("-%c flag illegal for %s output", flag[compCell], RstrValueScale(vs));
}

int main(int argc, char *argv[])
{
	MAP 	*out, *clone = NULL;
	int 	c;
	size_t colNr[3] = {0/*X*/,1/*Y*/,2/*V*/}; /* internal indices */
	const char *mv = "1e31";
	char *cloneFileName;
	COMP_CELL compCell = NOTHING;
	CSF_CR cellRepr;
	CSF_VS valueScale = VS_UNDEFINED;
	int  parseVal;
	int  sepChar = ',';
	
	if(InstallArgs(argc, argv, "x#y#v#m*(BLNOSDV)(atHM)(hl)s*", "col2map", __DATE__))
		goto failure;
	while((c = GetOpt()) != 0)
	 switch(c){
		case 'x':
			parseVal = (*((const int *) OptArg))-1;
			if(parseVal < 0)
			{
			  Error("-x: x column should be greater than 0");
			  goto failure;
			}
			colNr[POS_X] = (size_t)parseVal;
			break;
		case 'y':
			parseVal = (*((const int *) OptArg))-1;
			if(parseVal < 0)
			{
			 Error("-y: y column should be greater than 0");
			 goto failure;
			}
			colNr[POS_Y] = (size_t)parseVal;
			break;
		case 'v':
			parseVal = (*((const int *) OptArg))-1;
			if(parseVal < 0)
			{
			 Error("-v: value column should be greater than 0");
			 goto failure;
			}
			colNr[POS_V] = (size_t)parseVal;
			break;
		case 'm':
			mv = (const char *)OptArg;
			break;
		case 'a':
			compCell = AVERAGE;
			break;
		case 'H':
			compCell = HIGHEST;
			break;
		case 'h':
			compCell = MAJORITY;
			break;
		case 'M':
			compCell = LOWEST;
			break;
		case 'l':
			compCell = MINORITY;
			break;
		case 't':
			compCell = TOTAL;
			break;
		case 's':
			sepChar = ((const char *)OptArg)[0];
			if (isdigit(sepChar))
			{
			 Error("-s: separator must be a non-digit (not [0-9])");
			 goto failure;
			}
			break;
#		include "case_vs.h"
	}

	argv = ArgArguments(&argc);
	if(argv == NULL)
		goto failure;
	if (AppArgCountCheck(argc, 3, 3, USAGE))
	        goto failure;
	if (AppInputTest(argv[1]))
		goto failure;

	/* open the clone map */
	clone = AppOpenClone(&cloneFileName, NULL);
	if (clone == NULL)
		goto failure;

	if(valueScale == VS_UNDEFINED)
	{
		valueScale = RgetValueScale(clone);
		cellRepr   = RgetCellRepr(clone);
	}
	else
		cellRepr = AppDefaultCellRepr(valueScale);

	if(! RvalueScale2(valueScale))
	{
		Error("clone map '%s' has an illegal value scale", cloneFileName);
		goto failure2;
	}

	if(compCell == NOTHING)
	{ /* defaults */
		switch(valueScale) {
		 case VS_SCALAR:  compCell   = AVERAGE; break;
		 case VS_DIRECTION: compCell = DIR_AVERAGE; break;
		 default:           compCell = MAJORITY; break;
		}
	}
	else 
	{
		BOOL conflict = FALSE;
		CSF_VS vs = valueScale;
		switch(compCell) {
		 case AVERAGE : conflict = (vs != VS_SCALAR && vs != VS_DIRECTION);
		                if (vs == VS_DIRECTION)
		                   compCell = DIR_AVERAGE;
			        break;
		 case LOWEST  :
		 case HIGHEST : conflict = (vs != VS_SCALAR && vs != VS_ORDINAL);
			        break;
		 case MAJORITY:
		 case MINORITY:
		                conflict = (vs == VS_SCALAR || vs == VS_DIRECTION);
		                break;
		 case TOTAL:
		                conflict = vs != VS_SCALAR;
		                break;
		 default:       POSTCOND(FALSE);
		}
	       if (conflict)
	       {
	          CompCellError(compCell, vs);
	          goto failure2;
	        }
	}

	out = Rdup(argv[2], clone, cellRepr, valueScale);
	if(out == NULL)
	{
		Error("output map '%s' can not be created", argv[2]);
		goto failure2;
	}
	Mclose(clone);
	clone = NULL;
	RuseAs(out, CR_REAL8);

	if(Col2Map(out, argv[1], compCell, mv, colNr, sepChar))
	{
		Mclose(out);
		(void)remove(argv[2]);
		goto failure;
	}

	Mclose(out);
	AppEnd();
	exit(0);
	return 0;

failure2: 
	if (clone == NULL)
	  Mclose(clone);
failure:  AppEnd();
	  exit(1);
        return 1;
} /* main */
