#include "stddefx.h" 

/*
 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <string.h>    /* strchr */
#include "misc.h"
#include "csf.h"
#include "app.h"	/* GetOpt, InstallArgs, ArgArguments */

/* apps. called */
#include "map2asc.h"

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

#define USAGE \
 "USAGE: map2asc MapInputFile AsciiOutputFile\n"\
 " a   output in Arc/Info asciigrid format\n"\
 " R   create 1 line header with nr. of rows,cols\n"\
 " C   create 1 line header with nr. of cols,rows\n"\
 " m s mv for output (default 1e31)\n"\
 " n # nr. of cells to put on a single line (default 1 row on 1 line)\n"\
 " f s C-language format for output (default best fit)\n"\
 " s s separator for output\n"\
 " r   row wise output (default)\n"\
 " c   column wise output\n" 

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */ 
/******************/

static int CheckFmt(const char *fmt)
{ /* copied in map2col/main.c */
	if (strchr("feEgG", fmt[strlen(fmt)-1]) == NULL /* no fmt end */
	    || atoi(fmt) <= 0 ) /* should start with a non-neg number */
	{
	 Error("-f: format '%s' is not C floating point format",fmt);
	 return 1;
	}
	return 0;
}

/*!
 * \todo
 *   make this generice for dt2d, pcrcalc and command line
 */
int main(int argc,		/* number of arguments */
	char *argv[])		/* list of arguments */
{
	MAP 	*inputMap;
	int 	c;
	char    format[128];
	const char *separator=NULL;
	HEADER  head=HEAD_NONE;
  int colWise=FALSE;
	int     nrCellsOnLine = 0;
	const char    *mv = "1e31";

	strcpy(format,""); /* empty string if not given */

	if(InstallArgs(argc,argv,"n#m*s*(aRC)(rc)f*","map2asc",__DATE__))
		goto failure;

	while((c = GetOpt()) != 0)
	 switch(c)
	       {
		case 'a':
			head = HEAD_ARCINFO;
			break;
		case 'R':
			head = HEAD_ROWCOL;
			break;
		case 'C':
			head = HEAD_COLROW;
			break;
		case 'n':
			nrCellsOnLine = *((const int *) OptArg);
			if (nrCellsOnLine <= 0)
			{
			 Error("-n: nr. of cells must be greater than 0 (not %d)",
				nrCellsOnLine);
			 goto failure;
			}
			break;
		case 'm':
			mv = OptArg;
			break;
		case 's':
			separator = OptArg;
			break;
		case 'r':
			colWise = FALSE;
			break;
		case 'c':
			colWise = TRUE;
			break;
		case 'f':
                        if( CheckFmt( LeftRightTrim(strcpy(format,OptArg))) )
                        	goto failure;
			break;
	}

	argv = ArgArguments(&argc);
	if(argv == NULL)
		goto failure;

	if(AppArgCountCheck(argc,3,3,USAGE))
		goto failure;
	if (AppInputTest(argv[1]))
		goto failure;

	/* Determine the valueScale out of input map */
	inputMap = Mopen(argv[1], M_READ);
	if(inputMap == NULL)
	{
		Error("'%s' is not a (CSF) map file", argv[1]);
		goto failure;
	}

	if (LimitedVersionCheck( (int)RgetNrRows(inputMap),
	                         (int)RgetNrCols(inputMap),-1,-1,-1,-1))
		goto failure;

	RuseAs(inputMap, CR_REAL8);
	if(nrCellsOnLine == 0) /* not specified on command line */
		nrCellsOnLine = (int) 
		 (colWise ? RgetNrRows(inputMap) : RgetNrCols(inputMap));

	if(Map2Asc(inputMap, argv[2],
	           mv, format, separator, nrCellsOnLine, head, colWise))
		goto failure2;
 
	Mclose(inputMap);
	AppEnd();
	exit(0);
	return 0;
failure2:
       Mclose(inputMap);
failure:
       AppEnd();
       exit(1);
       return 1;
} /* main */

