#include "stddefx.h" 

/*
 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "csf.h"
#include "app.h"	/* GetOpt, InstallArgs, ArgArguments */
#include <ctype.h>

/* apps. called */
#include "asc2map.h"	

/*************/
/* EXTERNALS */
/*************/


/**********************/
/* LOCAL DECLARATIONS */
/**********************/

#define USAGE \
 "USAGE:   asc2map AsciiInputFile MapOutputFile\n"\
 " BLNOSDV  data type for map file.\n"\
 " s c      separator character (default none or ',')\n"\
 " m s      MV in AsciiInputFile (default 1E31)\n"\
 " a        AsciiInputFile is Arc/Info gridascii output\n"\
 " g        AsciiInputFile is genamap (report) format\n"\
 " r #      nr. of lines to skip before each row\n"\
 " h #      nr. of lines to skip for header\n"\
 "--clone f clone file\n"

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */ 
/******************/

int main(int argc,		/* number of arguments */
	char *argv[])		/* list of arguments */
{
	MAP 	*clone = NULL, *output;
	int 	c;
	char    *cloneFileName;
	CSF_CR   cellRepr; 
	CSF_VS   valueScale;

	int 	rowHeader=0, header=0;
	const char 	*mv = "1e31";
	ASC_TYPE a = ASC_PLAIN;
	int    sepChar=',';

	valueScale = VS_UNDEFINED;
	
	if(InstallArgs(argc, argv, "(ag)r#h#m*(BLNOSDV)s*", "asc2map", __DATE__))
		goto failure;

	while((c = GetOpt()) != 0)
	{
		switch(c){
			case 'a':
				a = ASC_ARCINFO;
				break;
			case 'g':
				a = ASC_GENAMAP;
				break;
			case 'r':
			 	rowHeader = *((const int *) OptArg);
			 	if (rowHeader <= 0)
			 	{
			 	 Error("-r: argument must be greater than 0 (not %d)",rowHeader);
			 	 goto failure;
			 	}
				break;
			case 'h':
			 	header = *((const int *) OptArg);
			 	if (header <= 0)
			 	{
			 	 Error("-h: argument must be greater than 0 (not %d)",header);
			 	 goto failure;
			 	}
				break;
			case 'm':
			 	mv = OptArg;
			        break;

		        case 's':
				sepChar = ((const char *)OptArg)[0];
				if (isdigit(sepChar))
				{
				  Error("-s: separator must be a non-digit (not [0-9])");
				  goto failure;
				}
			break;
#			include "case_vs.h"
		}
	}

	if ( (argv = ArgArguments(&argc)) == NULL)
	       goto failure;

	if (AppArgCountCheck(argc, 3,3, USAGE))
	       goto failure;

	/* Check input file  */
	if (AppInputTest(argv[1]))
	         goto failure;

	clone = AppOpenClone(&cloneFileName, NULL);
	if (clone == NULL)
	         goto failure;

        if( LimitedVersionCheck((int)RgetNrRows(clone),(int)RgetNrCols(clone),
              -1,-1,-1,-1)) goto failure2;

	if(valueScale == VS_UNDEFINED)
	{
		valueScale = RgetValueScale(clone);
		if (!RvalueScale2(valueScale))
		{
			Error("clone map '%s' has an illegal value scale",
			      cloneFileName);
			goto failure2;
		}
		cellRepr   = RgetCellRepr(clone);
	}
	else
		cellRepr   = AppDefaultCellRepr(valueScale);
	output = Rdup(argv[2], clone, cellRepr, valueScale);
	if(output == NULL)
	{
		Mperror(argv[2]);
		goto failure2;
	}
	Mclose(clone);
	clone = NULL;
	if (RuseAs(output, CR_REAL8))
	{
		Mperror(argv[2]);
		goto failure3;
	}
	if (a != ASC_PLAIN)
	{
		sepChar = ',';
	        header = 0;
	        rowHeader = 0;
	}

	if( Asc2Map(output, argv[1], mv,sepChar,a,header,rowHeader) != 0)
		goto failure3;

	Mclose(output);

	AppEnd();
	exit(0);
	return 0;

failure3:
	Mclose(output);
	(void)remove(argv[2]);
failure2:
	if (clone != NULL)
		Mclose(clone);
failure:
	AppEnd();
	exit(1);
	return 1;
} /* main */
