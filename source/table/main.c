#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "csf.h"
#include "misc.h"
#include "app.h"
#include "table.h"

/* apps. called */
#include "move.h"

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/
/* Prints message about usage of program.
 */
#define USAGE \
 "USAGE: table InputMaps OutputTable\n" \
 "0   include tuples with score 0\n" \
 "h   histogram stretched intervals\n" \
 "H # nr. of slots for histogram stretched intervals (implies -h)\n" \
 "n # nr. of intervals\n" \
 "i f input table or matrix\n" \
 "m # move column number (only with -i)\n" 

#define INIT_NR_INTERVALS 8
#define INIT_NR_SLOTS 1024

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */ 
/******************/
/* Deallocates all input maps
 */
static void FreeMaps(
		MAP **in,	
		size_t nrMaps)	/* number of input maps */
{
	size_t i;
	if (in == NULL)
		return;
	for(i = 0; i < nrMaps; i++)	/* Close all maps */
		if(in[i] != NULL)
		 Mclose(in[i]);
	Free(in);			/* Deallocate memory */
}

static MAP **ReadMaps(
	const char **mapArgs,
	size_t nrMaps)
{
	size_t i;
	/* calloc, NULL identifies maps not yet open
	 *  neccessary for FreeMaps
	 */
	MAP **in = (MAP **)ChkCalloc(nrMaps, sizeof(MAP *));
	if(in == NULL)
		return NULL;
	/* Read all input maps with desired cell representation */
	for(i = 0; i < nrMaps; i++)
	{
		in[i] = Mopen(mapArgs[i], M_READ);
		if(in[i] == NULL)
		{
		 Error("%s:%s", mapArgs[i],MstrError());
		 goto failure;
		}
		if (RuseAs(in[i], CR_REAL8))
		{
		 Mperror(mapArgs[i]);
		 goto failure;
		}
		if(!Rcompare(in[0], in[i]))
		{
		  PRECOND(i!=0);
		  Error("maps '%s' and '%s' do not have the same location attributes",
		         mapArgs[0], mapArgs[i]);
		  goto failure;
		}
		if(LimitedVersionCheck( (int)RgetNrRows(in[i]), (int)RgetNrCols(in[i]),
		    -1,-1,-1,-1))
		  goto failure;
	}	
	return in;
failure:
	FreeMaps(in,nrMaps);
	return NULL;
}

static int WriteCrossTable(
	const char *tableName,
	LOOK_UP_TABLE *t,
	BOOL  ommitZeros,
	double area)
{
	size_t i;
	if (appUnitTrue)
		area *= area;
	else
		area = 1;

	if(LimitedVersionCheck( -1, -1, -1,(int)t->nrRecords,-1,-1))
		return 1;

	 for(i=0; i < t->nrRecords; i++)
	 {
		if (ommitZeros && t->records[i][t->nrKeys].l == 0)
		 t->records[i][t->nrKeys].t = TEST_NOKEY;
		else
		 t->records[i][t->nrKeys].l *= area;
	}
	return WriteLookupTable(tableName,t);
}


/* Function for using a table and N input maps to produce output map.
 * Assumes a file "lookup table" and N input maps present. Checks
 * lookup table on being correct. Determines type and characteristics
 * of output map.
 * Returns nothing, exits with 1 in case of error.
 */
int main(int argc,		/* number of arguments */
	char *argv[])		/* list of arguments */
{
	MAP **in = NULL;
	int c;
	size_t nrMaps=0;	/* number of input maps */ 
	/* initialize the variables */
	const char *tableName = NULL;
     	const char *inputTableName = NULL;
     	BOOL histogram = FALSE;
     	BOOL ommitZeros =TRUE;
     	size_t nrIntervals = INIT_NR_INTERVALS;
     	size_t nrCountSlots = INIT_NR_SLOTS;
     	size_t moveColNr = 0;

     	if(InstallArgs(argc, argv, "0m#i*n#hH#", "table", __DATE__))
     		goto failure;

     	/* Check which options are used and set externals accordingly */
     	while((c = GetOpt()) != 0)
     	{
     	switch(c)
	{
     	 case '0':
     	  ommitZeros = FALSE;
     	  break;
	 case 'h':
	  histogram = TRUE;
	  break;
	 case 'H':
	  histogram = TRUE;
	  c = *((const int *) OptArg);
	  if (c <= 0 )
	  {
	   Error("-H: count slots must be greater than 0 (is %d)", c);
	   goto failure;
	   }
	   nrCountSlots = (size_t)c;
	   break;
	 case 'n':
	   c = *((const int *)OptArg);
	   if (c <= 0)
	   {
	    Error("-n: number of intervals must be greater than 0 (is %d)", c);
	   goto failure;
	   }
	   nrIntervals = (size_t)c;
	   break;
	 case 'i':
	   inputTableName = OptArg;
           if (AppInputTest(inputTableName))
           	goto failure;
	   break;
	 case 'm':
	   c = *((const int *) OptArg);
	   if (c <= 0)
	   {
	    Error("-m: column number to move must be greater than 0 (is %d)",
	 		 moveColNr);
	   goto failure;
	   }
	   moveColNr = c;
	   break;
	} /* switch */
     	} /* while  */

	if ( (argv = ArgArguments(&argc)) == NULL)
		goto failure;

	if (AppArgCountCheck(argc,2,-1,USAGE))
		goto failure;

	tableName = argv[argc-1];

	if(moveColNr > 0)
	{ /* move column */	
	 /* option -m is used -> -i has to be used also */
	 if (inputTableName == NULL)
	 {
	  Error("-m: no input table specified with -i");
	  goto failure;
	 }
	 if (MoveColumn(tableName, inputTableName, moveColNr))
		goto failure;
	}
	else 
	{ /* do a cross */
	 LOOK_UP_TABLE *t;
     	 if(nrCountSlots < nrIntervals)
     	 {
	   Error("-H,-n: number of count slots (is %d) must be greater than "
	         "number of intervals (is %d)", nrCountSlots, nrIntervals);
	   goto failure;
     	 }

	 nrMaps = argc - 2;
	 if (nrMaps == 0)
	 {
	  Error("No input maps specified");
	  goto failure;
	 }
	 in = ReadMaps((const char **)(argv+1),nrMaps);
	 if (in == NULL)
	  goto failure;

	 if (inputTableName != NULL)
	  /* existing table */
	   t = UpdateCrossTable(inputTableName, in, nrMaps);
	  else
	   t = MakeNewCrossTable(in,nrMaps,nrIntervals,
	                          histogram ? nrCountSlots : 0 );
	 if (t == NULL)
	  goto failure;
	 
	 WriteCrossTable(tableName, t, ommitZeros, RgetCellSize(in[0]));

	 FreeLookupTable(t);
	 FreeMaps(in, nrMaps);
	}

	AppEnd();
	exit(0);  			/* Successful exit */
	return 0; 			/* Never reached */

failure:
	FreeMaps(in,nrMaps);
	AppEnd();
	exit(1);
	return 1;
} /* main */
