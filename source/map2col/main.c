#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "csf.h"
#include "app.h"	/* GetOpt, InstallArgs, ArgArguments */

/* apps. called */
#include "map2col.h"
#include <string.h> /* strchr */

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

#define USAGE \
 "USAGE: map2col MapInputFiles ColumnOutputFile\n"\
 " p   plain format (default)\n"\
 " g   geoeas format\n"\
 " m s MV string\n"\
 " M   print MV records also\n"\
 " f s print format(s) (default best fit)\n"\
 " s s separator string\n"\
 " a f append to these records\n"\
 " x # column of x-coordinate\n"\
 " y # column of y-coordinate\n"\
 " r   row wise output (default)\n"\
 " c   column wise output\n"

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */ 
/******************/

static int CheckFmt(const char *fmt, size_t fmtNr)
{ /* copied in map2asc/main.c */
	if (strchr("feEgG", fmt[strlen(fmt)-1]) == NULL /* no fmt end */
	    || atoi(fmt) <= 0 ) /* should start with a non-neg number */
	{
	 Error("-f: format no. %u '%s' is not C floating point format",fmtNr,fmt);
	 return 1;
	}
	return 0;
}

int main(int argc,		/* number of arguments */
	char *argv[])		/* list of arguments */
{

	INP_MAP *maps;
	size_t 	i, nrMaps, nrMapsFinal;
	int c;
	size_t xcoord = 1;
	size_t ycoord = 2;
	BOOL geoEas = FALSE;
	BOOL printMV = FALSE;
	const char *mv = "1e31";
	const char *separator = NULL;
	char *usrFormat=NULL;
	const char *inputColumnFile = NULL; /* not used */
	BOOL colWise = FALSE;

	if(InstallArgs(argc,argv,"x#y#m*s*(rc)f*M(pg)a*","map2col",__DATE__))
		goto failure;

	while((c = GetOpt()) != 0)
	{
	 switch(c){
		case 'x':
			c = *((const int *) OptArg);
			if (c <= 0)
			{
			 Error("-x: x column should be greater than 0");
			 goto failure;
			}
			xcoord =(size_t)c;
			break;
		case 'y':
			c = *((const int *) OptArg);
			if (c <= 0)
			{
			 Error("-y: y column should be greater than 0");
			 goto failure;
			}
			ycoord = (size_t)c;
			break;
		case 'm':
			mv = (const char *)OptArg;
			break;
		case 's':
			separator = (const char *)OptArg;
			break;
		case 'M':
			printMV = TRUE;
			break;
		case 'r':
			colWise = FALSE;
			break;
		case 'c':
			colWise = TRUE;
			break;
		case 'p':
			geoEas = FALSE;
			break;
		case 'g':
			geoEas = TRUE;
			break;
		case 'f':
		if((usrFormat=StrcpyChkMalloc(OptArg))==NULL)
			goto failure;
			break;
		case 'a':
			inputColumnFile = (const char *)OptArg;			        
	 		if (AppInputTest(inputColumnFile))
				goto failure;
			break;
	 }
	}

	if ( (argv = ArgArguments(&argc)) == NULL)
		goto failure;

	if(AppArgCountCheck(argc,3,-1,USAGE))
		goto failure;

	nrMapsFinal = (argc - 2)+((inputColumnFile!=NULL)?0:2);
	if (inputColumnFile == NULL)
	{
	 if(xcoord == ycoord)
	 {
		Error("-x,-y: x and y can not be put in same column (%u)", xcoord);
		goto failure;
	 }
	 if(xcoord > nrMapsFinal)
	 {
		Error("-x: x column (%u) does not exist, %u columns available", 
		 xcoord, nrMapsFinal);
		goto failure;
	 }
	 if(ycoord > nrMapsFinal)
	 {
		Error("-y: y column (%u) does not exist, %u columns available", 
		 ycoord, nrMapsFinal);
		goto failure;
	 }
	}
	else if(separator != NULL && strlen(separator) > 1)
	{
		Error("-s,-a: separator must be one character in append mode (not %s)",
		       separator);
		goto failure;
	}

	--xcoord;
	--ycoord;
	/* Read the input maps */
	maps = ChkMalloc(nrMapsFinal * sizeof(INP_MAP));
	if(maps == NULL)
		goto failure;
	for(i = 0; i < nrMapsFinal; i++) {
		maps[i].type = 'v';
	   strcpy(maps[i].usrFmt,""); /* default empty */
	}
	if (inputColumnFile == NULL)
	{
	 POSTCOND(nrMapsFinal >= 3);
	 maps[xcoord].type = 'x';
	 maps[ycoord].type = 'y';
	}

	/* init format */
	if (usrFormat != NULL) {
	 char *p;
	 i = TokenSpaceTrim(usrFormat);
	 if (i == 0 ) {
		Error("-f: no format string found");
		goto failure;
	 }
	 i=0; /* current fmt-index */
	 p=strtok(usrFormat," ");
	 while ( p != NULL) {
	   if (CheckFmt(p,i+1))
	   	goto failure;
	 	strcpy(maps[i].usrFmt,p);
	   p=strtok(NULL," ");
	   i++;
	 }
	 /* copy last fmt to all remaining */
	 for( ; i < nrMapsFinal; i++)
	 	strcpy(maps[i].usrFmt,maps[i-1].usrFmt);
	}

	nrMaps = 0; /* keep track of init maps */
	for(i = 1; i < (size_t)argc-1; i++)
	{
	 MAP *lastMap=NULL;
	 int nr,nc;
	 while (maps[nrMaps].type != 'v') /* skip x and y */
	      maps[nrMaps++].m = NULL;
	 if (AppInputTest(argv[i]))
		goto failure1;
	 if ( (maps[nrMaps].m = Mopen(argv[i], M_READ)) == NULL)
	 {
	 	Error("'%s' is not a (CSF) map file",argv[i]);
	 	goto failure1;
	 }
	 RuseAs(maps[nrMaps].m, CR_REAL8);
	 nr = RgetNrRows(maps[nrMaps].m);
	 nc = RgetNrCols(maps[nrMaps].m);
	 if (LimitedVersionCheck(nr,nc,-1,-1,-1,-1))
	 	goto failure1;
	 nrMaps++;
	 if ( lastMap != NULL && 
	     MgetProjection(lastMap) != MgetProjection(maps[nrMaps-1].m) )
	 {
	 	Error("Projection differs between '%s' and '%s'",
	              MgetFileName(maps[0].m), 
	              MgetFileName(maps[nrMaps-1].m) );
	 	goto failure1;
	 }
	 	else lastMap = maps[nrMaps-1].m;
	}

	if(Map2Col(maps, 
	           argv[argc-1], /* = outputFile */ 
	           nrMapsFinal,
	           xcoord, ycoord,
	           mv,  separator,
	           geoEas, colWise, printMV,
	           inputColumnFile
	           ))
		goto failure1;

	for(i=0; i < nrMaps; i++)
	   if (maps[i].m != NULL)
		Mclose(maps[i].m);
	Free(maps);
	Free(usrFormat);
	AppEnd();
	exit(0);
	return 0;

failure1:
	for(i=0; i < nrMaps; i++)
	   if (maps[i].m != NULL)
		Mclose(maps[i].m);
	Free(maps);
failure:
	Free(usrFormat);
	AppEnd();
	exit(1);
	return 1;
} /* main */
