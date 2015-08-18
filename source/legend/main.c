#include "stddefx.h" 

/*
 * maplabel.c 
 */


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <string.h> 
#include "csf.h" 
#include "misc.h" 
#include "app.h" 

/* global header (opt.) and maplabel's prototypes "" */


/* headers of this app. modules called */ 
#include "select.h"

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

#define USAGE  \
 "USAGE: legend InputMaps\n" \
 "menu if -w, -f or -c are not specified\n" \
 "w f write output ascii file for labels\n" \
 "f f input ascii file for labels\n" \
 "c   copy legend of first map to other maps\n" \
 "l # new low value legend\n" \
 "h # new high value legend\n"

#define LINE_LENGTH ((size_t)128)

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/
	

static MAP *OpenClassMap(
	const char *name,
	enum MOPEN_PERM  perm)
{
	MAP *in = Mopen(name, perm);
	CSF_VS vs;
	if(in == NULL)
	{
	  Error("%s:%s", name,MstrError());
	  return NULL; 
	}
	vs = RgetValueScale(in);
	/* Check input map to have right value scale */
	if(!AppIsClassified(vs) || !RvalueScale2(vs) || vs == VS_LDD)
	{
	  Error("map '%s' has '%s' data type, no legend possible",
	           name, RstrValueScale(vs));
	  Mclose(in);
	  return NULL;
	}
	RuseAs(in, CR_INT4);
	return in;
}

static int WriteLegendFile(
	const char *outputFileName,
	CSF_LEGEND *legend, 
	int nrLegend)
{
	int i;
	FILE *f = fopen(outputFileName, "w");
	if (f == NULL)
		return RetError(1,"Can't create '%s'", outputFileName);
	if ( fprintf(f,"-0 %s\n",legend[0].descr) < 0 )
		goto failure;
	for(i=1; i < nrLegend; i++)
	 if ( fprintf(f,"%d %s\n",(int)legend[i].nr,legend[i].descr) < 0 )
		goto failure;
	fclose(f);
	return 0;

failure:
	fclose(f);
	return RetError(1,"Can't write to '%s'", outputFileName);
}

static int CmpLeg(
	const CSF_LEGEND *e1,
	const CSF_LEGEND *e2)
{
	return (int)(e1->nr -e2->nr);
}

static int SortLegend(
	CSF_LEGEND *l,
	int n)
{
	int i;
	PRECOND(n >=0);
	qsort(l+1, (size_t)n-1, sizeof(CSF_LEGEND), 
	      (QSORT_CMP)CmpLeg);
	for (i = 2; i < n; i++)
	  if (l[i].nr == l[i-1].nr)
	  	return RetErrorNested(1,"More than one entry with value '%d'", l[i].nr);
	return 0;
}

static void Blank0Descr(
	CSF_LEGEND *e)
{
	e->nr = 0;
	(void)memset(e->descr,'\0',(size_t)CSF_LEGEND_DESCR_SIZE); 
}

static CSF_LEGEND *NewLegend(
	size_t nrEntries)
{
	PRECOND(nrEntries >= 1);
	/* calloc -> make all blank */
	return (CSF_LEGEND *)ChkCalloc(sizeof(CSF_LEGEND), nrEntries);
}

static CSF_LEGEND *NewBlankLegend(
	int  *nrLegend,
	INT4 minVal,
	INT4 maxVal)
{
	int i;
	CSF_LEGEND *legend;
	if (minVal != MV_INT4)
	 *nrLegend = maxVal-minVal+2; /* 1 for name, 1 for interval */
	else
	 *nrLegend = 1;
	legend = NewLegend((size_t)*nrLegend); 
	if (legend == NULL)
		return NULL;
	PRECOND(*nrLegend >= 1);
	for(i=1; i < *nrLegend; i++)
	{
		Blank0Descr(legend+i);
		legend[i].nr = minVal+(i-1);
	}
	return legend;
}

static CSF_LEGEND *ReadMapLegends(
	int *nrOut,
	char **mapNames,
	int nrMapNames,
	INT4  userMinVal, /* user specified, MV if not specified */
	INT4  userMaxVal) /* user specified, MV if not specified */
{
	int  i;
	MAP  *map=NULL;

	CSF_LEGEND *resultLegend = NewLegend((size_t)1);
	int nrResultLegend = 1;
	INT4 mapsMin=MV_INT4, mapsMax=MV_INT4;

	if (resultLegend == NULL)
		goto failure;

/*
 *	if (userMinVal > userMaxVal)
 *	{
 *	 ErrorNested("high value ('%d') is smaller than low value ('%d') ",
 *	    userMaxVal, userMinVal);
 *	 goto failure;
 *	}
 */

	for(i=0; i < nrMapNames; i++)
	{
	  INT4 m;
	  int nrL, nrNew,r;
	  CSF_LEGEND *new,*mapL=NULL;
	  if ( (map = OpenClassMap(mapNames[i], M_READ)) == NULL)
	  	goto failure;

	  RgetMinVal(map,&m);
	  if (m != MV_INT4 && (mapsMin == MV_INT4 || m < mapsMin) )
	   mapsMin = m;
	  if (userMinVal != MV_INT4 && (mapsMin == MV_INT4 || userMinVal < mapsMin)) 
	   mapsMin = userMinVal;
	  RgetMaxVal(map,&m);
	  if (m != MV_INT4 && (mapsMax == MV_INT4 || m > mapsMax) )
	   mapsMax = m;
	  if (userMaxVal != MV_INT4 && (mapsMax == MV_INT4 || userMaxVal > mapsMax)) 
	   mapsMax = userMaxVal;

	  nrL = MgetNrLegendEntries(map);
	  if (nrL != 0 )
	  {
	  	mapL = NewLegend((size_t)nrL);
	  	if (mapL==NULL)
	  		goto failure;
		if (!MgetLegend(map,mapL))
		{
			Error("while reading legend of `%s`",mapNames[i]);
			Free(mapL);
			goto failure;
		}
	        Mclose(map); map = NULL;

		if (nrL > 1)
		{  
		 mapsMin = mapsMin==MV_INT4 ? mapL[1].nr:MIN(mapsMin, mapL[1].nr);
		 mapsMax = mapsMax==MV_INT4 ? mapL[nrL-1].nr:MAX(mapsMax, mapL[nrL-1].nr);
		}
	  }

	  /* now merge new one(= new legend and new mapsMin, mapsMax) 
	   * with the one we already have
	   */
	  new = NewBlankLegend(&nrNew,mapsMin,mapsMax);
	  if (new == NULL)
	  	{ Free(mapL); goto failure; }
	  strcpy(new[0].descr, resultLegend[0].descr); 
	  for(r = 1; r < nrResultLegend; r++)
	  {
		 PRECOND(resultLegend[r].nr >= mapsMin);
		 PRECOND(resultLegend[r].nr <= mapsMax);
		 PRECOND(new[resultLegend[r].nr-mapsMin+1].nr == resultLegend[r].nr);
		 strcpy(new[resultLegend[r].nr-mapsMin+1].descr, resultLegend[r].descr);
	  }
	  Free(resultLegend);
	  resultLegend = new;
	  nrResultLegend = nrNew;
	  if (nrL != 0)
	  {	/* add  new ones */
	  	PRECOND(mapL != NULL);
		if (resultLegend[0].descr[0] == '\0')
		 strcpy(resultLegend[0].descr, mapL[0].descr);
	  	for(r = 1; r < nrL; r++)
		{
		 PRECOND(mapL[r].nr >= mapsMin);
		 PRECOND(mapL[r].nr <= mapsMax);
		 PRECOND(resultLegend[mapL[r].nr-mapsMin+1].nr == mapL[r].nr);
		 if (resultLegend[mapL[r].nr-mapsMin+1].descr[0] == '\0')
		   strcpy(resultLegend[mapL[r].nr-mapsMin+1].descr, mapL[r].descr);
		}
		Free(mapL);
	  }
	} /* eofor */

	POSTCOND(nrResultLegend > 0);
	*nrOut = nrResultLegend;
	return resultLegend;
failure:
	Free(resultLegend);
	if (map != NULL)
	 Mclose(map);
	return NULL;
}

static void ToLong(
	int line)
{
 ErrorNested("line '%d' only '%d' characters allowed for legend entry",
             line, CSF_LEGEND_DESCR_SIZE-1);
}

/* Reads a legend from a file.
 * Returns NULL in case of an error, legend otherwise.
 */
static CSF_LEGEND *ReadLegendFile(
	int *nrOut,
	const char *inputFileName, 
	CSF_VS     vs,
	CSF_CR     cr)
{
	FILE *f = fopen(inputFileName,"r");
	char  	buf[LINE_LENGTH];
	int 	l = 0; /* linenr */
	int     nrLegend=0;
	CSF_LEGEND *legend = NULL; /* realloced each time */

	if (f == NULL)
	{
		ErrorNested("Can't open '%s'",inputFileName);
		return NULL;
	}

	while( memset(buf,'\0',LINE_LENGTH) != NULL &&
	       (fgets(buf, (int)LINE_LENGTH, f) != NULL)
	     )
	{
	 INT4 nr;
	 char *descr, *nrPtr;

	 l++;
	 if (buf[LINE_LENGTH-1] != '\0') /* one reason for memset */
	 {
		ToLong(l);
		goto failure;
	 }
	 (void)LeftRightTabTrim(buf);
	 if(EmptyString(buf))
		continue; /* skip empty line */
	 nrPtr = strchr(buf,' ');
	 if (nrPtr == NULL)
	 {
	    descr = buf+strlen(buf); /* empty string */
	    /* NOT
	 	ErrorNested("line does not contain a number and a desc");
	     */
	 }
	 else
	 {
	    descr = nrPtr+1;
	    *nrPtr = '\0'; /* terminate nr */
	 }
	 nrPtr = buf;

	 if (strlen(descr) >= CSF_LEGEND_DESCR_SIZE)
	 {
	 	ToLong(l);
	 	goto failure;
	 }

	/* allocate next one
	 */
	 legend = ChkRealloc(legend, sizeof(CSF_LEGEND) * (nrLegend+1));
	 if (legend == NULL)
	  goto failure;
	 Blank0Descr(legend+nrLegend); /* blank the new one */

	 /* should be on the first one */
	 if ( StrEq(nrPtr,"-0"))
	 {
	   /* it's the legend name */
	   if (nrLegend != 0)
	   {
	    ErrorNested("line '%d', contains the name of the legend (-0) but it's not "
	                "the first line",l);
	    goto failure;
	   }
	   legend[nrLegend].nr = 0;
	   (void)strcpy(legend[0].descr,descr); 
	   nrLegend++;
	   continue; /* next one */
	 }
	 else 
	 { /* not the legend name, if this is the first 
	    * one read then create empty name first
	    */
	    if (nrLegend == 0)
	    {
	     nrLegend++;
	     legend = ChkRealloc(legend, sizeof(CSF_LEGEND) * (nrLegend+1));
	     if (legend == NULL)
	      goto failure;
	     }
	     Blank0Descr(legend+nrLegend); /* blank the new one */
	 }

	/* check nr on being valid value */
	if (AppCheckVal(nrPtr, vs, cr))
		goto failure;
	(void)CnvrtINT4(&nr, nrPtr);

	(void)memset(legend[nrLegend].descr,'\0',(size_t)CSF_LEGEND_DESCR_SIZE);
	(void)strcpy(legend[nrLegend].descr, descr);
	legend[nrLegend].nr = nr;
	nrLegend++;
       } /* eowhile */

	if (nrLegend == 0)
	{
	  ErrorNested("empty file");
	  goto failure;
	}

	if (!feof(f))
	{
	 ErrorNested("read error");
	 goto failure;
	}

	if (SortLegend(legend, nrLegend))
		goto failure;
	*nrOut = nrLegend;
	return legend;

failure:
	fclose(f);
	Free(legend);
	return NULL;
}


int main(int argc,			/* number of arguments */
	char *argv[])			/* list of arguments */
{
	MAP 	*in=NULL;		/* 1st input map */
	int nrMaps = 0;
	int 	i,c; 
	CSF_VS 	valueScale;
	CSF_CR cellRepr;		/* value scale 1st input map */
  	CSF_LEGEND *theLegend = NULL;
  	int nrTheLegend = 0;
	/* options */
	const char *inputFileName = NULL;	/* -f input ascii file */
	const char *outputFileName = NULL;	/* -w output ascii file */
	BOOL 	copy = FALSE;           /* -c */
	INT4    minVal = MV_INT4, maxVal = MV_INT4; /* -l, -h */
	
	/* Initialize the arguments */

	/* install application */
	if(InstallArgs(argc, argv, "(w*f*c)l#h#", "legend", __DATE__))
		goto failure;

	/* get all local options and arguments */
	while((c = GetOpt()) != 0)
	{
	 switch(c)
	 {
		case 'f':
		 inputFileName = OptArg;
		 if (AppInputTest(inputFileName))
		 	goto failure;
		 break;
		case 'w':
		 outputFileName = OptArg;
		 break;
		case 'c':
		 copy = TRUE;
		 break;
		case 'l':
		 minVal = *((const int*) OptArg);
		 break;
		case 'h':
		 maxVal = *((const int*) OptArg);
		 break;
	}
       }

	/* set repr for nominal and ordinal
	 * at large, needed to parse the -f file
	 */
	appLarge = TRUE;

       if (copy) /* low high */
	 minVal = maxVal = MV_INT4;

	if ( minVal != MV_INT4 && maxVal != MV_INT4 && minVal > maxVal)
	{
		Error("high value ('%d') is smaller than low value ('%d') ",
		    maxVal, minVal);
		goto failure;
	}
	if ( (argv = ArgArguments(&argc)) == NULL)
		goto failure;

	if (AppArgCountCheck(argc,2,-1,USAGE))
		goto failure;

	nrMaps = argc-1;

	/* get info from first map */
	in = OpenClassMap(argv[1], M_READ);
	if(in == NULL)
	    goto failure;
	valueScale = RgetValueScale(in);
	cellRepr = RgetCellRepr(in);
	/* check additional maps */
	for(i =  1 ; i <  nrMaps; i++)
	{
	  MAP *m = OpenClassMap(argv[i+1], M_READ);
	  CSF_VS vs = RgetValueScale(m);
	  if (vs != valueScale)
	  {
		Error("Conflicting data types: '%s' is %s,'%s' is %s",
		       argv[1],RstrValueScale(valueScale),argv[i+1],
		       RstrValueScale(vs));
		Mclose(m);
		goto failure;
	  }
	  if(LimitedVersionCheck((int)RgetNrRows(m),(int)RgetNrCols(m),-1,-1,-1,-1))
	  	{ Mclose(m); goto failure;}
	  Mclose(m);
	}
	Mclose(in);
	in = NULL;

	/* read legend from first map or input file
	 */
	if (inputFileName != NULL)
	{
		theLegend = ReadLegendFile(&nrTheLegend,inputFileName, valueScale, cellRepr);

		if (theLegend == NULL)
		{
			Error("While reading file '%s':",inputFileName);
			goto failure;
		}
	}
	else
	{
	  theLegend = ReadMapLegends(&nrTheLegend,argv+1,copy ? 1 : nrMaps, 
	                          minVal, maxVal);
	  if (theLegend == NULL)
		goto failure;
	}


	POSTCOND(nrTheLegend > 0);
	if (outputFileName != NULL)
	{
		if (WriteLegendFile(outputFileName,theLegend, nrTheLegend))
			goto failure;
		goto done;
	}
	
	PRECOND(theLegend[0].nr == 0);
	if (inputFileName == NULL && (!copy) )
	{
	  switch(Menu(theLegend,nrTheLegend, (const char **)argv+1,nrMaps))
	  {
	   case 0 : goto failure;
	   case 1 : break; /* resume */
	   case 2 : AppVerbose("No legend modified\n");
	            goto done;
	  }
	}

	/* legend always sorted in app */
	PRECOND(!SortLegend(theLegend,nrTheLegend));

	/* write legend to all files 
	 *  to all in non-copy mode
	 *  to additional maps only in copy mode
	 */
	PRECOND(theLegend[0].nr == 0);
	 for(i = copy ? 1 : 0 ; i < nrMaps; i++)
	 {
	  MAP *m = OpenClassMap(argv[i+1], M_READ_WRITE);
	  if(m == NULL)
	      goto failure;
	  if (!MputLegend(m, theLegend, (size_t)nrTheLegend))
	  {
	  	Error("writing to map '%s' failed", argv[i+1]);
	  	goto failure;
	  }
	  Mclose(m);
	 }

done:
	Free(theLegend);
	AppEnd();
	exit(0);  			/* Successful exit */
	return 0; 			/* Never reached */

failure:
	if (in != NULL)
		Mclose(in);
	AppEnd();
	exit(1);
	return 1;
} /* main */
