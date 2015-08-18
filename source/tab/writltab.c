#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <math.h>
#include <string.h>
#include "misc.h"
#include "app.h"
#include "table.h"

/* global header (opt.) and test's prototypes "" */
#include "table_p.h"

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/* definition of SPEC (LIBRARY_INTERNAL)
 */
typedef struct SPEC {
         BOOL allOne;
         size_t lFmt;
         size_t hFmt;
} SPEC;

#define LOW_DEFS   (             \
         (1<<TEST_ONE   )        \
	|(1<<TEST_GE_INF)        \
	|(1<<TEST_GT_INF)        \
	|(1<<TEST_GE_LE )        \
	|(1<<TEST_GT_LE )        \
	|(1<<TEST_GE_LT )        \
	|(1<<TEST_GT_LT ) )
#define LOW_DEFINED(t)	((1<<t) & LOW_DEFS)
#define HIGH_DEFINED(t)	((t) > 3)

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

static void BufPrint(
	char *buf,
	double v)
{
	if (floor(v) == v)
		sprintf(buf,"%.0f",v);
	else
		sprintf(buf,"%g",v);
}

static void PrintKey(
	FILE *f,
	const SPEC *s,
	const LOOK_UP_KEY *k,
        BOOL lastCol)
{
	char lBuf[64], hBuf[64];
	const char *fmts[]    = { "",
                                  "<%-*s,%-*s>",
                                  "[%-*s,%-*s>",
                                  "<%-*s,%-*s>",
                                  "<%-*s,%-*s]",
                                  "[%-*s,%-*s]",
                                  "<%-*s,%-*s]",
                                  "<%-*s,%-*s>",
                                  "[%-*s,%-*s>",
                                  "<%-*s,%-*s>"};

        PRECOND(ARRAY_SIZE(fmts) > k->t);

        if (LOW_DEFINED(k->t))
        {
        	if (lastCol)
        	 BufPrint(lBuf,k->l);
        	else
        	 BufPrint(lBuf,k->l);
        }
        else
        	lBuf[0] = '\0';

        if (HIGH_DEFINED(k->t))
        	BufPrint(hBuf,k->h);
        else
        	hBuf[0] = '\0';

	if (k->t == TEST_ONE)
	{
		int len = s->lFmt+s->hFmt; 
		if (s->allOne)
		 fprintf(f,"%-*s",len,lBuf);
		else
		 fprintf(f," %-*s",len+2,lBuf); /* minus one for preceding space
		                                * plus 3 for [,] 
		                                */
	}
	else
		fprintf(f,fmts[k->t], s->lFmt,lBuf,s->hFmt,hBuf);
}

static const LOOK_UP_TABLE *CreateMatrix(
	const LOOK_UP_TABLE *t)
{
  LOOK_UP_TABLE *m = ChkMalloc(sizeof(LOOK_UP_TABLE));
  size_t r,i;
  if (m == NULL)
  	return NULL;
  /* determine nrCols => nrKeys */
  m->nrKeys= t->nrMatrCols;
  POSTCOND( (t->nrRecords%m->nrKeys) == 0);
  m->nrRecords = (t->nrRecords/m->nrKeys)+1;
  m->records = NULL;
  m->keyVs = NULL;
  if (AllocLookupTable(m))
  {
  	FreeLookupTable(m);
  	return NULL;
  }
  /* left/top */
  m->records[0][0].l = 0;
  m->records[0][0].t = TEST_ONE;
  /* col-index */
  for(i=1; i < (m->nrKeys+1); i++)
    m->records[0][i] = t->records[i-1][0];
  /* row-index */
  for(i=1; i < m->nrRecords; i++)
    m->records[i][0] = t->records[(i-1)*t->nrMatrCols][1];
  /* fill matrix */
  for(r=1; r < m->nrRecords; r++)
   for(i=1; i < (m->nrKeys+1); i++)
   {
    m->records[r][i] = t->records[(r-1)*m->nrKeys+(i-1)][2]; 
    POSTCOND(m->records[r][i].t == TEST_ONE  ||
             m->records[r][i].t == TEST_NOKEY );
    m->records[r][i].t = TEST_ONE;
   }
  return m;
}

static LOOK_UP_KEY *ConvDir(
	const LOOK_UP_KEY *key,
	CSF_VS vs)
{
	static LOOK_UP_KEY b;
	b = *key;
	if (vs == VS_DIRECTION)
	{
		if (LOW_DEFINED(b.t))
		 b.l = AppOutputDirection(b.l);
		if (HIGH_DEFINED(b.t))
		 b.h = AppOutputDirection(b.h);
	}
	return &b;
}
/* write a lookup or cross table to a file
 * if (t->records[r][t->nrKeys].t == TEST_NOKEY) then record r is not written
 * returns 
 * 0 if succesfull, 1 if failure
 */
int WriteLookupTable(
	const char *fileName,
	const LOOK_UP_TABLE *table)
{
	size_t k,r;
	SPEC *spec;
	FILE *f;
	LOOK_UP_KEY *key; /* copy necc. for directional conv. */
	BOOL matrWrite = (app2dMatrix && table->nrKeys == 2);
	const LOOK_UP_TABLE *t = table;
	if (matrWrite)
	{
		t = CreateMatrix(t);
		if (t == NULL)
		 return	RetError(1,"While writing file '%s'",fileName);
	}
	spec = (SPEC *)ChkMalloc(sizeof(SPEC)*(t->nrKeys+1));
	if (spec == NULL)
		goto error;
	f = fopen(fileName,"w");
	if (f == NULL)
	{
		Error("Can't create file '%s'",fileName);
		goto error;
	}
	for (k = 0; k < t->nrKeys+1; k++) 
	{
         spec[k].allOne  = TRUE;
         spec[k].lFmt = 0;
         spec[k].hFmt = 0;
	 for(r=0; r < t->nrRecords; r++)
	 if (t->records[r][t->nrKeys].t != TEST_NOKEY)
	 {
	 	char buf[64];
	 	key = ConvDir(t->records[r]+k, t->keyVs[k]);
	 	spec[k].allOne &= (key->t == TEST_ONE);
		if (LOW_DEFINED(key->t))
		{
			if (k == t->nrKeys)
			 BufPrint(buf,key->l);
			else
			 BufPrint(buf,key->l);
			spec[k].lFmt = MAX(spec[k].lFmt,strlen(buf));
		}
		if (HIGH_DEFINED(key->t))
		{
			BufPrint(buf,key->h);
			spec[k].hFmt = MAX(spec[k].hFmt,strlen(buf));
		}
	 }
        }
        for (r=0; r < t->nrRecords; r++)
	if (t->records[r][t->nrKeys].t != TEST_NOKEY)
       {
	 for(k=0; k < (t->nrKeys+1); k++)
	 {
	 	key = ConvDir(t->records[r]+k, t->keyVs[k]);
		PrintKey(f,spec+k,key, k == t->nrKeys);
		if (k != t->nrKeys)
			fprintf(f," ");
	 }
         if (fprintf(f,"\n") < 0)
         {
           Error("Writing to '%s' failed",fileName);
           goto errorWrite;
         }
        } /* eofor each line */
	if (matrWrite)
		FreeLookupTable((LOOK_UP_TABLE *)t);
	fclose(f);
	return 0;
errorWrite:
         fclose(f);	
         remove(fileName);
error:
	if (matrWrite)
		FreeLookupTable((LOOK_UP_TABLE *)t);
	return 1;
}
