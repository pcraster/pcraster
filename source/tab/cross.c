#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <string.h>
#include "table.h"
#include "misc.h"
#include "csf.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 
/* key index in single col tables */
#define INTERVAL 0
#define COUNT    1

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

static void SetValue(
	LOOK_UP_KEY *k,
	LOOK_UP_TEST t,
	double l,
	double h)
{
	k->t = t;
	k->l = l;
	k->h = h;
}

static LOOK_UP_TABLE *MakeSingleCross(
	CSF_VS vs,
	double minVal,
	double maxVal,
	size_t   nrInt)
{
	size_t i;
	LOOK_UP_TABLE *t = ChkMalloc(sizeof(LOOK_UP_TABLE));
	if (t == NULL)
	  return NULL;
	t->nrKeys = 1;
	t->keyVs = NULL;
	t->records = NULL;
	PRECOND(maxVal >= minVal);
        switch(vs) {
	 case VS_DIRECTION: 
	       if (minVal == maxVal)
	        t->nrRecords = 1;
	       else
	       {
	 	t->nrRecords = nrInt;
	 	if (minVal == -1)
	 	 t->nrRecords++;
	       }
	       break;
	 case VS_SCALAR:
	 case VS_CONTINUOUS: 
	       if (minVal == maxVal)
	        t->nrRecords = 1;
	       else
	        t->nrRecords = nrInt; 
	       break;
	 default: /* classified */
	       t->nrRecords = (int)(maxVal-minVal+1);
	}
        if (AllocLookupTable(t))
        	goto error;
        t->keyVs[0] = vs;
        switch(vs) {
         case VS_DIRECTION:
         {
          size_t i_s=0;; /* i start */
          if (minVal == maxVal || minVal == -1)
           SetValue(t->records[i_s++]+INTERVAL, TEST_ONE, minVal, minVal);
          if (minVal != maxVal)
          {
           double inc;
           if (minVal == -1)
                minVal = 0;
           inc = (maxVal-minVal)/nrInt;
           if (inc == 0)
            SetValue(t->records[i_s++]+INTERVAL, TEST_ONE, minVal, minVal);
           else
           { 
            for(i=0; i < nrInt; i++)
             SetValue(t->records[i+i_s]+INTERVAL, TEST_GE_LT, 
                         minVal+(i*inc), minVal+((i+1)*inc)); 
            t->records[t->nrRecords-1][INTERVAL].t = TEST_GE_LE;
            t->records[t->nrRecords-1][INTERVAL].h = maxVal; /* remove round errors */
           } 
           }
          }
          break;
         case VS_SCALAR:
	 case VS_CONTINUOUS: 
            if (t->nrRecords == 1)
                 SetValue(t->records[0]+INTERVAL, TEST_ONE, minVal, minVal);
            else
            {
             double inc = (maxVal-minVal)/nrInt;
             for(i=0; i < nrInt; i++)
              SetValue(t->records[i]+INTERVAL, TEST_GE_LT, minVal+(i*inc), 
                                                    minVal+((i+1)*inc)); 
             t->records[nrInt-1][INTERVAL].t = TEST_GE_LE;
             t->records[nrInt-1][INTERVAL].h = maxVal; /* remove round errors */
            }
            break;
          default: /* classified */
            { double inc = minVal;
             for(i=0; inc <= maxVal; inc += 1,i++)
              SetValue(t->records[i]+INTERVAL, TEST_ONE, inc, inc);
            }
        } /* eoswitch */
	return t;
error:
	FreeLookupTable(t);
	return NULL;
}

static int ApplyCross(
	LOOK_UP_TABLE *t, 
	MAP **maps)
{
	size_t m,r,c;
	size_t nrRows = RgetNrRows(maps[0]);
	size_t nrCols = RgetNrCols(maps[0]);
	double **buf = (double **)Malloc2d(t->nrKeys, nrCols, sizeof(double));
	double *key;
	if (buf == NULL)
		return 1;
	key = (double *)ChkMalloc(sizeof(double)*t->nrKeys);
	if (key == NULL)
		goto allocError;

	/* set types */
	for(m=0; m < t->nrKeys; m++)
		t->keyVs[m] = RgetValueScale(maps[m]);
	t->keyVs[t->nrKeys] = VS_UNDEFINED;

	/* set count to zero */
	for(r=0; r < t->nrRecords; r++)
	{
		t->records[r][t->nrKeys].t = TEST_ONE;
		t->records[r][t->nrKeys].l = 0;
	}

	/* count */
	for (r = 0; r < nrRows; r++)
	{
	  for(m=0; m < t->nrKeys; m++)
	   if (RgetRow(maps[m], r, buf[m]) != nrCols)
	   {
	    ErrorNested("read error on '%s'", MgetFileName(maps[m]));
	    goto readError;
	   }
	  for(c=0 ; c < nrCols; c++)
	  {
	   size_t k;
	   for(m=0; m < t->nrKeys && (!IS_MV_REAL8(buf[m]+c)) ; m++)
	    key[m] = buf[m][c];
	   if (m != t->nrKeys) /* mv read */
	        continue;
           for (k = 0; k < t->nrRecords; k++)
           {
           	k = FindCrossKey(t, key, k);
           	if (k != t->nrRecords)
           	{
           		t->records[k][t->nrKeys].l += 1;
           		PRECOND(t->records[k][t->nrKeys].t == TEST_ONE);
           	}
          } /* eofor all keys */
	 } /* eofor all cols */
      } /* eofor all rows */
      Free(key);
      Free2d((void **)buf, t->nrKeys);
      return 0;

readError:
      Free(key);
allocError:
      Free2d((void **)buf, t->nrKeys);
      return 1;
}


static LOOK_UP_TABLE *MakeHistoCross(
	MAP *m,
	size_t   nrInt,
	size_t   nrSlots)
{
	LOOK_UP_TABLE *h=NULL;
	double count,minVal, maxVal, chunk, total = 0;
	size_t r,h_i;
	CSF_VS vs = RgetValueScale(m);

	RgetMinVal(m, &minVal);
	RgetMaxVal(m, &maxVal);
	if (IS_MV_REAL8(&minVal) || IS_MV_REAL8(&maxVal))
		minVal = maxVal = 0;

	h = MakeSingleCross(vs, minVal, maxVal, nrSlots);
	if (h == NULL)
		return NULL;
	if (h->nrRecords == 1)
		return h;
	h->searchMethod = SEARCH_BINARY;
	if (ApplyCross(h, &m))
		goto error;
	for(r =0 ; r < h->nrRecords; r++)
	{
		total += h->records[r][h->nrKeys].l;
		POSTCOND(h->records[r][h->nrKeys].t == TEST_ONE);
	}
	POSTCOND(total <= (RgetNrRows(m)*RgetNrCols(m)) );
	chunk = total/nrInt;

	if (vs == VS_DIRECTION && h->records[0][INTERVAL].l == -1)
	{ /* keep single entry for flat */
	  total = h->records[0][COUNT].l;
	  r = h_i = 1; /* r result index, h_i histo input index */
	}
	else
	{
	  total =0; /* total already processed */
	  r = h_i = 0; /* r result index, h_i histo input index */
	}

	/* be aware that r and r_h are shadowed
	 * that's why I use count:
	 */

	/* init first interval */
	h->records[r][INTERVAL].t = TEST_GE_LT;
	h->records[r][INTERVAL].l = h->records[h_i][INTERVAL].l;
	h->records[r][COUNT].t    = TEST_ONE; 
	count                     = 0;

	for (  ; h_i < h->nrRecords; h_i++)
	{
		count                  += h->records[h_i][COUNT].l;
		total                  += h->records[h_i][COUNT].l;
		if (total >= chunk*(r+1) || (h_i == h->nrRecords-1))
		{ /* interval filled or last one */

		  /* end interval */
		  h->records[r][COUNT].l    = count;
		  h->records[r][INTERVAL].h = h->records[h_i][INTERVAL].h;
		  count = 0;
		  r++;

		  if (h_i < h->nrRecords-1) /* more to be done */
		  {
		    /* init new interval */
		    h->records[r][INTERVAL].t = TEST_GE_LT;
		    h->records[r][INTERVAL].l = h->records[h_i+1][INTERVAL].l;
		    h->records[r][COUNT].t    = TEST_ONE; 
		  }
		}
	}
	h->records = (LOOK_UP_KEY **)Realloc2d((void **)h->records, r, (size_t)2, 
	              h->nrRecords, (size_t)2, sizeof(LOOK_UP_KEY));
	PRECOND(h->records != NULL); /* always smaller */
        h->records[r-1][INTERVAL].t = TEST_GE_LE;
	h->nrRecords = r;
	return h;
error:
	FreeLookupTable(h);
	return NULL;
}


/* create a cross table 
 * cross scores are stored in in the
 * nrMaps column of the lookup keys
 * Scores are in pixels
 * returns the tables with scores
 * or zero in case of an error
 */
LOOK_UP_TABLE *MakeNewCrossTable(
	MAP **maps,
	size_t nrMaps,
	size_t nrInt,   /* number of intervals */
	size_t nrSlots) /* number of histo slots, 0 if not histogram stretched */
{
	LOOK_UP_TABLE **s = ChkCalloc((size_t)nrMaps,sizeof(LOOK_UP_TABLE *)); 
	                          /* array of single tables, NULL init */
	LOOK_UP_TABLE *t=NULL; /* final table, if set in loop then histo */
	size_t *count = NULL;
	size_t m, t_nr; /* number in t */

	if (s == NULL)
		return NULL;
	for ( m=0 ; m<nrMaps ; m++)
	{
		double minVal, maxVal;
		CSF_VS vs = RgetValueScale(maps[m]);
		RgetMinVal(maps[m], &minVal);
		RgetMaxVal(maps[m], &maxVal);
		if (IS_MV_REAL8(&minVal) || IS_MV_REAL8(&maxVal))
		   minVal= maxVal= 0;
		if (nrSlots > 0 &&
		       (vs==VS_DIRECTION||vs==VS_SCALAR||vs==VS_CONTINUOUS)
		   )
		  {
		   s[m] = MakeHistoCross(maps[m], nrInt, nrSlots);
		   t = s[m];
		  }
		else
		   s[m] = MakeSingleCross(vs, minVal, maxVal, nrInt);
		if (s[m] == NULL)
		{
		 nrMaps = m;
		 goto done;
		}
	}
	if (nrMaps == 1 && t != NULL) { /* one map, histo done */
		Free(s);
		return t;
	 }

	/* permutate,order is significant
	 *  due to matrix lay out and binary search
	 */
	t= (LOOK_UP_TABLE *)ChkMalloc(sizeof(LOOK_UP_TABLE));
	if (t == NULL)
		goto done;
	if (nrMaps == 2)
		t->nrMatrCols = s[0]->nrRecords;
	count = (size_t *)ChkMalloc(nrMaps*sizeof(size_t));
	count[0] = 1;
	for(m=1; m < nrMaps ;  m++)
		count[m] = s[m-1]->nrRecords * count[m-1];
	t_nr = count[nrMaps-1] * s[nrMaps-1]->nrRecords;
	t->nrRecords = t_nr;
	t->nrKeys = nrMaps;
	if (AllocLookupTable(t))
		goto error2;
	for(t_nr = 0; t_nr < t->nrRecords; t_nr++) 
	{
		int index = t_nr;
		m=nrMaps;
		do {
		 m--;
		 t->records[t_nr][m] = s[m]->records[index/count[m]][INTERVAL];
		 index %= count[m];
		} while (m != 0);
		POSTCOND(index == 0);
	}

	t->searchMethod = SEARCH_BINARY;
	if (!ApplyCross(t, maps))
		goto done; /* success */
error2: /* else error */
	FreeLookupTable(t);
	t= NULL;
done:
	Free(count);
	for(m=0; m < nrMaps; m++)
		FreeLookupTable(s[m]);
	Free(s);
	return t;
}

static LOOK_UP_TABLE *CutTable(
	LOOK_UP_TABLE *t,
	size_t nrCols)
{
	LOOK_UP_TABLE *n = (LOOK_UP_TABLE *)ChkMalloc(sizeof(LOOK_UP_TABLE));
	size_t i;

	PRECOND(nrCols >= 2); /* for memcpy keyVs */

	if (n == NULL)
		return NULL;
	*n = *t;
	n->nrKeys = nrCols-1;
	if (AllocLookupTable(n))
	{
		Free(n);
		FreeLookupTable(t);
		return NULL;
	}
	memcpy(n->keyVs,t->keyVs,sizeof(CSF_VS)*(nrCols-1));
	for(i=0; i < t->nrRecords; i++)
	 memcpy(n->records[i],t->records[i],sizeof(LOOK_UP_KEY)*nrCols);
	FreeLookupTable(t);
	return n;
}


LOOK_UP_TABLE *UpdateCrossTable(
	const char *crossTable,
	MAP **maps,
	size_t nrMaps)
{
	CSF_VS *keyVs = ChkMalloc(nrMaps * sizeof(CSF_VS));
	LOOK_UP_TABLE *t= NULL;
	FILE *f;
	size_t i;
	if (keyVs == NULL)
		return NULL;
	for (i=0; i< nrMaps; i++)
		keyVs[i] = RgetValueScale(maps[i]);
	f = fopen(crossTable,"r");
	if (f == NULL)
	{
		Error("Can't open '%s'", crossTable);
		goto failure;
	}
	t = ReadLookupTable(f,keyVs,nrMaps, VS_UNDEFINED);
	fclose(f);
	if (t == NULL)
	{
		Error("While reading file '%s'",crossTable);
		goto failure;
	}
	if (t->nrKeys+1 < nrMaps)
	{
		Error("table '%s' does not have enough columns, '%d' expected"
		      " but only '%d' read",crossTable,nrMaps,t->nrKeys+1);
		goto failure;
	}
	if (t->nrKeys != nrMaps)
	{
		t = CutTable(t,nrMaps+1);
		if (t == NULL)
			goto failure;
	}
	t->searchMethod = SEARCH_LINEAR;
	if (ApplyCross(t,maps))
		goto failure;
	Free(keyVs);
	return t;

failure:
	Free(keyVs);
	if (t != NULL)
		FreeLookupTable(t);
	return NULL;
}
