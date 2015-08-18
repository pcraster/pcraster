#include "stddefx.h" 


/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include "misc.h"
#include "mathx.h"      /* GassDev */
#include "calc.h"
#include "table.h"	/* SEARCH_TABLE, STfind, Insert, 
			 * STnew and STfree.
			 */

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 
/* LOCAL_GET_FUNC (LIBRARY_INTERNAL)
 */
typedef int (* LOCAL_GET_FUNC)(REAL8 *v, int r, int c, const void *mapPtr);

/* DATA (LIBRARY_INTERNAL)
 */
typedef struct DATA {
	INT4   id;	   	/* id  (or class value) */
				/* id equals index in fastlist */
	REAL8 count;		/* nr of cells in class */
	INT4 maj;		/* value of majority */

	union value{
	REAL8 minMax;        		/* min or max of cells in class */
	REAL8 total;			/* total of class */
	struct SEARCH_TABLE *tab;   	/* count of classes and area */
	}value;
} DATA;

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* Returns id of a given record.
 */
static int RetIdArea(const DATA *f)	/* given record */
{
	PRECOND(f != NULL);	/* can not return id if record empty */
	return f->id;
}

/* Compares two records according to their id.
 * Returns a value > 0 if e1 > e2, 0 if e1 = e2, a value < 0 otherwise.
 */
static int CmpStatCont(
	const DATA *e1,		/* 1st argument to compare */
	const DATA *e2)		/* 2nd argument to compare */
{
	PRECOND(e1 != NULL && e2 != NULL);
	return((e1->id) - (e2->id));
}

static void InitRecAve(
	DATA *e,
	int  id)
{
	e->id=id;
	e->value.total=0;
	e->count=0;
}

static void PutMinMax(
     MAP_REAL8 *min,		/* write-only output minimum or max map  */ 
     const MAP_INT4 *class, 	/* input classes map	*/
     const SEARCH_TABLE *table)
{
	int 		r, c;
	int nrRows = class->NrRows(class);
	int nrCols = class->NrCols(class);
	/* scan map to put min or max values in the output map */
	for(r = 0; r < nrRows; r++)
	{
		for(c = 0; c < nrCols; c++)
		{
			INT4 id;
			if(class->Get(&id, r, c, class)) 
			{
				DATA key, *record;
				key.id = id;
				record = STfind(table, &key);
				if (record != NULL && record->count != 0)
				 min->Put(record->value.minMax, r, c, min);
				else
				 min->PutMV(r, c, min);
			}
			else	/* MV in -> MV out */
				min->PutMV(r, c, min);
		 }
	}
}

/* Determines minimum of an area with one class value.
 * Uses a fast list and a slow list. The fast list is already allocated, 
 * on the slow list a binary search has to be done every time.
 * Returns 1 if memory allocation fails, 0 otherwise.
 */
int AreaMin(
     MAP_REAL8 *min,		/* write-only output minimum map  */ 
     const MAP_REAL8 *val, 	/* input value map	*/
     const MAP_INT4 *class) 	/* input classes map	*/
{
	int 		r, c, nrRows, nrCols;
	SEARCH_TABLE 	*table;		/* table */

	val->SetGetTest(GET_MV_TEST, val);
	class->SetGetTest(GET_MV_TEST, class);

	nrRows = class->NrRows(class);
	nrCols = class->NrCols(class);

	/* allocate and initialize the search table */
	table = STnew((size_t)class->HintNrFastList(class), 
		  sizeof(DATA), (RETURN_ID)RetIdArea,
		  (INIT_REC)InitRecAve, (QSORT_CMP) CmpStatCont);
	if(table == NULL)
		return 1;

	/* scan map to calculate the minimum value */
	for (r = 0; r < nrRows; r++)
	{
	 	for (c = 0; c < nrCols; c++)
		{
		 INT4 		id;	
		 REAL8 		value;
		 if(class->Get(&id, r, c, class) &&
		    val->Get(&value, r, c, val))
			 {
				DATA key, *record;
				key.id = id;
				record = STfindOrInsert(table, &key);
				if(record == NULL)
				{
					STfree(table);
					return 1;
				}
				if (record->count == 0)
				{
					record->value.minMax = value;	
					record->count = 1;
				}
				record->value.minMax=
				 MIN( value, record->value.minMax);	
			 }
                }
       }
	PutMinMax(min,class,table);
	STfree(table);
	return 0;
}

/* Determines maximum of an area with one class value.
 * Uses a fast list and a slow list. The fast list is already allocated, 
 * on the slow list a binary search has to be done every time.
 * Returns 1 if memory allocation fails, 0 otherwise.
 */
int AreaMax(
     MAP_REAL8 *max,		/* write-only output maximum map  */ 
     const MAP_REAL8 *val, 	/* input value map	*/
     const MAP_INT4 *class) 	/* input classes map	*/
{
	int 		r, c, nrRows, nrCols;
	SEARCH_TABLE 	*table;			/* table */

	val->SetGetTest(GET_MV_TEST, val);
	class->SetGetTest(GET_MV_TEST, class);

	nrRows = class->NrRows(class);
	nrCols = class->NrCols(class);

	/* allocate and initialize the search table */
	table = STnew((size_t)class->HintNrFastList(class), 
		  sizeof(DATA), (RETURN_ID)RetIdArea,
		  (INIT_REC)InitRecAve, (QSORT_CMP) CmpStatCont);
	if(table == NULL)
		return 1;

	/* scan map to calculate the maximum value */
	for (r = 0; r < nrRows; r++)
	{
	 	for (c = 0; c < nrCols; c++)
		{
		 INT4 		id;	
		 REAL8 		value;
		 if(class->Get(&id, r, c, class) &&
		    val->Get(&value, r, c, val))
			 {
				DATA key, *record;
				key.id = id;
				record = STfindOrInsert(table, &key);
				if(record == NULL)
				{
					STfree(table);
					return 1;
				}
				if (record->count == 0)
				{
					record->value.minMax = value;	
					record->count = 1;
				}
				record->value.minMax=
				 MAX( value, record->value.minMax);	
			 }
                }
       }
       PutMinMax(max,class,table);

	STfree(table);		/* deallocate the table */
	return 0;
}


static int GetInt4(
	REAL8 *v,
	int    r,
	int    c,
	const MAP_INT4 *m)
{
	INT4 v4;
	int result = m->Get(&v4,r,c,m); 
	if (result)
		*v = v4;
	return result;
}

static int GetReal8(
	REAL8 *v,
	int    r,
	int    c,
	const MAP_REAL8 *m)
{
	return  m->Get(v,r,c,m); 
}

static SEARCH_TABLE *TotalTable(
     LOCAL_GET_FUNC   get,    /* how to value from val */
     const void *val, 	/* MAP_INT4 or MAP_REAL8 */
     const MAP_INT4 *class) 	/* input classes map	*/
{
	int 		r, c, nrRows, nrCols;
	INT4 		id;			/* value in class map */
	REAL8 		value;			/* value in value map */
	SEARCH_TABLE 	*table;			/* table */

	nrRows = class->NrRows(class);
	nrCols = class->NrCols(class);

	table = STnew((size_t)class->HintNrFastList(class), 
		  sizeof(DATA), (RETURN_ID)RetIdArea,
		  (INIT_REC)InitRecAve, (QSORT_CMP) CmpStatCont);
	if(table == NULL)
		return NULL;

	/* scan the map to calculate the average value of each area */
	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
	{
		 if(class->Get(&id, r, c, class) &&
			  get(&value, r, c, val))
		 {
			DATA *record, key;
			key.id = id;
			record = STfindOrInsert(table, &key);
			if(record == NULL)
			{ 	
				STfree(table);
				return NULL;
			}
			record->value.total += value;	
			record->count++;	
		 }
		 /* ignore MV */
	}
	return table;
}

static int AreaGeneration(
     MAP_REAL8       *result,
     const MAP_INT4  *class, 	/* input classes map	*/
     double (*genFunc)(void))
{
	int 		r, c, nrRows, nrCols;
	SEARCH_TABLE 	*table;			/* table */

	nrRows = class->NrRows(class);
	nrCols = class->NrCols(class);

	class->SetGetTest(GET_MV_TEST, class);

	table = STnew((size_t)class->HintNrFastList(class), 
		  sizeof(DATA), (RETURN_ID)RetIdArea,
		  (INIT_REC)InitRecAve, (QSORT_CMP) CmpStatCont);
	if(table == NULL)
		return 1;
	/* use value.total to determine if it's
	 *   the first time (gen a number)
	 * use count to store the number 
	 */ 

	/* scan the map to calculate the average value of each area */
	for (r = 0; r < nrRows; r++)
	for (c = 0; c < nrCols; c++)
	{
		 INT4 id;/* value in class map */
		 if(class->Get(&id, r, c, class))
		 {
			DATA *record, key;
			key.id = id;
			record = STfindOrInsert(table, &key);
			if(record == NULL)
			{ STfree(table);
			  return 1;
			}
			if (record->value.total == 0) 	
			{ record->value.total = 1;
			  record->count = genFunc();
			}
			result->Put(record->count,r,c,result);
		 }
		 else
		  result->PutMV(r,c,result);
	}
	STfree(table);
	return 0;
}

int AreaNormal(
     MAP_REAL8 *result,	/* write-only output  map  */ 
     const MAP_INT4 *class) 	/* input classes map	*/
{
 return AreaGeneration(result,class,GasDev);
}

int AreaUniform(
     MAP_REAL8 *result,	/* write-only output  map  */ 
     const MAP_INT4 *class) 	/* input classes map	*/
{
 return AreaGeneration(result,class,Ran);
}

/* Determines average of an area with one class value.
 * Uses a fast list and a slow list. The fast list is already allocated, 
 * on the slow list a binary search has to be done every time.
 * Returns 1 if memory allocation fails, 0 otherwise.
 */
int AreaAverage(
     MAP_REAL8 *average,	/* write-only output average map  */ 
     const MAP_REAL8 *val, 	/* input value map	*/
     const MAP_INT4 *class) 	/* input classes map	*/
{
	int 		r, c, nrRows, nrCols;
	SEARCH_TABLE 	*table;			/* table */

	val->SetGetTest(GET_MV_TEST, val);
	class->SetGetTest(GET_MV_TEST, class);

	nrRows = class->NrRows(class);
	nrCols = class->NrCols(class);

	table = TotalTable((LOCAL_GET_FUNC)GetReal8,val,class);
	if(table == NULL)
		return 1;

	/* scan the map to put the average values in the output map */
	for (r = 0; r < nrRows; r++)
	{
		for (c = 0; c < nrCols; c++)
		{
			INT4 	id;	
			if (class->Get(&id, r, c, class)) 
			{
				DATA 	key, *record;
				key.id = id;
				record = STfind(table, &key);
				if (record != NULL && record->count != 0)
				 average->Put(
				  record->value.total / record->count,
				   r, c, average);
				else
				 average->PutMV(r, c, average);
			}
			else	/* MV in -> MV out */
				average->PutMV(r, c, average);
		}
	}
	STfree(table);
	return 0;
}

/* Determines total of an area with one class value.
 * Uses a fast list and a slow list. The fast list is already allocated, 
 * on the slow list a binary search has to be done every time.
 * Returns 1 if memory allocation fails, 0 otherwise.
 */
int AreaTotal(
     MAP_REAL8 *total,		/* write-only output total map  */ 
     const MAP_REAL8 *val, 	/* input value map	*/
     const MAP_INT4 *class) 	/* input classes map	*/
{
	int 		r, c, nrRows, nrCols;
	SEARCH_TABLE 	*table;			/* table */

	val->SetGetTest(GET_MV_TEST, val);
	class->SetGetTest(GET_MV_TEST, class);

	nrRows = class->NrRows(class);
	nrCols = class->NrCols(class);

	table = TotalTable((LOCAL_GET_FUNC)GetReal8,val,class);
	if(table == NULL)
		return 1;

	/* scan the map to put the total values in the output map */
	for (r = 0; r < nrRows; r++)
	{
		for (c = 0; c < nrCols; c++)
		{
			INT4 	id;
			if(class->Get(&id, r, c, class)) 
			{
				DATA *record, key;
				key.id = id;
				record = STfind(table, &key);
				if(record != NULL && record->count != 0)
				 total->Put(record->value.total, r, c, total);
				else
				 total->PutMV(r, c, total);
			}
			else	/* MV in -> MV out */
				total->PutMV(r, c, total);
		}
	}
	STfree(table);
	return 0;
}

static BOOL InitRecAllocFailure = FALSE;
static int InitRecFastList = 0;

static void InitRecDivMaj(
	DATA *e,
	int  id)
{
	e->id = id;
	e->count = 0;
	e->value.tab = STnew((size_t)InitRecFastList, sizeof(DATA),
			(RETURN_ID)RetIdArea, 
			(INIT_REC)InitRecAve,  
			       /* as long as the count is set to 0 */
			(QSORT_CMP) CmpStatCont);
	if (e->value.tab == NULL)
		InitRecAllocFailure = TRUE;
}

static void FreeRecDivMaj(
	DATA *e)
{
	if (e->value.tab != NULL)
		STfree(e->value.tab);
}

/* create table with for each a table as record
 * record table cannot be empty; 
 * it's only created if there is a value
 */
static SEARCH_TABLE *MajTable(
     const MAP_INT4 *val, 	/* input value map	*/
     const MAP_INT4 *class) 	/* input classes map	*/
{
	int r,c,nrRows = class->NrRows(class);
	int nrCols = class->NrCols(class);
	SEARCH_TABLE *table;

	InitRecFastList = val->HintNrFastList(val);
	table = STnew((size_t)class->HintNrFastList(class), 
		  sizeof(DATA), (RETURN_ID)RetIdArea,
		  (INIT_REC)InitRecDivMaj, (QSORT_CMP) CmpStatCont);
	if(table == NULL || InitRecAllocFailure)
	{
		STfreeAction(table,(ACTION_REC)FreeRecDivMaj);
		InitRecAllocFailure = FALSE;
		return NULL;
	}

	/* scan the map to determine the number of different values 
	 * for each area.
	 */
	for (r = 0; r < nrRows; r++)
	{
	for (c = 0; c < nrCols; c++)
	{
		 INT4 id,value;
		 if( class->Get(&id, r, c, class) &&
		     val->Get(&value, r, c, val) )
		 {
			DATA *idRec, *valRec, idKey, valKey;

			/* find or insert table for this key */
			idKey.id = id;
			idRec = STfindOrInsert(table, &idKey);
			if(idRec == NULL || InitRecAllocFailure)
			{
			 STfreeAction(table,(ACTION_REC)FreeRecDivMaj);
			 InitRecAllocFailure = FALSE;
			 return NULL;
			}

			 /* add value to this id */
			 valKey.id = value;
			 valRec = STfindOrInsert(idRec->value.tab, &valKey);
			 if(valRec == NULL || InitRecAllocFailure)
			 {
			  STfreeAction(table,(ACTION_REC)FreeRecDivMaj);
			  InitRecAllocFailure = FALSE;
			  return NULL;
			 }
			 /* if first time this value found
			  * then increment count of id 
			  */
			 if (valRec->count++ == 0)
				idRec->count++;
		 }
		 /* ignore MV */
	}}
	return table;
}

int AreaDiversity(
     MAP_REAL8 *diversity,	/* write-only output diversity map  */ 
     const MAP_INT4 *val, 	/* input value map	*/
     const MAP_INT4 *class) 	/* input classes map	*/
{
	int 		r, c, nrRows, nrCols;
	SEARCH_TABLE 	*table;			/* table */

	val->SetGetTest(GET_MV_TEST, val);
	class->SetGetTest(GET_MV_TEST, class);

	if ((table = MajTable(val, class)) == NULL)
		return 1;

	nrRows = class->NrRows(class);
	nrCols = class->NrCols(class);

	/* put the output values and deallocate the sub-tables */
	for (r = 0; r < nrRows; r++)
	{
		for (c = 0; c < nrCols; c++)
		{
			INT4 id;
			if(class->Get(&id, r, c, class))
			{
				DATA *record, key;
				key.id = id;
				record = STfind(table, &key);
				if(record != NULL && record->count != 0)
				 diversity->Put(record->count, r, c, diversity);
				else
				 diversity->PutMV(r, c, diversity);
			}
			else
			{	/* MV in -> MV out */
				diversity->PutMV(r, c, diversity);
			}
		}
	}
	STfreeAction(table,(ACTION_REC)FreeRecDivMaj);
	return 0;
}

static DATA const *FindMaj(
	const DATA *e1,
	const DATA *e2)
{
	if (e1->count == e2->count)
	 /* highest value if maj equal */
		return (e1->id > e2->id ? e1 : e2);
	return (e1->count > e2->count ? e1 : e2);
}

/* find majority in table
 */
static void ForAllMajArea(
	DATA *e)
{
	DATA *majClass = STsearch(e->value.tab,(SEARCH_REC)FindMaj);
	PRECOND(majClass != NULL); /* should contain records */
	e->maj = majClass->id;
}


int AreaMajority(
     MAP_INT4 *majority,	/* write-only output majority map  */ 
     const MAP_INT4 *val, 	/* input value map	*/
     const MAP_INT4 *class) 	/* input classes map	*/
{
	int 		r, c, nrRows, nrCols;
	INT4 		id;		/* value at r, c in class map */
	SEARCH_TABLE 	*table;		/* table */

	val->SetGetTest(GET_MV_TEST, val);
	class->SetGetTest(GET_MV_TEST, class);

	if ((table = MajTable(val, class)) == NULL)
		return 1;

	/* now search for each item it's table to find the majority */
	STforAll(table, (ACTION_REC)ForAllMajArea);

	nrRows = class->NrRows(class);
	nrCols = class->NrCols(class);

	/* scan the map to put the values in the output map */
	for (r = 0; r < nrRows; r++)
	{
		for (c = 0; c < nrCols; c++)
		{
			if(class->Get(&id, r, c, class)) 
			{
				DATA *record, key;
				key.id = id;
				record = STfind(table, &key);
				if (record != NULL && record->count != 0)
				 majority->Put(record->maj, r, c, majority);
				else
				 majority->PutMV(r, c, majority);
			}
			else	/* MV in -> MV out */
				majority->PutMV(r, c, majority);
		}
	}

	STfreeAction(table, (ACTION_REC)FreeRecDivMaj); 
	return 0;
}

/* Determines total area of one class value. (is areaarea)
 * Uses a fast list and a slow list. The fast list is already allocated, 
 * on the slow list a binary search has to be done every time.
 * Returns 1 if memory allocation fails, 0 otherwise.
 */
int AreaCount(
     MAP_REAL8 *out,		/* write-only output area count map  */ 
     const MAP_INT4 *class) 	/* input classes map	*/
{
	int 		r, c, nrRows, nrCols;
	INT4 		id;		/* value at r, c in class map */
	SEARCH_TABLE 	*table;		/* table */
	REAL8 	        area = Area();	/* total area */

	class->SetGetTest(GET_MV_TEST, class);

	nrRows = class->NrRows(class);
	nrCols = class->NrCols(class);

	/* initialize table */
	table = TotalTable((LOCAL_GET_FUNC)GetInt4,class,class);
	if(table == NULL)
		return 1;

	/* this can be optimized by doing the area*record->count
	 * on the table, Probably true for all area-funcs that call
	 * TotalTable
	 */

	/* scan map to determine the output value for each cell. */
	for (r = 0; r < nrRows; r++)
	{
	 	for (c = 0; c < nrCols; c++)
	 	{
			if(class->Get(&id, r, c, class))
			{
				DATA 	*record, key;
				key.id = id;
				record = STfind(table, &key);
				POSTCOND(record != NULL && record->count > 0);
				out->Put(area*record->count, r, c, out);
			}
			else	/* MV in -> MV out */
				out->PutMV(r, c, out);
		}
	}
	STfree(table);
	return 0;
}

/* Adds a timestep to the time-table. create if currTimeStep is 0
 * For each id (column in time-table) the value is determined.
 * Returns NULL in case of an error, pointer to time-table otherwise.
 */
int AddToTssRowREAL8(
	REAL8 *data,		/* write values, starts at col 1 of TIME_TABLE row! */
	size_t nrData,	        /* nr of Cols of data */
	const MAP_INT4 *id,	/* id map */
	const MAP_REAL8 *expr)	/* expression map */
{
	SEARCH_TABLE *table;
	size_t i;
	int nrRows, nrCols;

	/* Initialize settings */
	id->SetGetTest(GET_MV_TEST, id);
	expr->SetGetTest(GET_MV_TEST, expr);
	nrRows = id->NrRows(id);
	nrCols = id->NrCols(id);

	PRECOND(expr->NrRows(expr) == nrRows);
	PRECOND(expr->NrCols(expr) == nrCols);

	/* POSTCOND(t->vs == VS_SCALAR); temporary, no statistics for directional! */
	table = TotalTable((LOCAL_GET_FUNC)GetReal8,expr,id);
	if(table == NULL)
		return 1;

	/* scan id map */
	for(i = 0; i < nrData; i++)
	{
		DATA 	key, *record;
		key.id = i+1;
		record = STfind(table, &key);
		if (record == NULL || record->count == 0)
		 SET_MV_REAL8(data+i);
		else
		 data[i] = record->value.total / record->count;
	}
	STfree(table);
	return 0;
}

/* Adds a timestep to the time-table. create if currTimeStep is 0
 * For each id (column in time-table) the value is determined.
 * Returns 1 in case of an error, 0 otherwise.
 */
int AddToTssRowINT4(
	REAL8 *data,		/* write values, starts at col 1 of TIME_TABLE row! */
	size_t nrData,	        /* nr of Cols of data */
	const MAP_INT4 *id,	/* id map */
	const MAP_INT4 *expr)	/* expression map */
{
	SEARCH_TABLE *table;
	size_t i;
	int nrRows, nrCols;

	/* Initialize settings */
	id->SetGetTest(GET_MV_TEST, id);
	expr->SetGetTest(GET_MV_TEST, expr);
	nrRows = id->NrRows(id);
	nrCols = id->NrCols(id);

	PRECOND(expr->NrRows(expr) == nrRows);
	PRECOND(expr->NrCols(expr) == nrCols);

	if ((table = MajTable(expr, id)) == NULL)
		return 1;
	/* now search for each item it's table to find the majority */
	STforAll(table, (ACTION_REC)ForAllMajArea);


	/* scan id map */
	for(i = 0; i < nrData; i++)
	{
		DATA 	key, *record;
		key.id = i+1;
		record = STfind(table, &key);
		if (record == NULL || record->count == 0)
		 SET_MV_REAL8(data+i);
		else
		 data[i] = record->maj;
	}

	STfreeAction(table, (ACTION_REC)FreeRecDivMaj); 
	return 0;
}
