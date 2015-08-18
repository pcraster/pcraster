#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "csf.h"
#include "misc.h"
#include "app.h"
#include <string.h> /* memcpy */

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/* additional ones, std are in header file */
#define POS_X   0
#define POS_Y   1
#define POS_V   2
#define POS_TAG 3

#define REC_ALLOC  1024 /* will result 32Kb for XYV data (=4 doubles) */

#define NR_COL_MISMATCH \
     "There are %u columns on line %u, %u required"
/**********************/
/* LOCAL DEFINITIONS  */
/**********************/
/* used in ColError message: 
 */
static const char *colName[3] = {"x","y","value"};

/* bookkeeping for the stuff 
 * used by the static function ReadAllColumnFile
 */
static double **allRecList=NULL;
static size_t     nrAllRecords=0; /* number of records actually filled */
static size_t     nrAllRecList=0; /* number of records allocated */
static size_t     nrColsinAllRec=0; /* nr of collumns in a record */

/******************/
/* IMPLEMENTATION */
/******************/

static void InitAllRecs(
  size_t nrColsReq)
{
 allRecList=NULL;
 nrAllRecords=0;
 nrAllRecList=0;
 nrColsinAllRec=nrColsReq;
}

/* Frees data allocated by AppReadTimeSeriesFile or AppReadColumnFile
 * FreeColumnOrTimeSeriesData is the only way to free the space of
 * data returned through the first argument
 * (recs) of a call to AppReadTimeSeriesFile
 * or AppReadColumnFile
 */
static void FreeColumnOrTimeSeriesData(
  double **dataRecords, /* data returned through the first argument
                         * (recs) of a call to AppReadTimeSeriesFile
                         * or AppReadColumnFile
                         */
  size_t nrDataRecords)  /* the number of records in dataRecords
                         */
{
  size_t i;
  /* Free the chunks allocated
   */
  for(i=0; i < nrDataRecords; i += REC_ALLOC)
    Free(dataRecords[i]);
  /* Free the list itself
   */
  Free(dataRecords);
}

static void FreeAllRecs(void)
{
   FreeColumnOrTimeSeriesData(allRecList,nrAllRecList);
   /* ensure that a second call to
    * FreeAllRecs will do nothing
    */
   InitAllRecs((size_t)0);
}

/* Free data returned from AppReadColumnFile
 */
void AppFreeColumnData(
  REAL8 **columnData, /* destructed. Data returned by
                        * AppReadColumnFile
                        */
  size_t nrRecs)      /* number of records in columnData
                       */
{
 FreeColumnOrTimeSeriesData(columnData, nrRecs);
}

/* Free data returned from AppReadTimeSeriesFile
 */
void AppFreeTimeSeriesData(
  REAL8 **timeSeriesData, /* destructed. Data returned by
                           * AppReadTimeSeriesFile
                           */
  size_t nrRecs)      /* number of records in timeSeriesData
                       */
{
 FreeColumnOrTimeSeriesData(timeSeriesData, nrRecs);
}

static double *NewAllRec(void)
{
  if (nrAllRecords == nrAllRecList)
  {
   double **r;
   double *newChunk;
   size_t i;
   r = (double **)ChkRealloc(allRecList, sizeof(double *)*
       (nrAllRecList+REC_ALLOC));
   newChunk = CHK_MALLOC_TYPE(double,REC_ALLOC*nrColsinAllRec);
   if (r == NULL || newChunk == NULL)
   {     FreeAllRecs();
    return NULL;
   }
   allRecList = r;
   /* index recs of new chunk
    */
   i=nrAllRecList;/* old is start value */ 
   nrAllRecList += REC_ALLOC; /* new is end value */
   for( ; i < nrAllRecList; i++) {
     allRecList[i] = newChunk;
     newChunk += nrColsinAllRec;
   }
  }
  return allRecList[nrAllRecords++];
}

static int ColError(
        size_t   colNameIndex,
        size_t   colNr,
        size_t   nrCols)
{
  if(nrCols <= colNr)
        {
         ErrorNested("%s column '%u' too big, nr. of columns in file is '%u'",
           colName[colNameIndex], colNr+1, nrCols);
         return 1;
        }
        return 0;
}


static int HandleAnErrorDefault(
  REAL8 *retVal,size_t colNr,size_t lineNr,const char *wrong)
{
  ErrorNested("column nr. '%u' on line %u contains an"
                " illegal value: '%s'",colNr,lineNr,wrong);
  SET_MV_REAL8(retVal);/* only for arg not used */
  return 1;
}

static int HandleAnErrorTss(
  REAL8 *retVal,size_t colNr,size_t lineNr,const char *wrong)
{
  if (appHeader == APP_NOHEADER && colNr == 1) {
    SET_MV_REAL8(retVal);
    return 0;
  } else
    return HandleAnErrorDefault(retVal,colNr,lineNr,wrong);
}

static int (*handleAnError)(REAL8 *v,size_t c,size_t l, const char *w)
  = HandleAnErrorDefault;

/* Reads column records from inputFile.
 * AppAllReadColumnFile, reads all columns from an input file.
 * Resulting data is stored in the static allRecList which is set up
 * first by the caller.
 * Errors are printed to ErrorNested, the name of the input file is not 
 * printed on the error stream.
 * Returns 1 in case of error, 0 otherwise.
 */
static int ReadAllColumnFile(
  double ***recs,      /* write-only array of nrRecs ptrs to arrays
                        * of double's. dynamically allocated, but
                        * don't flip indices, free  with 
                        * FreeColumnOrTimeSeriesData.
                        */
  size_t    *nrRecs,   /* size of recs array */
  size_t    *nrCols,   /* nr of cols */
  size_t *nrRecordsRead,     /* write-only */
  size_t *nrMVvalueColumn,   /* write-only */
  size_t *nrMVcoordColumn,   /* write-only */
  BOOL   skipMV,            /* skip MV- records */
  BOOL *geoeas,        /* Geo-eas  Y/N */
  const char *inputFile,/* file to read */
  const char *mv,       /* missing value used */
  int       sepChar,    /* separator character that MAY occur
                         * in addition to white space. YOU have
                         * give a non-space default (ex. , ).
                         */
  const size_t *colNr)  /* columns nrs to copy. If NULL then call
                         * from AppReadTimeSeriesFile otherwise from
                         * AppReadColumnFile (= 3 columns copied
                         * in 4 col structure)
                         */
{
  FILE *f;
  char    sepBuf[2];
  size_t     l,nCols;
  double  mvDbl;
  BOOL    number = CnvrtDouble(&mvDbl ,mv);
  double  *currRecValues; /* the record values for a line parsed */
  double *resultRec = NULL; /* marked NULL if we need a new one */
  sepBuf[0] = (char)sepChar;
  sepBuf[1] = '\0';

  *nrRecordsRead=0;
  *nrMVvalueColumn=0;
  *nrMVcoordColumn=0;

  nCols = AppDetectColumnFile(geoeas, inputFile, sepChar);
  if (nCols == 0)
  {   ErrorNested("Can't determine number of columns");
    return 1;
  }
  if ( (currRecValues = CHK_MALLOC_TYPE(double,nCols)) == NULL)
    return 1;

  /* set up local recordBuffer */
  InitAllRecs(colNr == NULL ? nCols : 4);

  /* open file */
  f = fopen(inputFile, "r");
  if(f == NULL) {
    AppFileOpenError(inputFile);
    goto openError;
  }

  /* initialize the lexical analyzer */
  LexInstall(f, sepBuf);
  if(*geoeas)
  {
    if (LexSkipLines((int)(nCols+2)) != (int)(nCols+2))
    {
     ErrorNested("There are not '%u' lines in the GeoEas header", 
                 nCols+2);
     goto error;
    }
  }

  l = LexGetLineNr()-1; /* current line nr */ 
  while (1) {
   size_t i; /* current line nr */ 
   for(i = 1; i <= nCols; i++)
   {
       const char *v;
       int token = LexGetToken();
       if (token == sepChar) /* skip separator */
         token = LexGetToken();
       v = LexGetTokenValue();
       switch(token) {
         case 0 : /* EOF */
             if (i == 1) /* OK */
               goto wholeFileParsed;
             /* EOF in the middle of a record */
             /* column i not found: */
             ErrorNested(NR_COL_MISMATCH, i-1, l, nCols);
             goto error;
         case LEX_READ_ERROR:
             ErrorNested("General read error");
             goto error;
         case LEX_TOKEN_TOBIG:
             ErrorNested("value '%s' for column %u on line %d is too long",
                 v, i, LexGetLineNr());
             goto error;
         default: /* a column value or empty column */
             if (token == sepChar)
             {
               /* empty column */
              ErrorNested("column %u on line %d is empty",
                 i, LexGetLineNr());
              goto error;
             }
             /* a column value */
             PRECOND(token == LEX_ILL_TOKEN || token == LEX_NUMBER);
             if (i == 1) 
             {  /* first col of new line, check and get a record (r)
                 */
      if (LexGetLineNr() == (int)l)
      {
                   ErrorNested("Too many columns on line %d, %u "
                   "columns required", LexGetLineNr(), nCols);
                   goto error;
                  }
                  l = LexGetLineNr();
             }
             else
             {
         if (LexGetLineNr() != (int)l)
               {
                ErrorNested(NR_COL_MISMATCH, i-1, l, nCols);
                goto error;
               }
             }
              } /* eoswitch */
              /* now we have a token for column i value in v
               * PRECOND(r!= NULL);
               */
        if (! CnvrtValueMV(currRecValues+(i-1), v, mv, number, mvDbl))
        {
    if (handleAnError(currRecValues+(i-1),i,l,v))
      goto error;
        }
  } /* eofor */
  if (resultRec == NULL) {
    /* kept NON-NULL if last one is rejected
     */
    if ( (resultRec = NewAllRec()) == NULL)
      goto error;
  }

  /* copy required columns
  */
  (*nrRecordsRead)++;
  if (colNr == NULL) {
    memcpy(resultRec,currRecValues,sizeof(double)*nCols);
    resultRec = NULL;
  } else {
     BOOL hadMV = FALSE;
     COPY_REAL8(resultRec+POS_X, currRecValues+colNr[POS_X]);
     COPY_REAL8(resultRec+POS_Y, currRecValues+colNr[POS_Y]);
     COPY_REAL8(resultRec+POS_V, currRecValues+colNr[POS_V]);
     if ( IS_MV_REAL8(resultRec+POS_V)) {
       (*nrMVvalueColumn)++;
       hadMV = TRUE;
     }
     if ((IS_MV_REAL8(resultRec+POS_X)||IS_MV_REAL8(resultRec+POS_Y))) {
       (*nrMVcoordColumn)++;
       hadMV = TRUE;
     }
     if (skipMV) {
      if (!hadMV)
               resultRec = NULL;  /* non MV or keep: next record please */
           }
         else
             resultRec = NULL;  /* always next record please */
    } /* eo copy XYV */
  } /* eowhile */
wholeFileParsed:
  if (resultRec != NULL && nrAllRecords > 0) { 
    /* last records read must be skipped */
    nrAllRecords--;
  }
  (void)fclose(f);
  free(currRecValues);
  *nrCols = nCols;
  *nrRecs = nrAllRecords;
  *recs = allRecList;
  /* WE SHOULD resize to actual size here */
  return 0;
error:
  (void)fclose(f);
openError:
  free(currRecValues);
  FreeAllRecs();
  return 1;
}

/* Reads column records from inputFile.
 * AppReadColumnFile, reads 3 columns from an input file, X, Y and the
 * value (V). The columns may be identical. They are read in a fixed structure:
 * an array of double of size 4. X, Y, V are respectively
 * in column 0,1 and 2.
 * Errors are printed to ErrorNested, the name of the input file is not 
 * printed on Error.
 * Returns 1 in case of error, 0 otherwise.
 */
int AppReadColumnFile(
  REAL8  ***recs,      /* write-only array of nrRecs ptrs to arrays
                        * of 4 double's. dynamically allocated, but
                        * don't flip indices, free  with 
                        * AppFreeColumnData.
                        */
  size_t *nrRecs,            /* size of recs array */
  size_t *nrRecordsRead,     /* write-only */
  size_t *nrMVvalueColumn,   /* write-only */
  size_t *nrMVcoordColumn,  /* write-only, note that a record is also counted
                           * for coord mv's if it also has a mv in the value
                           * column 
                           */
  BOOL *geoeas,           /* Geo-eas  Y/N */
  const char *inputFile,  /* file to read */
  const char *mv,         /* missing value used */
  CSF_VS vs,               /* type 2 value scale or
                           * VS_UNDEFINED if any double
                           * should be read in the value column
                           */
  CSF_CR cr,               /* cell representation */
  const size_t *colNr,    /* the column numbers of X,Y and value 
                           * (internal index numbers)
                           */
  int       sepChar,    /* separator character that MAY occur
                         * in addition to white space. YOU have
                         * give a non-space default (ex. , ).
                         */
  BOOL  skipMVrecords)    /* don't read mv records into returned recs 
                           * But they are reported in the return counts
                           */
{
  size_t     rec,nrCols, lineDelta;
        if ( ReadAllColumnFile(recs, nrRecs, &nrCols, 
                 nrRecordsRead,nrMVvalueColumn,nrMVcoordColumn,skipMVrecords,
                                  geoeas, inputFile, mv, sepChar, colNr) )
                 return 1;
  lineDelta = (*geoeas) ? (nrCols+2) : 0;

  if (ColError(POS_X,colNr[POS_X],nrCols) ||
      ColError(POS_Y,colNr[POS_Y],nrCols) ||
      ColError(POS_V,colNr[POS_V],nrCols) )
    goto error;

  for(rec=0; rec < *nrRecs; rec++)
  {
   REAL8 *r = allRecList[rec];
#        ifdef DEBUG
    if (skipMVrecords)
    {
     POSTCOND(! IS_MV_REAL8(r+POS_X));
     POSTCOND(! IS_MV_REAL8(r+POS_Y));
     POSTCOND(! IS_MV_REAL8(r+POS_V));
    }
#        endif
   if (!IS_MV_REAL8(r+POS_V) && AppCheckValNum(r[POS_V], vs, cr))
   {
    ErrorNested("value-column %u on line %u",
                colNr[POS_V], rec+lineDelta+1);
    goto error;
   }
   if (!IS_MV_REAL8(r+POS_V) && vs == VS_DIRECTION && r[POS_V] != -1)
        r[POS_V] = AppInputDirection(r[POS_V]);
  } /* eofor */
  return 0;
error:
  FreeAllRecs();
  return 1;
}

/* Read a timeseries file.
 * Errors are printed to ErrorNested, the name of the input file is not 
 * printed on Error.
 * Returns 1 in case of error, 0 otherwise.
 */
int AppReadTimeSeriesFile(
  REAL8  ***recs,      /* write-only array of nrSteps ptrs to arrays
                        * of nrCols double's. dynamically allocated, but
                        * don't flip indices, free  with 
                        * AppFreeTimeSeriesData.
                        */
  size_t *nrSteps,     /* size of recs array */
  size_t *nrCols,      /* size of 1 record */
  BOOL *geoeas,           /* Geo-eas  Y/N */
  const char *inputFile,  /* file to read */
  const char *mv,         /* missing value used */
  CSF_VS vs,               /* type 2 value scale or
                           * VS_UNDEFINED if some double
                           * should be read in the value column
                           */
  CSF_CR cr,               /* cell representation or
                           * CR_UNDEFINED if default cr of
                           * vs must be read
                           */
  int       sepChar)     /* separator character */
{
  size_t     i,c,lineDelta;
  size_t nrRecordsRead,nrMVvalueColumn,nrMVcoordColumn;

  if (appHeader == APP_NOHEADER)
    handleAnError = HandleAnErrorTss;

        if ( ReadAllColumnFile(recs, nrSteps, nrCols, 
                 &nrRecordsRead,&nrMVvalueColumn,&nrMVcoordColumn, FALSE,
                                  geoeas, inputFile, mv, sepChar,NULL) )
                 return 1;

  handleAnError = HandleAnErrorDefault;

  lineDelta = (*geoeas) ? ((*nrCols)+2) : 0;
  for (i = 0;  i < (*nrSteps); i++)
  {
   REAL8 *r = allRecList[i];
   for(c=0; c < (*nrCols); c++)
   {
          switch(c) {
           case 0: 
       if (IS_MV_REAL8(r+0))
       {
    if (appHeader == APP_NOHEADER)
      r[0] = i+1;
    else {
     ErrorNested("timestep column (column 1) on line %u contains a MV",
       i+lineDelta+1);
           goto error;
    }
       }
            if (r[0] != (i+1))
       {
    ErrorNested("timestep column (column 1) on line %u is not %u but %g",
       i+lineDelta+1, i+1, r[c]);
          goto error;
       }
      break;
     default: 
       if (!IS_MV_REAL8(r+c))
       {
        if (AppCheckValNum(r[c], vs, cr))
        {
    ErrorNested("column %u on line %u", c+1, i+lineDelta+1);
    goto error;
        }
        if (vs == VS_DIRECTION && r[c] != -1)
         r[c] = AppInputDirection(r[c]);
       }
      break;
   }
  } /* eofor col */
       }  /* eofor row */
  return 0;
error:
  FreeAllRecs();
  return 1;
}
