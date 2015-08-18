#ifndef TABLE__H
#define TABLE__H

#ifdef __cplusplus
extern "C" {
#endif

#include "csf.h"

typedef int (* RETURN_ID)(const void *r);
typedef void (* INIT_REC)(void *r, int id);
typedef void (* ACTION_REC)(void *r);
typedef const void *(* SEARCH_REC)(const void *e1, const void *e2);

typedef struct SEARCH_TABLE {
  void *fastList;         /* array of data nodes */
  size_t nrFastList;      /* size of fast list */
  void *slowList;         /* array of data nodes */
  size_t nrSlowList;      /* number of records */
  size_t recSize;         /* size of data */
  QSORT_CMP cmp;          /* for binary search */
  RETURN_ID ReturnId;     /* returns ID of record */
  INIT_REC  InitRec;
}SEARCH_TABLE;

/* table.c */
extern void STfree(SEARCH_TABLE *table);
extern void STforAll(SEARCH_TABLE *t, ACTION_REC action);
extern void STfreeAction(SEARCH_TABLE *t, ACTION_REC action);
extern void *STsearch(const SEARCH_TABLE *t, SEARCH_REC f);
extern SEARCH_TABLE *STnew(size_t nrFastList, size_t recSize, RETURN_ID ReturnId, INIT_REC InitRec, QSORT_CMP cmp);
extern void *STinsert(SEARCH_TABLE *t, const void *f);
extern void *STfind(const SEARCH_TABLE *t, const void *record);
extern void *STfindOrInsert(SEARCH_TABLE *t, const void *r);


typedef enum LOOK_UP_TEST{
 /* code depends on particalur enum values
  * order is also important in ParseKey, WriteLookupTable and Lookup
  */
 TEST_ONE    =0,  /* single value  EqualTo           */
 TEST_INF_INF=1,  /* infinity      AnythingInterval */
 TEST_GE_INF =2,  /* [l  ,inf>     GreaterEqual      */
 TEST_GT_INF =3,  /* <l  ,inf>     Greater           */
 TEST_INF_LE =4,  /* <inf,h]       LessEqual         */
 TEST_GE_LE  =5,  /* [l  ,h]                         */
 TEST_GT_LE  =6,  /* <l  ,h]                         */
 TEST_INF_LT =7,  /* <inf,h>                         */
 TEST_GE_LT  =8,  /* [l  ,h>                         */
 TEST_GT_LT  =9,  /* <l  ,h>                         */
 /* special values when scanned partially or returned from ParseKey */
 TEST_NOKEY, /* end of file */
 TEST_ERROR
}LOOK_UP_TEST;

#define TEST_NOKEYREAD(t)  ((t) >= TEST_NOKEY)

typedef struct LOOK_UP_KEY {
  double    l, h;
  LOOK_UP_TEST   t;
} LOOK_UP_KEY;

typedef struct LOOK_UP_TABLE{
  LOOK_UP_KEY   **records;  /* pointer to keys
                             * array of array of nrKeys+1 keys
                             * record[nrKeys] contains
                             * output value
                             */
  CSF_VS           *keyVs;   /* vs's keyVs[nrKeys] is outputVs
                              */
  size_t        nrRecords;  /* number of records */
  size_t        nrKeys;     /* number of keys */
  int           searchMethod;   /* 0 for linear search
                                 * 1 for binary search
                                 */
  size_t        nrMatrCols;     /* for matrix only */
}LOOK_UP_TABLE;

#define  SEARCH_LINEAR 0
#define  SEARCH_BINARY 1

/* readltab.c */
extern LOOK_UP_TABLE *ReadLookupTable(FILE *f,
       const CSF_VS *keyVs, size_t nrKeys, CSF_VS outputVs);
/* writltab.c */
extern int WriteLookupTable(const char *fileName, const LOOK_UP_TABLE *t);

/* looktab.c */
extern void FreeLookupTable(LOOK_UP_TABLE *t);
extern int AllocLookupTable(LOOK_UP_TABLE *t);
/* lookup.c */
extern int NrKeysLookupTable(const struct LOOK_UP_TABLE *table);
extern LOOK_UP_KEY *FindLookupKey(const LOOK_UP_TABLE *t, const double *keyValues);
extern int Lookup(double *result, const LOOK_UP_TABLE *table, const double *keyValues);
extern size_t FindCrossKey(
    const LOOK_UP_TABLE *t,   /* lookup table */
    const double *keyValues, /* values to match, no MV's allowed
                              */
    size_t   startHere);        /* index to start from
                              */

extern LOOK_UP_TABLE *MakeNewCrossTable(
  MAP **maps,
  size_t nrMaps,
  size_t nrInt,   /* number of intervals */
  size_t nrSlots); /* number of histo slots, 0 if not histogram stretched */

extern LOOK_UP_TABLE *UpdateCrossTable(
  const char *crossTable,
  MAP **maps,
  size_t nrMaps);

#ifdef __cplusplus
 }
#endif

#endif /* TABLE__H */
