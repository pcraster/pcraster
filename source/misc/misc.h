#ifndef MISC__H
#define MISC__H

#ifdef __cplusplus
 extern "C" {
#endif

#include <stdarg.h>   /* see error.c */
#ifdef CSF_V1
# include "csftyp_1.h" /* see cnvrtnum.c */
#else
# include "csftypes.h" /* see cnvrtnum.c */
#endif


/* error.c */
extern int exitOnError;
extern const char *errorPrefixMsg;
extern void (*errorHandler)(const char *msg);
extern void ErrorNested(const char *fmt, ...);
extern void vfErrorNested(const char *fmt, va_list marker);
extern void vfError(const char *fmt, va_list marker);
extern void Error(const char *fmt, ...);
extern int RetError(int returnValue, const char *fmt, ...);
extern int RetErrorNested(int returnValue, const char *fmt, ...);
extern void Warning(const char *fmt, ...);

/* chkmem.c */
#ifdef DEBUG_DEVELOP
 extern void Free(void *ptr);
# define FreeFuncPtr    Free
 extern void StopLimitMalloc(void);
 extern void StartLimitMalloc(void);
# ifdef LIMITMALLOC
#  ifndef CHKMEM_C
#  	  undef  free
#  	  define free(x)   Free(x)
#	  undef  malloc
#	  define malloc(x) ChkMalloc(x)
#	  undef  realloc
#	  define realloc(p,x) ChkRealloc(p,x)
#	  undef  calloc
#	  define calloc(n,s)  ChkCalloc((n),(s))
#   endif
# endif
#else
# define Free(x)	free(x)
# define FreeFuncPtr    free
#endif

#ifndef __C2MAN__
# define FREE_NULL(x)	Free(x),x=NULL
# define CHK_MALLOC_TYPE(type,nr)   ( (type *)ChkMalloc(sizeof(type)*((size_t)(nr))))
#endif

extern void ChkRegisterNoMoreMemory(void (*f)(void));
extern void ChkRegisterTryReleaseMemory(int (*f)(void));
extern void *ChkMalloc(size_t size);
extern void *ChkCalloc(size_t nnemb, size_t size);
extern void *ChkTmpMalloc(size_t size);
extern void ChkTmpFree(void *v);
extern void *ChkRealloc(void *ptr, size_t size);
extern int ChkReallocFree(void **ptr, size_t size);

/* mallocxd.c */
void **Malloc2d(size_t row, size_t col, size_t size);
void **Realloc2d(void **ptr, size_t newRow, size_t newCol, 
              size_t oldRow, size_t oldCol, size_t size);
void **MallocIndex2d(size_t row, size_t col, size_t size, const void *linArray);
void Free2d(void **array2d, size_t nrRows);
void FreeIndex2d(void **array2d);
void *Linear2d(void **array2d);

/* exstring.c */
int StrEq(const char *s1, const char *s2);
int IsAlphaNumericUnderscore(int c);
int StrCaseEq(const char *s1, const char *s2);
int StrNCaseEq(const char *s1, const char *s2, size_t count);
char *Strncpy0(char *dest, const char *src, size_t count);
char *StrcpyChkTmpMalloc(const char *str);
char *StrcpyChkMalloc(const char *str);
void *MemcpyChkMalloc(const void *str, size_t n);
int NoSpaces(const char *string);
int EmptyString(const char *str);
char *DelSpaces(char *str);
size_t CountChars(const char *str, const char *charSet);
char *DelChars(char *str, const char *charSet);
char *LeftRightTabTrim(char *str);
char *LeftRightTrim(char *str);
int TokenSpaceTrim(char *s);

/* cnvrtnum.c */
extern BOOL CnvrtINT4(INT4 *result, const char *str);
extern BOOL CnvrtUINT1(UINT1 *result, const char *str);
extern BOOL CnvrtREAL8(REAL8 *result, const char *str);
extern BOOL CnvrtREAL4(REAL4 *result, const char *str);
extern BOOL CnvrtDouble(double *result, const char *str);
extern BOOL CnvrtInt(int *result, const char *str);
extern BOOL CnvrtValueMV(REAL8 *vNum, const char *vStr, const char *mvStr, 
                         BOOL number, double mvDbl);

/* qsortcmp.c */
extern int CmpUchar(const unsigned char *e1, const unsigned char *e2);
extern int CmpInt(const int *e1, const int *e2);
extern int CmpFloat(const float *e1, const float *e2);
extern int CmpDouble(const double *e1, const double *e2);

/* aterror.c */
extern int AtError(void (*func)(void));
extern int NoLongerAtError(void (*func)(void));
extern void ExecAtError(void);

/* bitset.c */
extern int NrBitSet(const unsigned char *set, int setSize);
extern int FirstBitSet(const unsigned char *set, int setSize);
extern int LastBitSet(const unsigned char *set, int setSize);
extern int SetBit1(unsigned char *set, int setBit);
extern int SetBit0(unsigned char *set, int setBit);
extern int BitSet(const unsigned char *set, int setBit);
extern int FirstBitSetType(const unsigned char *set, int setByteSize);
extern int NrBitSetType(const unsigned char *set, int setByteSize);

#define FIRSTBITSET_TYPE(set, type) \
	FirstBitSetType((const unsigned char *)(&(set)), sizeof(type))
#define NRBITSET_TYPE(set, type) \
	NrBitSetType((const unsigned char *)(&(set)), sizeof(type))

/* bitset2d.c */
extern unsigned char **NewBitMatrix(size_t nrRows, size_t nrCols);
extern void SetAllBitMatrix(unsigned char **m, int nrRows, int nrCols, int v);
extern int Set1BitMatrix(unsigned char **m, int r, int c);
extern int Set0BitMatrix(unsigned char **m, int r, int c);
extern int BitMatrixSet(const unsigned char **m, int r, int c);

/* simplex.c */
#define LEX_NUMBER      300
#define LEX_FIRST_UNUSED_TOKEN_VALUE 301
#define LEX_ILL_TOKEN   -1
#define LEX_READ_ERROR  -2
#define LEX_TOKEN_TOBIG -3
extern void LexInstall( FILE *fd, const char *specialSymbols);
const char *LexGetTokenValue(void);
long LexGetLineNr(void);
int LexGetToken(void);
void LexUngetToken(void);
int LexSkipLines(int nrLines);
int LexError(int token);


/* filestat.c */
extern int FileStat(const char *fileName);
extern int FileStatValid(const char *fileName);
extern int FileNamesEq(const char *fileName1, const char *fileName2);
extern int FileNameExt(const char *fileName, const char *extension);
extern int FileNameValid(const char *fileName);
extern char *MakeFilePathName(char *buf, const char *dirName, const char *fileName);
extern void SplitFilePathName(const char *fullPathName, char **dirName, char **fileName);
extern char *ReplaceDirPathDelimChar(char *str);

/* fileset.c */
extern int CheckFileSets(const char **conflictFileName, const char **outputFiles, int nrOutputFiles, const char **inputFiles, int nrInputFiles);

/* fileio.c */
extern int FileRead(void *ptr, size_t size, size_t nmemb, FILE *f, BOOL shortItemCountIsError);
extern int FileReadAtPos(void *ptr, size_t size, size_t nmemb, long pos, FILE *f, BOOL shortItemCountIsError);
extern int FileWrite(const void *ptr, size_t size, size_t nmemb, FILE *f);
extern int FileWriteAtPos(const void *ptr, size_t size, size_t nmemb, long pos, FILE *f);
extern FILE *FileOpen(const char *fileName, const char *perm);
extern long FileSize(FILE *f);
extern int FileGetString(char *s, int size, FILE *f);

/* recmem.c */
typedef struct RECMEM_HEAP {
# ifdef DEBUG_DEVELOP
	long   count;       /* number of calls to FreeRecord() vs. NewRecord() 
	                     * < 0 means more calls to FreeRecord then 
	                     *          to NewRecord
	                     *     (not possible since FreeRecord already checks
	                     * > 0 vice versa
	                     */
# endif /* DEBUG_DEVELOP */
	void *freeList; /* start of single linked list, 
	                 * private type: (struct RECMEM_LINK *)
	                 */
	size_t	recSize;
	void	**blocks;	/* array of allocated memory blocks */
	size_t	nrBlocks;	/* number of blocks                 */
	size_t blockSize;	/* number of records in a block     */
	void    *(*Malloc)(size_t size);
				/* function to allocate blocks      */
	void    (*_Free)(void  *ptr);
				/* function to free blocks          */
} RECMEM_HEAP;
extern RECMEM_HEAP *NewRecMemHeap(size_t recSize, size_t blockSize, 
       void *(*mallocFunc)(size_t size), void (*freeFunc)(void *ptr));
extern void *NewRecord(RECMEM_HEAP *r);
extern void FreeRecord(void *m, RECMEM_HEAP *r);
extern void FreeAllRecords(RECMEM_HEAP *r);
#ifdef DEBUG
 extern BOOL VerifyRecPtr(void *m, RECMEM_HEAP *r);
#endif

/* swapbyte.c */
extern void SwapByte2(void *b);
extern void SwapByte4(void *b);
extern void SwapByte8(void *b);

#ifdef WIN32
char *Win32GetLastError(void);
int Win32SetLastError(long errorCode);
#endif

#ifdef __cplusplus
 }
#endif

#endif /* MISC__H */
