#ifndef API_P__H
#define API_P__H
#line 3 "api_p.h"

extern REAL8 testApiArea     ;
extern REAL8 testApiSide     ;
extern REAL8 testApiDiagonal ;
/* extern size_t   testApiNrRows   ; */
/* extern size_t   testApiNrCols   ; */
extern int   testApiYproj    ;
extern BOOL  testApiInit     ;

/* ranpriv.c */
extern void PutMVPrivate_UINT1(UINT1 **matrix, int r, int c);
extern void PutMVPrivate_INT4(INT4 **matrix, int r, int c);
extern void PutMVPrivate_REAL4(REAL4 **matrix, int r, int c);
extern void Put_REAL8_in_REAL4(REAL4 **matrix, const REAL8 *value, int r, int c);
extern void Put_REAL8_in_INT4(INT4 **matrix, const REAL8 *value, int r, int c);
extern void Put_REAL8_in_UINT1(UINT1 **matrix, const REAL8 *value, int r, int c);
extern void Put_INT4_in_REAL4(REAL4 **matrix, const INT4 *value, int r, int c);
extern void Put_INT4_in_INT4(INT4 **matrix, const INT4 *value, int r, int c);
extern void Put_INT4_in_UINT1(UINT1 **matrix, const INT4 *value, int r, int c);
extern void Put_UINT1_in_REAL4(REAL4 **matrix, const UINT1_T *value, int r, int c);
extern void Put_UINT1_in_INT4(INT4 **matrix, const UINT1_T *value, int r, int c);
extern void Put_UINT1_in_UINT1(UINT1 **matrix, const UINT1_T *value, int r, int c);
extern void Get_in_UINT1_to_UINT1(UINT1 *value, const UINT1 **matrix, int r, int c);
extern void Get_in_UINT1_to_INT4(INT4 *value, const UINT1 **matrix, int r, int c);
extern void Get_in_UINT1_to_REAL8(REAL8 *value, const UINT1 **matrix, int r, int c);
extern void Get_in_INT4_to_INT4(INT4 *value, const INT4 **matrix, int r, int c);
extern void Get_in_INT4_to_REAL8(REAL8 *value, const INT4 **matrix, int r, int c);
extern void Get_in_REAL4_to_REAL8(REAL8 *value, const REAL4 **matrix, int r, int c);

/* ERRORS:  */
#define  Get_in_INT4_to_UINT1  NULL
#define  Get_in_REAL4_to_UINT1 NULL
#define  Get_in_REAL4_to_INT4  NULL

/* mapdim.c */
int TestApiInitTest(const char *funcName);

#else /* API_P__H */
#error api_p__h included twice
#endif
