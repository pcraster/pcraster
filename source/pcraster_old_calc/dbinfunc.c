#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */

/* global header (opt.,size_t n) and dbinfunc's prototypes "" */
#include <string.h> /* memset, memcpy */
#include "csftypes.h" 
#include "dbinfunc.h" 


/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

#define CMP_SS(type, op) \
 { size_t i; \
   for(i=0; i < n; i++) \
    if ( IS_MV_##type((v1+i)) || IS_MV_##type((v2+i)) ) \
    	r[i] = MV_UINT1; \
    else \
        r[i] = (v1[i] op v2[i]); \
 }

#define CMP_SS_1(type, op) \
 { size_t i; \
   for(i=0; i < n; i++) \
    if (v1[i] != MV_UINT1) \
    { \
     if (v2[i] == MV_UINT1) \
    	v1[i] = MV_UINT1; \
    else \
        v1[i] = (v1[i] op v2[i]); \
    } \
    return 0;\
 }


#define CMP_NS_1(type, op) \
 { size_t i; \
   PRECOND(*v1 != MV_UINT1); \
   for(i=0; i < n; i++) \
    if (v2[i] != MV_UINT1)\
        v2[i] = (v2[i] op *v1); \
    return 0;\
 }

#define CMP_SN(type, op) \
 { size_t i; \
   type  val2; \
   PRECOND(! IS_MV_##type(v2)); \
   val2 = *v2; \
   for(i=0; i < n; i++) \
    if ( IS_MV_##type((v1+i)) ) \
    	r[i] = MV_UINT1; \
    else \
        r[i] = (v1[i] op val2); \
 }

#define CMP_NS(type, op) \
 { size_t i; \
   type  val1; \
   PRECOND(! IS_MV_##type(v1)); \
   val1 = *v1; \
   for(i=0; i < n; i++) \
    if ( IS_MV_##type((v2+i)) ) \
    	r[i] = MV_UINT1; \
    else \
        r[i] = (val1 op v2[i]); \
 }

/* SAME_BIN */
int Do_ne_1_ss(UINT1 *v1, const UINT1 *v2, size_t n)
 CMP_SS_1(UINT1, != )
int Do_ne_1_ns(const UINT1 *v1, UINT1 *v2, size_t n)
 CMP_NS_1(UINT1, != )
int Do_eq_1_ss(UINT1 *v1, const UINT1 *v2, size_t n)
 CMP_SS_1(UINT1, == )
int Do_eq_1_ns(const UINT1 *v1, UINT1 *v2, size_t n)
 CMP_NS_1(UINT1, == )

/* CMP: */
void Do_ne_4_ss(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 CMP_SS(INT4, != )
void Do_ne_4_ns(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 CMP_NS(INT4, != )
void Do_eq_4_ss(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 CMP_SS(INT4, == )
void Do_eq_4_ns(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 CMP_NS(INT4, == )
void Do_gt_4_ss(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 CMP_SS(INT4, > )
void Do_gt_4_ns(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 CMP_NS(INT4, > )
void Do_gt_4_sn(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 CMP_SN(INT4, > )
void Do_ge_4_ss(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 CMP_SS(INT4, >= )
void Do_ge_4_ns(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 CMP_NS(INT4, >= )
void Do_ge_4_sn(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 CMP_SN(INT4, >= )
void Do_lt_4_ss(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 { Do_gt_4_ss(r,v2,v1,n); }
void Do_lt_4_ns(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 { Do_gt_4_sn(r,v2,v1,n); }
void Do_lt_4_sn(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 { Do_gt_4_ns(r,v2,v1,n); }
void Do_le_4_ss(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 { Do_ge_4_ss(r,v2,v1,n); }
void Do_le_4_ns(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 { Do_ge_4_sn(r,v2,v1,n); }
void Do_le_4_sn(UINT1 *r, const INT4 *v1, const INT4 *v2, size_t n)
 { Do_ge_4_ns(r,v2,v1,n); }

void Do_ne_s_ss(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 CMP_SS(REAL4, != )
void Do_ne_s_ns(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 CMP_NS(REAL4, != )
void Do_eq_s_ss(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 CMP_SS(REAL4, == )
void Do_eq_s_ns(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 CMP_NS(REAL4, == )
void Do_gt_s_ss(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 CMP_SS(REAL4, > )
void Do_gt_s_ns(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 CMP_NS(REAL4, > )
void Do_gt_s_sn(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 CMP_SN(REAL4, > )
void Do_ge_s_ss(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 CMP_SS(REAL4, >= )
void Do_ge_s_ns(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 CMP_NS(REAL4, >= )
void Do_ge_s_sn(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 CMP_SN(REAL4, >= )
void Do_lt_s_ss(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 { Do_gt_s_ss(r,v2,v1,n); }
void Do_lt_s_ns(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 { Do_gt_s_sn(r,v2,v1,n); }
void Do_lt_s_sn(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 { Do_gt_s_ns(r,v2,v1,n); }
void Do_le_s_ss(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 { Do_ge_s_ss(r,v2,v1,n); }
void Do_le_s_ns(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 { Do_ge_s_sn(r,v2,v1,n); }
void Do_le_s_sn(UINT1 *r, const REAL4 *v1, const REAL4 *v2, size_t n)
 { Do_ge_s_ns(r,v2,v1,n); }



void Do_if_4_ss(INT4  *r, const UINT1 *test, const INT4  *val, size_t n)
{
	size_t i;
	for(i = 0; i < n; i++)
	  r[i] = (test[i] == 1) /* true and not FALSE or MV */
	        ?  val[i] :  MV_INT4;
}

void Do_if_4_ns(INT4  *r, const UINT1 *test, const INT4  *val, size_t n)
{
	size_t i;
	if ( (*test) == 1)
		memcpy(r,val,sizeof(INT4)*n);
	else
		for(i=0;i < n ; i++)
		 r[i] = MV_INT4;
}
void Do_if_4_sn(INT4  *r, const UINT1 *test, const INT4  *val, size_t n)
{
	size_t i;
	INT4 v = *val;
	PRECOND(v != MV_INT4);
	for(i = 0; i < n; i++)
	  r[i] = (test[i] == 1) /* true and not FALSE or MV */
	        ?  v :  MV_INT4;
}

void Do_if_s_ss(REAL4 *in_r, const UINT1 *test, const REAL4 *in_val, size_t n)
{
	/* process as REAL4 */
  UINT4 *r = (UINT4 *)in_r;
  const UINT4 *val = (const UINT4 *)in_val;
	size_t i;
	for(i = 0; i < n; i++)
	  r[i] = (test[i] == 1) /* true and not FALSE or MV */
	        ?  val[i] :  MV_UINT4;
}

void Do_if_s_ns(REAL4 *r, const UINT1 *test, const REAL4 *val, size_t n)
{
	if ( (*test) == 1)
		memcpy(r,val,sizeof(REAL4)*n);
	else /* MV_UINT1 pattern will do for MV_REAL4 */
		memset(r,MV_UINT1,sizeof(REAL4)*n);
}

void Do_if_s_sn(REAL4 *in_r, const UINT1 *test, const REAL4 *in_val, size_t n)
{
	/* process as REAL4 *, so can copy MV_REAL4 safe */
	size_t i;
  UINT4 *r = (UINT4 *)in_r;
  UINT4 v = *((const UINT4 *)in_val);
	PRECOND(v != MV_UINT4);
	for(i = 0; i < n; i++)
	  r[i] = (test[i] == 1) /* true and not FALSE or MV */
	        ?  v :  MV_UINT4;
}


void Do_if_else_1_nns(UINT1 *r, const UINT1 *test,const  UINT1 *t,const UINT1 *f, size_t n)
{
	PRECOND(*test == 0 || *test == 1);
	if (*test)
	 memset(r,*t,n);
	else
	 memcpy(r,f,n); 
}

void Do_if_else_1_nsn(UINT1 *r, const UINT1 *test,const  UINT1 *t,const UINT1 *f, size_t n)
{
	PRECOND(*test == 0 || *test == 1);
	if (*test)
	 memcpy(r,t,n); 
	else
	 memset(r,*f,n);
}

void Do_if_else_1_nss(UINT1 *r, const UINT1 *test,const  UINT1 *t,const UINT1 *f, size_t n)
{
	PRECOND(*test == 0 || *test == 1);
	memcpy(r,(*test) ? t : f , n); 
}

#define DO_IFELSE_SXX(true,false, type) \
{ \
	size_t i; \
	for(i=0; i < n; i++) \
	{ \
	 PRECOND(test[i] == 0 || test[i] == 1 || test[i] == MV_UINT1); \
	 switch(test[i]) { \
	  case 0        : r[i] = (false); break; \
	  case 1        : r[i] = (true); break; \
	  default       : r[i] = MV_##type; break; \
	 } \
	} \
} 

void Do_if_else_1_sss(UINT1 *r, const UINT1 *test,const  UINT1 *t,const UINT1 *f, size_t n)
 DO_IFELSE_SXX(t[i],f[i], UINT1)
void Do_if_else_1_ssn(UINT1 *r, const UINT1 *test,const  UINT1 *t,const UINT1 *f, size_t n)
 DO_IFELSE_SXX(t[i],f[0], UINT1)
void Do_if_else_1_snn(UINT1 *r, const UINT1 *test,const  UINT1 *t,const UINT1 *f, size_t n)
 DO_IFELSE_SXX(t[0],f[0], UINT1)
void Do_if_else_1_sns(UINT1 *r, const UINT1 *test,const  UINT1 *t,const UINT1 *f, size_t n)
 DO_IFELSE_SXX(t[0],f[i], UINT1)

void Do_if_else_4_nss(INT4 *r, const UINT1 *test,const  INT4 *t,const INT4 *f, size_t n)
{
	PRECOND(*test == 0 || *test == 1);
	memcpy(r,(*test) ? t : f , n *sizeof(INT4)); 
}

void Do_if_else_4_sss(INT4 *r, const UINT1 *test,const  INT4 *t,const INT4 *f, size_t n)
 DO_IFELSE_SXX(t[i],f[i], INT4)
void Do_if_else_4_ssn(INT4 *r, const UINT1 *test,const  INT4 *t,const INT4 *f, size_t n)
 DO_IFELSE_SXX(t[i],f[0], INT4)
void Do_if_else_4_snn(INT4 *r, const UINT1 *test,const  INT4 *t,const INT4 *f, size_t n)
 DO_IFELSE_SXX(t[0],f[0], INT4)
void Do_if_else_4_sns(INT4 *r, const UINT1 *test,const  INT4 *t,const INT4 *f, size_t n)
 DO_IFELSE_SXX(t[0],f[i], INT4)
void Do_if_else_4_nns(INT4 *r, const UINT1 *test,const  INT4 *t,const INT4 *f, size_t n)
{
	size_t i;
	PRECOND(*test == 0 || *test == 1);
	if ( (*test))
		for(i=0;i < n ; i++)
		 r[i] = *t;
	else
		memcpy(r,f,sizeof(INT4)*n);
}

void Do_if_else_4_nsn(INT4 *r, const UINT1 *test,const  INT4 *t,const INT4 *f, size_t n)
{
	size_t i;
	PRECOND(*test == 0 || *test == 1);
	if ( (*test))
		memcpy(r,t,sizeof(INT4)*n);
	else
		for(i=0;i < n ; i++)
		 r[i] = *f;
}
void Do_if_else_s_nss(REAL4 *r, const UINT1 *test,const  REAL4 *t,const REAL4 *f, size_t n)
{
	PRECOND(*test == 0 || *test == 1);
	memcpy(r,(*test) ? t : f , n *sizeof(REAL4)); 
}
void Do_if_else_s_sss(REAL4 *in_r, const UINT1 *test,const  REAL4 *in_t,const REAL4 *in_f, size_t n)
{
 UINT4 *r = (UINT4 *)in_r;
 const UINT4 *t = (const UINT4 *)in_t;
 const UINT4 *f = (const UINT4 *)in_f;
 DO_IFELSE_SXX(t[i],f[i], UINT4)
}

void Do_if_else_s_ssn(REAL4 *in_r, const UINT1 *test,const  REAL4 *in_t,const REAL4 *in_f, size_t n)
{
 UINT4 *r = (UINT4 *)in_r;
 const UINT4 *t = (const UINT4 *)in_t;
 const UINT4 *f = (const UINT4 *)in_f;
 DO_IFELSE_SXX(t[i],f[0], UINT4)
}
void Do_if_else_s_snn(REAL4 *in_r, const UINT1 *test,const  REAL4 *in_t,const REAL4 *in_f, size_t n)
{
 UINT4 *r = (UINT4 *)in_r;
 const UINT4 *t = (const UINT4 *)in_t;
 const UINT4 *f = (const UINT4 *)in_f;
 DO_IFELSE_SXX(t[0],f[0], UINT4)
}
void Do_if_else_s_sns(REAL4 *in_r, const UINT1 *test,const  REAL4 *in_t,const REAL4 *in_f, size_t n)
{
 UINT4 *r = (UINT4 *)in_r;
 const UINT4 *t = (const UINT4 *)in_t;
 const UINT4 *f = (const UINT4 *)in_f;
 DO_IFELSE_SXX(t[0],f[i], UINT4)
}

void Do_if_else_s_nns(REAL4 *in_r, const UINT1 *test,const  REAL4 *in_t,const REAL4 *in_f, size_t n)
{
	size_t i;
 UINT4 *r = (UINT4 *)in_r;
 const UINT4 *t = (const UINT4 *)in_t;
 const UINT4 *f = (const UINT4 *)in_f;
	PRECOND(*test == 0 || *test == 1);
	if ( (*test))
		for(i=0;i < n ; i++)
		 r[i] = *t;
	else
		memcpy(r,f,sizeof(INT4)*n);
}
void Do_if_else_s_nsn(REAL4 *in_r, const UINT1 *test,const  REAL4 *in_t,const REAL4 *in_f, size_t n)
{
	size_t i;
 UINT4 *r = (UINT4 *)in_r;
 const UINT4 *t = (const UINT4 *)in_t;
 const UINT4 *f = (const UINT4 *)in_f;
	PRECOND(*test == 0 || *test == 1);
	if ( (*test))
		memcpy(r,t,sizeof(UINT4)*n);
	else
		for(i=0;i < n ; i++)
		 r[i] = *f;
}

/*
*int Do_if_1_ss(UINT1 *test, const UINT1 *val, size_t n)
*{
*	size_t i;
*	for(i = 0; i < n; i++)
*	  test[i] = (test[i] == 1)  true and not FALSE or MV 
*	        ?  val[i] :  MV_UINT1;
*	return 0;
*}
*
*int Do_if_1_ns(const UINT1 *test, UINT1 *val, size_t n)
*{
*	if ( (*test) != 1)
*		memset(val,MV_UINT1,n);
*	return 0;
*}
*
*int Do_if_1_sn(UINT1 *test, const UINT1 *val, size_t n)
*{
*	size_t i;
*	UINT1 v = *val;
*	PRECOND(v != MV_UINT1);
*	for(i = 0; i < n; i++)
*	  test[i] = (test[i] == 1) true and not FALSE or MV 
*	        ?  v :  MV_UINT1;
*	return 0;
*}
*/


void Do_if_1_ss(UINT1  *r, const UINT1 *test, const UINT1  *val, size_t n)
{
	size_t i;
	for(i = 0; i < n; i++)
	  r[i] = (test[i] == 1) /* true and not FALSE or MV */
	        ?  val[i] :  MV_UINT1;
}

void Do_if_1_ns(UINT1  *r, const UINT1 *test, const UINT1  *val, size_t n)
{
	size_t i;
	if ( (*test) == 1)
		memcpy(r,val,sizeof(UINT1)*n);
	else
		for(i=0;i < n ; i++)
		 r[i] = MV_UINT1;
}

void Do_if_1_sn(UINT1  *r, const UINT1 *test, const UINT1  *val, size_t n)
{
	size_t i;
	UINT1 v = *val;
	PRECOND(v != MV_UINT1);
	for(i = 0; i < n; i++)
	  r[i] = (test[i] == 1) /* true and not FALSE or MV */
	        ?  v :  MV_UINT1;
}
