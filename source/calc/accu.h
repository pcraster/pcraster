#ifndef  __ACCU_H__ 
#define  __ACCU_H__ 

/*
   $Log: accu.h,v $
   Revision 1.1.1.1  2000-01-04 21:02:30  cees
   Initial import Cees

   Revision 1.1  1995/04/05 13:28:02  matthuschka
   Initial revision

 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */

/*************/
/* EXTERNALS */
/*************/
typedef REAL8 (*ACCU_FUNC)(REAL8 amount, REAL8 val);

/*******************/
/*   PROTOTYPES    */
/*******************/

/* accu.c */
extern int PerformAccu(
	MAP_REAL8 *state,
	MAP_REAL8 *flux,
	const MAP_UINT1 *ldd,
	const MAP_REAL8 *amount,
	const MAP_REAL8 *value,
	ACCU_FUNC f);

#endif /* __ACCU_H__*/
/* End of File */
