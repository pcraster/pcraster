#ifndef INCLUDED_CALC_STACKCR
#define INCLUDED_CALC_STACKCR

#ifdef __cplusplus
 extern "C" {
#endif

typedef enum STACK_CR { /* STACK CELL REPRESENTATION */
	STACK_CR_1=0,   /* UINT1 */
	STACK_CR_4=1,   /* INT4 */
	STACK_CR_S=2,   /* scalar,directional type (REAL4 or REAL8) */
	STACK_CR_TABLE=3,   /* TABLE */
	STACK_CR_TSS=4     /* TIMESERIES */
} STACK_CR;

#ifdef __cplusplus
 }

# ifndef  INCLUDED_VSENUM
# include "vsenum.h"
# define INCLUDED_VSENUM
# endif

 //! select cellrepresentation based VS with largest type
 STACK_CR stackCellRepr(VS vs);
#endif

#endif
