#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */

/* global header (opt.) and ranpriv's prototypes "" */
#include "api.h" 
#include "api_p.h" 


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

/* (LIBRARY_INTERNAL)
 */
void PutMVPrivate_UINT1(
        UINT1   **matrix,    /* map matrix */
	int    r,           /* row number */
	int    c)           /* column number */
{
	matrix[r][c] = MV_UINT1;
}

/* (LIBRARY_INTERNAL)
 */
void PutMVPrivate_INT4(
        INT4   **matrix,    /* map matrix */
	int    r,           /* row number */
	int    c)           /* column number */
{
	matrix[r][c] = MV_INT4;
}

/* (LIBRARY_INTERNAL)
 */
void PutMVPrivate_REAL4(
        REAL4   **matrix,    /* map matrix */
	int    r,           /* row number */
	int    c)           /* column number */
{
	SET_MV_REAL4( matrix[r]+c);
}
/* (LIBRARY_INTERNAL)
 */
void Put_REAL8_in_REAL4(
        REAL4  **matrix,    /* map matrix */
        const REAL8 *value,
	int    r,          /* row number */
	int    c)          /* column number */
{
	matrix[r][c] = (REAL4)(*value);
}

/* (LIBRARY_INTERNAL)
 */
void Put_REAL8_in_INT4(
        INT4  **matrix,    /* map matrix */
        const REAL8 *value,
	int    r,          /* row number */
	int    c)          /* column number */
{
	matrix[r][c] = (INT4)(*value);
}

/* (LIBRARY_INTERNAL)
 */
void Put_REAL8_in_UINT1(
        UINT1  **matrix,    /* map matrix */
        const REAL8 *value,
	int    r,          /* row number */
	int    c)          /* column number */
{
	matrix[r][c] = (UINT1)(*value);
}

/* (LIBRARY_INTERNAL)
 */
void Put_INT4_in_REAL4(
        REAL4  **matrix,    /* map matrix */
        const INT4 *value,
	int    r,          /* row number */
	int    c)          /* column number */
{
	matrix[r][c] = (REAL4)(*value);
}

/* (LIBRARY_INTERNAL)
 */
void Put_INT4_in_INT4(
        INT4  **matrix,    /* map matrix */
        const INT4 *value,
	int    r,          /* row number */
	int    c)          /* column number */
{
	matrix[r][c] = (INT4)(*value);
}

/* (LIBRARY_INTERNAL)
 */
void Put_INT4_in_UINT1(
        UINT1  **matrix,    /* map matrix */
        const INT4 *value,
	int    r,          /* row number */
	int    c)          /* column number */
{
	matrix[r][c] = (UINT1)(*value);
}

/* (LIBRARY_INTERNAL)
 */
void Put_UINT1_in_REAL4(
        REAL4  **matrix,    /* map matrix */
        const UINT1_T *value,
	int    r,          /* row number */
	int    c)          /* column number */
{
	matrix[r][c] = (REAL4)(*value);
}

/* (LIBRARY_INTERNAL)
 */
void Put_UINT1_in_INT4(
        INT4  **matrix,    /* map matrix */
        const UINT1_T *value,
	int    r,          /* row number */
	int    c)          /* column number */
{
	matrix[r][c] = (INT4)(*value);
}

/* (LIBRARY_INTERNAL)
 */
void Put_UINT1_in_UINT1(
        UINT1  **matrix,    /* map matrix */
        const UINT1_T *value,
	int    r,          /* row number */
	int    c)          /* column number */
{
	matrix[r][c] = (UINT1)(*value);
}

/* (LIBRARY_INTERNAL)
 */
void Get_in_UINT1_to_UINT1(
        UINT1 *value,
        const UINT1   **matrix,    /* map matrix */
	int    r,                 /* row number */
	int    c)                 /* column number */
{
	*value = matrix[r][c];
}

/* (LIBRARY_INTERNAL)
 */
void Get_in_UINT1_to_INT4(
        INT4 *value,
        const UINT1   **matrix,    /* map matrix */
	int    r,                 /* row number */
	int    c)                 /* column number */
{
	*value = matrix[r][c];
	if (*value == MV_UINT1)
		*value = MV_INT4;
}

/* (LIBRARY_INTERNAL)
 */
void Get_in_UINT1_to_REAL8(
        REAL8 *value,
        const UINT1   **matrix,    /* map matrix */
	int    r,                 /* row number */
	int    c)                 /* column number */
{
	if ( matrix[r][c] == MV_UINT1 )
		SET_MV_REAL8(value);
	else
		*value = matrix[r][c];
}

/* (LIBRARY_INTERNAL)
 */
void Get_in_INT4_to_INT4(
        INT4 *value,
        const INT4   **matrix,    /* map matrix */
	int    r,                 /* row number */
	int    c)                 /* column number */
{
	*value = matrix[r][c]; 
}

/* (LIBRARY_INTERNAL)
 */
void Get_in_INT4_to_REAL8(
        REAL8 *value,
        const INT4   **matrix,    /* map matrix */
	int    r,                 /* row number */
	int    c)                 /* column number */
{
	if ( matrix[r][c] == MV_INT4 )
		SET_MV_REAL8(value);
	else
		*value = matrix[r][c];
}

/* (LIBRARY_INTERNAL)
 */
void Get_in_REAL4_to_REAL8(
        REAL8 *value,
        const REAL4   **matrix,    /* map matrix */
	int    r,                 /* row number */
	int    c)                 /* column number */
{
	if ( IS_MV_REAL4(matrix[r]+c) )
		SET_MV_REAL8(value);
	else
		*value = matrix[r][c];
}

