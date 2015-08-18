
#include "stddefx.h" 

/**************************************************************************/
/*  recipes.c                                                            */ 
/*   Some functions adapted from Numerical Recipes (Press. et al)        */
/*                                                                        */
/*                                                                        */
/**************************************************************************/

/********/
/* USES */
/********/
#include "mathx.h"
#include "misc.h"

/***************/
/* EXTERNALS   */
/***************/


/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 


/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* lower/upper triangulization of matrix
 * Decompose matrix a into lower triangular matrix L and upper   
 * triangular matrix U. 
 * .BR
 * Adapted from NUMERICAL RECIPES 1st edition.                                        
 * returns  0 if succes, not 0 if matrix is singular 
 */
int Ludcmp(
	double **a,   /* Read-write. Input matrix, on output the upper and lower
	               * left triangle 
	               */
 	size_t n,     /* dimension of a */
	size_t *indx, /* Write-only. Integer vector (n elements) needed by Luksb */
	double *d)    /* Write-only.  +1 or -1 needed by invert. */


{
	const double tiny=1.0e-20;
	size_t k,j,imax,i;
	double sum,dum,big;
	double *vv = ChkMalloc(sizeof(double)*n);

	/* real initialization of imax happens in loop 
	 * this is only to tell the compiler to keep his big mouth shut
	 */
	imax=0; 
	*d=1;
	for(i=0;i<n;i++)
	{
		big=0;
		for(j=0;j<n;j++)
			if(fabs(a[i][j])>big)
				big=fabs(a[i][j]);
	    	if(big==0)
	    	{ /* matrix is (effectively) singular */
	      		*d=0;
			Free(vv);
			return(-1);
		}
	    	vv[i]=1/big;
	 }
	for(j=0;j<n;j++)
	{
	    if(j>0)
	    {
	      for (i=0; i < j; i++)
	      {
		sum=a[i][j];
		if(i>0)
		{
		  for(k=0;k<i;k++)
		  	sum -= a[i][k]*a[k][j];
		  a[i][j] = sum;
		}
	      }
	    }
	    big=0;
	    for (i=j; i < n; i++)
	    {
	      sum = a[i][j];
	      if(j>0)
	      {
		for(k=0;k<j;k++)
		  sum -= a[i][k]*a[k][j];
		a[i][j]=sum;
	      }
	      dum=vv[i]*fabs(sum);
	      if( dum>big)
	      {
		big=dum;
		imax = i; /* !! initialization of imax */
	      }
	    }
#ifdef NEVER_DEF_THIS
	    if (big == 0)
	    {      /* imax is not initialized */
	    	  (void)fprintf(stderr,"LUDCMP failed!!!!\n");
		  EXIT(1);
	    }
#endif
	    if (j != imax)
	    {
	      for (k =0; k < n; k++)
	      {
		dum =a[imax][k];
		a[imax][k] = a[j][k];
		a[j][k]=dum;
	      }
	      *d=-*d;
	      vv[imax]=vv[j];
	    }
	    indx[j]=imax;
	    if (j != n-1)
	    {
	      if (a[j][j]==0)
	           a[j][j]=tiny;
	      dum = 1/a[j][j];
	      for(i=j+1;i<n;i++)
		a[i][j] *= dum;
	    }
	  }
	  if (a[n-1][n-1]==0) 
		 a[n-1][n-1]= tiny;
	Free(vv);
	return(0);
} /* Ludcmp */

/* Cholesky decomposition of symmetric positive definite matrix 
 * .BR
 * Adapted from NUMERICAL RECIPES 1st edition.                                        
 * returns 0 if succesfull, -1 if failure due to singularity, 1 if failure 
 * because the matrix is not positive definite.
 */
int Cholesky(
	const double **a,  /* Matrix (n*n elements) to be decomposed */
	double **sqrta,    /* Write-only. lower square root of a */
	size_t      n,     /* dimensions of a and sqrta */
	double eps)        /* accuracy criterion */
{
	double result;
	size_t *index = ChkMalloc(sizeof(size_t)*n);
	size_t i,j,k;

	for(i=0; i< n; i++) /* copy input to output */
		for(j=0; j< n; j++)
			sqrta[i][j] = a[i][j];
	result = Ludcmp(sqrta,n,index,&result);
	Free(index);
	if(result!=0)
		return(-1); /* singular matrix */
	for(i=0; i< n; i++) /* Calculate diagonal */
	{
		if(sqrta[i][i]<=0)
			return(1);
		sqrta[i][i] = sqrt(sqrta[i][i]);
	}
	for (i=1;i<n;i++)
		for (j=0;j<i;j++) /* calculate lower triangle */
			sqrta[i][j] *= sqrta[j][j];

	/* now test the result by taking square of sqrta: */
	for (i=0;i<n;i++)
	for (j=0;j<=i;j++) /* j>i not necessary because of symmetry */
	{
		result = 0;
		for(k=0;k<=j;k++) /* sqrta[j,k]==0 for k>j */
			result += sqrta[i][k]*sqrta[j][k];
		result = fabs(result);
		if ((fabs(a[i][j])>(1E-12+(1.0+eps)*result))||
		  (result>(1E-12+(1.0+eps)*fabs(a[i][j]))))
		  	return(1);
	}
	for(i=0; i < n-1; i++) /* upper triangle is zero */
		for(j=i+1; j < n; j++)
			sqrta[i][j]=0;

#ifdef DEBUG
	{
	double **sqrtaT = NewSqrMatrix(n);
	double **check = NewSqrMatrix(n);

	TransposeSqr(sqrtaT,(const double **)sqrta,n);
	(void)MltSqrMm(check, (const double **)sqrta,(const double **)sqrtaT,n);
	for(i=0;i<n;i++)
	for(j=0;j<n;j++)
		POSTCOND(fabs(check[i][j] - a[i][j]) < eps);
	FreeSqrMatrix(sqrtaT,n);
	FreeSqrMatrix(check,n);
	}
#endif
	return(0); /* success */
} /* Cholesky */

/* Multiply two square matrices */
/* returns the resulting matrix */
double **MltSqrMm(
	double **result, /* Write-only, resulting matrix of A*B */
	const double **A,  /* first operand */
	const double **B,  /* second operand */
	size_t n)      /* dimension of matrices */
{
	size_t i,j,k;

	for(i=0;i<n;i++)
	for(j=0;j<n;j++)
	{
		result[i][j] = 0;
		for(k=0;k<n;k++)
			result[i][j] += A[i][k] * B[k][j];
	}
	return(result);
} /* MltSqrMm */

/* Multiply square matrix with vector */
/* returns the resulting vector */
double *MltSqrMv(
	double *result,    /* Write-only. Resulting vector of A*V */
	const double **A,  /* first operand */
	const double *V,   /* second operand */
	size_t n)          /* dimension of matrix, length of vetor */
{
	size_t i,k;

	for(i=0;i<n;i++)
	{
		result[i] = 0;
		for(k=0;k<n;k++)
			result[i] += A[i][k] * V[k];
	}
	return(result);
} /* MltSqrMv */

/* Transpose a square matrix */
/* returns the transposed matrix */
double **TransposeSqr(
	double **result,     /* Write-only. The transposed matrix of A */
	const double **A,    /* matrix to transpose */
	size_t n)            /* dimension of matrices */
{
	size_t i,j;

	for(i=0;i<n;i++)
	for(j=0;j<n;j++)
		result[i][j] = A[j][i];
	return(result);
} /* TransposeSqr */

/* Allocate a n*n matrix
 * Deallocation must be done by a call to FreeSqrMatrix().
 * returns matrix or NULL if no memory 
 */
double **NewSqrMatrix(
	size_t n) /* dimension of square matrix */
{
	double **m = ChkMalloc(sizeof(double *)*n);
	size_t i;

	if (m != NULL)
	 for(i=0;i<n;i++)
		if ( (m[i] = ChkMalloc(sizeof(double)*n)) == NULL)
	       		return NULL;
	return(m);
} /* NewSqrMatrix */

/* Deallocate a n*n matrix.
 * Matrix must be created with a call to NewSqrMatrix().
 */
void FreeSqrMatrix(
	double **m, /* destroyed. Matrix to deallocate */
	size_t n)  /* dimension of m */
{
	size_t i;
	for(i=0;i<n;i++)
		Free(m[i]);
	Free(m);
} /* FreeSqrMatrix */

#ifdef TEST_THIS_MODULE
#define N 3
#define Dyn2Static(s,d) \
	{ size_t i,j; for (i=0;i<N;i++) for (j=0;j<N;j++) s[i][j] = d[i][j];}


void main(void)
{
	double **a      = NewSqrMatrix(N);
	double **sqrta  = NewSqrMatrix(N);
	double **sqrtaT = NewSqrMatrix(N);
	double **check  = NewSqrMatrix(N);
	double v1[N][N];
	double v2[N][N];
	int r ;

a[0][0] = 1;
a[1][0] = a[0][1] = -0.4; a[1][1] = 1;
a[0][2] = a[2][0] = 0.4; a[1][2] = a[2][1] = -0.4; a[2][2]= 1;

	r          = Cholesky(a, sqrta, N, 0.00001);
	if (r == 0)
		(void)printf("Success\n");
	else {
		if (r < 0)
			(void)printf("Singularity\n");
		else
			(void)printf("Not Positive deifnite\n");
	     }

	 (void)TransposeSqr(sqrtaT,sqrta,N);
	 Dyn2Static(v1,sqrta);
	 Dyn2Static(v2,sqrtaT);
	 (void)MltSqrMm(check, sqrta,sqrtaT,N);
	 Dyn2Static(v1,check);
}
#endif 
/* TEST_THIS_MODULE */
