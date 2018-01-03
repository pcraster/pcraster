/* File: PCG.h */
#ifndef SOLVERS_H
#define SOLVERS_H

/* Append underscore to fortran modual name */
#ifdef _UF
  #define resprint   resprint_
#endif
#ifndef _UF
  #define resprint   RESPRINT
#endif
#include "r_vector.h"

/*
 *  Generic PCG:
 *
 *    Iterative method for approximating the solution to Ax=b
 *    where x is an r_vector structure representing the initial guess
 *    and b is an r_vector structure representing the right-hand side.
 *    Upon completion, the x vector contains the approximate solution
 *    computed by the PCG method.
 *
 *    The matrix A and the preconditioner B are passed in
 *    as pointers to generic operators.
 *
 *    Specifications:
 *
 *     IITER      Maximum number of iterations allowed.
 *
 *     RCLOSE     Stopping criterion for PCG.
 *
 *     pf         Print flag: If pf is non-zero, then the
 *                l2-norm of the preconditioned residual,
 *                the residual and the max-norm of the residual
 *                are printed.  The average reduction is also
 *                printed at the end of the iterations.
 *
 *     IOUT       FORTRAN unit number.
 *
 *     ow         If ow is non-zero, then the right-hand side
 *                is overwritten with the residual.  This
 *                saves memory.  For non-zero ow a zero
 *                initial guess is used.
 *
 *  The iteration parameters are set using PCG_set.  For example:
 *
 *    PCG_set(&PCG,50,1.0e-10,1,IOUT);
 *
 *  will set the maximum number of iteration to 50,
 *  the stopping criterion will 1.0e-10 and the reduction
 *  history is printed to IOUT.
 *
 *  The PCG_eval call returns the number of iterations:
 *
 *    iter=PCG_eval(&x,&b,&PCG);
 *
 *  The final l2-norm of the residual squared
 *  is stored in the PCG structure, PCG.BIGR
*/

typedef struct PCG_operator
{
  /* Work vectors. */
  r_vector r,p,z;

  /* Iteration parameters. */
  int IITER;
  double RCLOSE;
  int pf;
  int IOUT;

  /*
    Pointers to generic operators.
  */
  GEN_operator *A_ptr,*B_ptr;

  int ow; /* Overwrite flag. */

  double BIGR; /* Final Residuals */
}PCG_operator;

int PCG_allocate(GEN_operator* PCG_GEN_ptr, int ow, r_data* rdp);
void PCG_free(void* A_ptr);
int PCG_assemble(GEN_operator* PCG_GEN_ptr,
                 GEN_operator* A_GEN_ptr,
                 GEN_operator* B_GEN_ptr);
int PCG_set(GEN_operator* PCG_GEN_ptr, int IITER,
            double RCLOSE, int pf, int IOUT);

int PCG_eval(r_vector* d2_ptr, r_vector* d1_ptr, void* A_ptr);


/*
 *  Generic multigrid algorithm:
 *
 *  MG_r_data contains a list of r_data structures and number of levels.
 *
 *  An mg_vector contains a list of r_vector's.
 *  The r_vectors are allocated for a range of levels
 *  i0,..,i1.
 *
 *  An MG_GEN_operator contains a list of GEN_operator's.
 *  A list of GEN_operator's is used to define the multi-level
 *  smoothers, stiffness matrices, or prolongation/restriction
 *  pairs.
 *
 *  An MG_matrix contains mg_vector's, pointers to MG_GEN_operator's,
 *  and iteration parameters.  The MG_GEN_operators are defined
 *  outside the context of the generic MG operator and depend on the
 *  multigrid method being used shuch as cell-centered finite-difference.
 *
 *  The generic MG structures allow for recursive definitions
 *  such as alternating plane smoothers which are 2-D multigrid
 *  solvers.
 */

typedef struct MG_r_data
{
  r_data* rd_list; /* Array of r_data objects */
  int levels;      /* Number of levels */
}MG_r_data;

int MG_r_data_allocate(MG_r_data* mgdp, int levels);
void MG_r_data_free(MG_r_data* mgdp);

/* Multilevel r_vector */
typedef struct mg_vector
{
  MG_r_data* mgrdp; /* Pointer to MG_r_data object */
  int i0,i1;        /* Levels i0 through i1 */
  r_vector* r_list; /* Array of r_vector objects */
}mg_vector;

int mg_vector_allocate(mg_vector* mgp, int i0, int i1, MG_r_data* mgrdp);
void mg_vector_free(mg_vector* mgp);

/* Multilevel GEN_operator */
typedef struct MG_GEN_operator
{
  MG_r_data* mgrdp;       /* Pointer to MG_r_data object */
  GEN_operator* GEN_list; /* Array of GEN_operator's */
}MG_GEN_operator;

int MG_GEN_allocate(MG_GEN_operator* MG_GEN_ptr, MG_r_data* mgrdp);
void MG_GEN_free(MG_GEN_operator* MG_GEN_ptr);

typedef struct MG_operator
{
  MG_r_data* mgrdp;    /* Pointer to MG_r_data object */

  /* Overwrite Flag */
  int ow; /* ow=0-> allocate multilevel work vector */

  /* Lists of mg vectors */
  mg_vector mgp; /* Multilevel solution vector */
  mg_vector mgb; /* Multilevel right-hand side */
  mg_vector mgr; /* Multilevel residual vector */
  mg_vector mgw; /* Multilevel work vector */

  MG_GEN_operator* A_ptr;   /* Multilevel coefficient matrix */
  MG_GEN_operator* B_ptr;   /* Multilevel Smoother */
  MG_GEN_operator* P_ptr;   /* Multilevel prolongation operator */
  MG_GEN_operator* R_ptr;   /* Multilevel restriction operator */

  int mu0;        /* Number of smoothing iterations. */
  int mu1;        /* Number of multigrid cycles. */
  int nu;         /* V-Cycle(nu=1), W-Cycle(nu=2) */

}MG_operator;

int MG_allocate(GEN_operator* MG_ptr, int ow, MG_r_data* mgrdp);

void MG_free(void* A_ptr);

int MG_assemble(GEN_operator* MG_GEN_ptr,
                MG_GEN_operator* A_ptr,
                MG_GEN_operator* B_ptr,
                MG_GEN_operator* P_ptr,
                MG_GEN_operator* R_ptr);

void MG_set(GEN_operator* MG_ptr, int mu0, int mu1, int nu);

int MG_eval(r_vector* p2_ptr, r_vector* p1_ptr, void* A_ptr);

#endif
