/* File:   ccfd.h
 * Author: John D. Wilson
 * Date:   August, 2004
 */

#ifndef CCFD_H
#define CCFD_H

#include "solvers.h"

/*
 *  The 7-point cell-centered finite difference
 *  (CCFD) matrix.
 *
 *  The negative of the conductance associated with each face 
 *  results in the off-diagonal terms.  There are column conductances (CC),
 *  row conductances (CR), and vertical (or layer) conductances (CV).
 *
 *  The sum of the conductances plus an HCOF 
 *  make the diagonal.  
 *  
 *  The resulting matrix is symmetric semidefinite.  Specified
 *  heads are described by the IBOUND array resulting in a
 *  positive definite system of equation.  If IBOUND[i]<=0,
 *  then cell i is treated as a specified head.
 *
 *  The off-diagonals and diagonal are stored as vectors
 *  with an i,j,k ordering.  The CCFD matrix is symmetric
 *  so only half of the off-diagonals are stored.
 *
 *  The conductances are either single precision (prec=0) or double
 *  precision.  These arrays are declared as pointer to void.
 *  The pointers are resolved at runtime to either double or float.
 *
 *  Allocation routines return bytes allocated if successful; otherwise,
 *  a negative value is returned.
 *
 *  Evaluation methods all return an integer value (usually 0)
 *  to be compatible with generic operator definition.
 */

typedef struct CCFD_operator
{
  r_data* rdp;       /* Pointer to r_data object */
  void *CC,*CR,*CV;  /* Conductance arrays */
  double *DD;        /* Diagonal */
  int* IBOUND;       /* Indicates specified heads */
  int prec;          /* prec=0 -> single precision; double otherwise */
}CCFD_operator;

int CCFD_allocate(GEN_operator* GEN_ptr, r_data* rdp, int prec);
void CCFD_free(void* A_ptr);

int CCFD_eval(r_vector* p2_ptr, r_vector* p1_ptr, void* A_ptr);
void CCFD_print(FILE* fp, GEN_operator* GEN_ptr);


/* CCFD ILU:
 *
 * In matrix form, ILU0-D factorization is given by
 *
 * (L+D)(I+D^{-1}U)
 *
 * CCFD_ILU_eval computes the inverse using a back solve
 * followed by a forward solve.  
 *
 * p2_ptr and p1_ptr can point to the same vector
 * in the ILU eval method.
 *
 * The diagonal in the ILU0-D factorization is assembled.
 * The parameter w is for modified ILU0-D.  A value of 0.0
 * means unmodified and a value of 1.0 means row-sums are preserved.
 */
typedef struct CCFD_ILU_operator
{
  r_vector pivots;
  CCFD_operator* CCFD_ptr;
}CCFD_ILU_operator;

int CCFD_ILU_allocate(GEN_operator* GEN_ptr, r_data* rdp);
void CCFD_ILU_free(void* A_ptr);
int CCFD_ILU_assemble(GEN_operator* GEN_ILU_ptr, 
                      GEN_operator* GEN_CCFD_ptr, double w);
int CCFD_ILU_eval(r_vector* p2_ptr, r_vector* p1_ptr, void* A_ptr);
int CCFD_ILU_print(FILE* fp, GEN_operator* GEN_ptr);

/* CCFD SGS: Symmetric Gauss-Seidel
 *
 * In matrix form, SGS
 *
 * (L+D)(I+D^{-1}U)
 *
 * CCFD_SGS_eval computes the inverse using a back solve
 * followed by a forward solve.  
 *
 * p2_ptr and p1_ptr can point to the same vector
 * in the SGS eval method.
 *
 */

int CCFD_SGS_allocate(GEN_operator* GEN_ptr);
void CCFD_SGS_free(void* A_ptr);
int CCFD_SGS_assemble(GEN_operator* GEN_SGS_ptr, 
                      GEN_operator* GEN_CCFD_ptr, double w);
int CCFD_SGS_eval(r_vector* p2_ptr, r_vector* p1_ptr, void* A_ptr);

/* 
 * CCFD MG operator:
 *
 * Multilevel CCFD matrix, Smoother, and prolongation/restriction pair.
 *
 * CCFD matrices are allocated on each level except the finest level.
 * The multilevel CCFD matrix on the finest level points to (aliases)
 * a external CCFD matrix.  
 *
 * Restriction is natural embedding and prolongation is transpose
 * of restriction.  
 *
 * Smoother is either ILU or symmetric gauss-seidel (SGS).  
 *
 * The coarse-grid solver is ILU which is exact for 1-D.
 *
 * Coarsening is typically standard coarsening with no limit on the number
 * of levels.  The coarse-grid is always 1-D so that ILU is exact.
 * Semi-coarsening is implemented by setting the number
 * of levels to 1 for either columns, rows, or layers.
 * Semi-coarsening is controlled by the SC variable.
 * The possible values of SC and their meanings follow:
 *
 * --------------------------------
 * |    |        LEVELS           |
 *      ---------------------------
 * | SC | ROWS | COLUMNS | LAYERS |
 * --------------------------------
 * | 0  | MAX  | MAX     | MAX    |
 * | 1  | MAX  | MAX     | 1      |
 * | 2  | 1    | MAX     | MAX    |
 * | 3  | MAX  | 1       | MAX    |
 * | 4  | 1    | 1       | 1      |
 * --------------------------------
 *
 * If SC=4, then CCFD MG is equivalent to ILU preconditioner.
 * In this case, the relaxation parameter 0<= w <= 1 may be used to improve
 * the condition number.
 *
 * On fine-grid levels, the smoother is controlled by SM.  If SM=0,
 * then ILU smoothing is implemented, otherwise SGS smoothing is implemented.
 * ILU smoothing requires storing a vector on each level while SGS requires
 * no additional storage.  ILU is more robust.
 *
 * The CCFD matrices are coarsened using a non-variational coarsening
 * algorithm for cell-centered finite difference matrices.  In matrix
 * form this is 1/2P*A*R.
 */

typedef struct CCFD_MG_operator
{
  /* Multigrid Operators */
  MG_r_data mgrd;         /* Multilevel Data */
  MG_GEN_operator MGCCFD; /* Multilevel CCFD Matrix */
  MG_GEN_operator MGB;    /* Multilevel Smoother */
  MG_GEN_operator MGP;    /* Multilevel Prolongation */
  MG_GEN_operator MGR;    /* Multilevel Restriction */
  GEN_operator MG;        /* Multigrid Operator */
  int SM;                 /* Smoother Flag */
  int SC;                 /* Coarsening Flag */

}CCFD_MG_operator;

int CCFD_MG_allocate(GEN_operator* GEN_ptr, 
                     GEN_operator* GEN_CCFD_ptr,
                     r_data* rdp, int SM, int SC);
		    
void CCFD_MG_free(void* A_ptr);

int CCFD_MG_assemble(GEN_operator* GEN_CCFD_MG_ptr, double w);

/*
 *  CCFD_MG_set
 *  Sets multigrid iteration parameters.
 *    mu0 -- Number of smoothing iterations
 *    mu1 -- Number of multigrid cycles
 *    nu  -- Type of cycle.  nu=1-> V-cycle, nu=2-> W-cycle.
 *
 *    if mu0=0, then mu0=2^(levels-i)-1 where i is the current level.
 */
void CCFD_MG_set(GEN_operator* GEN_CCFD_MG_ptr,
                 int mu0, int mu1, int nu);

int CCFD_MG_eval(r_vector* p2_ptr, r_vector* p1_ptr, void* A_ptr);

#endif
