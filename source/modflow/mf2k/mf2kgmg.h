/* File  : mf2kgmg.h
 * Author: JohnD. Wilson
 * Date  : August, 2004
 */

#ifndef MF2KGMG_H
#define MF2KGMG_H

/* Append underscore to functions called from FORTRAN GMG interface */
#ifdef _UF
#define MF2KGMG_ALLOCATE   mf2kgmg_allocate_
#define MF2KGMG_FREE       mf2kgmg_free_
#define MF2KGMG_ASSEMBLE   mf2kgmg_assemble_
#define MF2KGMG_EVAL       mf2kgmg_eval_
#define MF2KGMG_UPDATE     mf2kgmg_update_
#define MF2KGMG_BIGR       mf2kgmg_bigr_
#define MF2KGMG_BIGH       mf2kgmg_bigh_
#endif

#include "ccfd.h"


/* Static global variables for CCFD problem. */

static r_data rd;               /* Vector Data */
static r_vector r;              /* Residual */
static r_vector z;              /* Head-Change */
static GEN_operator CCFD;       /* Cell-Centered Finite Difference Matrix */
static GEN_operator CCFDMG;     /* Multilevel CCFD Operator */
static GEN_operator PCG;        /* Preconditioned Conjugate Gradient */
static double w;                /* Relaxation Parameter */

/* Pointer to access CCFD operator */
static CCFD_operator *CCFD_ptr; 

/*
 * MODFLOW-2000 C-INTERFACE:
 *
 *  Methods linking the GMG library to MF2K
 *  via the gmg1.f interface.
 *
 *  Arguments are passed in by reference; the default passing mode in FORTRAN.
 *
 *  The fine-grid CCFD operator is allocated and assembled in this
 *  library.  MF2K internal arrays are aliased with the fine-grid
 *  CCFD operator.  
 *
 *  MF2K internal data includes:
 *
 *    NCOL   -- Number of columns.
 *    NROW   -- Number of rows.
 *    NLAY   -- Number of layers.
 *    CC     -- Conductances for columns.
 *    CR     -- Conductances for rows.
 *    CV     -- Conductances for layers.
 *    HCOF   -- Source terms
 *    HNEW   -- Current approximation.
 *    RHS    -- Right-hand side.
 *    HNOFLO -- Value of no-flow condition.
 *    IBOUND -- Boundary flags
 *
 *  Data defined in the GMG FORTRAN interface program includes:
 *
 *    DRCLOSE -- Residaul convergence criterion.
 *    IITER   -- Max PCG iterations.
 *    IOUTGMG -- Print flag.
 *    IOUT    -- Fortran unit number.
 *    IPREC   -- Value of 0 indicates single precision; double otherwise.
 *    ISM     -- Smoother flag; not 0 indicates ILU0-D, SGS otherwise.
 *    ISC     -- Semi-Coarsening Flag (see CCFD_MG_allocate)
 *    RELAX   -- If ISC=4, then RELAX can be used to improve condition number.
 *
 *  Data returned to the GMG FORTRAN interface program includes:
 *
 *    ISIZ  -- Size in MB of data allocted by GMG.
 *    IERR  -- Value less than 0 indicates error.
 *    ITER  -- PCG iterations
 *    BIGR0 -- l2-norm of initial residual
 *    BIGR  -- l2-norm of residual
 *    BIGH  -- Max head change
 */

/* 
 * Allocates GMG data 
 */
void MF2KGMG_ALLOCATE(int* NCOL, int* NROW, int* NLAY,
                      int* IPREC, int* ISM, int* ISC,
                      double* RELAX, int* ISIZ, int* IERR);

/* 
 * Deallocates GMG data; necessary when MF2K is run in batch mode. 
 */
void MF2KGMG_FREE();

/* Assemble GMG data:
 * Arguments that are pointer to void are either singel precision
 * or double precision and are resolved at run time.
 */
void MF2KGMG_ASSEMBLE(double* BIGR0, void* CC, void* CR, void* CV, 
                      void* HCOF, double* HNEW, void* RHS,
                      void* HNOFLO, int* IBOUND, int* IERR);
                     
/* MF2KGMG_EVAL: Computes head change returning l2-norm of residual (BIGR)
 *  and max head change (BIGH).  Absolute value of BIGH is max-norm of 
 *  head-change.
 */
void MF2KGMG_EVAL(int* ITER, double* BIGR, double* DRCLOSE, 
                  int* IITER, int* IOUTGMG, int* IOUT);
/*
 *  MF2KGMG_UPDATE: Adds damped head change to current approximation. 
 */
void MF2KGMG_UPDATE(double* HNEW, double* DDAMP);

/* Calculate l2-norm of residual and return location 
 * of max residual.
 */
void MF2KGMG_BIGR(double* BIGR, int* IBIGR, int* JBIGR, int* KBIGR);

/*
 * Calculate max head change and return location of max head change.
 * Absolute value of max head change is max-norm of head change.
 */
void MF2KGMG_BIGH(double* BIGH, int* IBIGH, int* JBIGH, int* KBIGH);

#endif
