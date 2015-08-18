/* File  : mf2kgmg.c
 * Author: JohnD. Wilson
 * Date  : August, 2004
 */

#include "mf2kgmg.h"

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
                      double* RELAX, int* ISIZ, int* IERR)
{
  int size,total_size=0;   /* Number of bytes allocated. */
  int neq;                 /* Number of nodal values. */
  int ow=1;                /* Overwrite flag for PCG enabled. */

  *IERR=0;

  /* Assemble r_data */
  neq=(*NCOL)*(*NROW)*(*NLAY);
  rd.l=*NCOL;
  rd.m=*NROW;
  rd.n=*NLAY;
  rd.neq=neq;

  /* Allocate residual and head-change vectors. */
  size=r_allocate(&r,&rd);
  if(size<0)
  {
    *IERR=-1;
    return;
  }
  total_size+=size;

  size=r_allocate(&z,&rd);
  if(size<0)
  {
    *IERR=-1;
    return;
  }
  total_size+=size;

  /*
   * Allocate fine-grid CCFD operator:
   * The fine-grid CCFD operator is allocated and assembled
   * outside of the normal context of the CCFD operator.
   */
  CCFD_ptr=(CCFD_operator*)calloc(1,sizeof(CCFD_operator));
  if(CCFD_ptr==NULL)
  {
    *IERR=-1;
    return;
  }
  total_size+=sizeof(CCFD_operator);

  /* Note, set deallocation method to NULL. */
  GEN_assemble(&CCFD,CCFD_ptr,CCFD_eval,NULL);

  CCFD_ptr->rdp=&rd;
  CCFD_ptr->prec=*IPREC;

  CCFD_ptr->DD=(double*)calloc(neq,sizeof(double));
  if(CCFD_ptr->DD==NULL)
  {
    *IERR=-1;
    return;
  }
  size=neq*sizeof(double);
  total_size+=size;

  /* Allocate CCFD Multigrid operator */
  size=CCFD_MG_allocate(&CCFDMG,&CCFD,&rd,*ISM,*ISC);
  if(size<0)
  {
    *IERR=-1;
    return;
  }
  total_size+=size;

  /* Allocate PCG solver; enable overwrite */
  size=PCG_allocate(&PCG,ow,&rd);
  if(size<0)
  {
    *IERR=-1;
    return;
  }
  total_size+=size;

  /* Size in megabytes. */
  *ISIZ=(int)(total_size*1.0e-6);

  w=*RELAX;

  return;
}

/*
 *  MF2KGMG_FREE: Deallocates  vectors and operators for cell-centered
 *  finite-diference problem.
 */
void MF2KGMG_FREE()
{

  GEN_free(&CCFDMG);
  GEN_free(&PCG);
  r_free(&r);
  r_free(&z);

  /* Dealocate CCFD outside normal contex */
  free(CCFD_ptr->DD);
  free(CCFD_ptr);

  return;
}

/*
 * Local functions for assembling CCFD matrix.
 */
/* Single precision CCFD assembly method. */
void MF2KGMG_SCCFD_ASSEMBLE(double* BIGR0, float* RHS, float* HCOF,
                            float* HNOFLO, double* HNEW);

/* Double precision CCFD assembly method. */
void MF2KGMG_DCCFD_ASSEMBLE(double* BIGR0, double* RHS, double* HCOF,
                            double* HNOFLO, double* HNEW);

/* Assemble GMG data:
 * Arguments that are pointer to void are either singel precision
 * or double precision and are resolved at run time.
 */
void MF2KGMG_ASSEMBLE(double* BIGR0, void* CR, void* CC, void* CV,
                      void* HCOF, double* HNEW, void* RHS,
                      void* HNOFLO, int* IBOUND, int* IERR)
{
  int size; /* Error Flag */

  int mu0=2,mu1=2,nu=2;  /* Multigrid iteration parameters. */

  *IERR=0;

  /* Alias MF2K internal arrays to fine-grid CCFD operator */
  CCFD_ptr->CC=CC;
  CCFD_ptr->CR=CR;
  CCFD_ptr->CV=CV;
  CCFD_ptr->IBOUND=IBOUND;

  if(CCFD_ptr->prec==0)
    MF2KGMG_SCCFD_ASSEMBLE(BIGR0,RHS,HCOF,HNOFLO,HNEW);
  else
    MF2KGMG_DCCFD_ASSEMBLE(BIGR0,RHS,HCOF,HNOFLO,HNEW);

  size=CCFD_MG_assemble(&CCFDMG,w);
  if(size<0)
  {
    *IERR=-1;
    return;
  }
  CCFD_MG_set(&CCFDMG,mu0,mu1,nu);

  PCG_assemble(&PCG,&CCFD,&CCFDMG);

  return;
}

/*
 *  MF2KGMG_EVAL: Computes head change returning l2-norm of residual (BIGR)
 *  and number of iterations (ITER).
 */
void MF2KGMG_EVAL(int* ITER, double* BIGR, double* DRCLOSE,
                  int* IITER, int* IOUTGMG, int* IOUT)

{
  /* Pointer to access PCG data */
  PCG_operator *PCG_ptr=PCG.A_ptr;

  int pf=0; /* Print Flag */

  if((*IOUTGMG==2)||(*IOUTGMG==4))
    pf=1;

  PCG_set(&PCG,*IITER,*DRCLOSE,pf,*IOUT);

  /* Approximate head change. */
  *ITER=GEN_eval(&z,&r,&PCG);

  *BIGR=PCG_ptr->BIGR;

  return;
}

/*
 *  MF2KGMG_UPDATE: Adds damped head change to current approximation.
 */
void MF2KGMG_UPDATE(double* HNEW, double* DDAMP)
{
  r_vector p;

  p.rdp=&rd;
  p.vec=HNEW;

  r1_gets_r1_plus_cr2(&p,&z,*DDAMP);
}


/* Calculate l2-norm of residual and return location
 * of max residual.
 */
void MF2KGMG_BIGR(double* BIGR, int* IBIGR, int* JBIGR, int* KBIGR)
{
  int l,m,n;
  int lm;
  int i,j,k;
  int e,jl,klm;

  double re,resqr,maxresqr;

  l=rd.l, m=rd.m, n=rd.n;
  lm=l*m;

  *BIGR=0.0;

  maxresqr=0.0;
  *IBIGR=1;
  *JBIGR=1;
  *KBIGR=1;

  for(k=0, klm=0; k<n; k++, klm+=lm)
    for(j=0, jl=0; j<m; j++, jl+=l)
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;
        re=r.vec[e];
        resqr=re*re;
        *BIGR+=resqr;

        if(resqr>maxresqr)
        {
          maxresqr=resqr;
          *IBIGR=i+1;
          *JBIGR=j+1;
          *KBIGR=k+1;
        }
      }

  *BIGR=sqrt(*BIGR);

  return;
}

/*
 * Calculate max of head change and return location of max head change.
 * Absalute value of BIGH is max-norm of head change.
 */
void MF2KGMG_BIGH(double* BIGH, int* IBIGH, int* JBIGH, int* KBIGH)
{
  int l,m,n;
  int lm;
  int i,j,k;
  int e,jl,klm;

  double ze,fabsze;

  l=rd.l, m=rd.m, n=rd.n;
  lm=l*m;

  *BIGH=0.0;
  *IBIGH=1;
  *JBIGH=1;
  *KBIGH=1;

  for(k=0, klm=0; k<n; k++, klm+=lm)
    for(j=0, jl=0; j<m; j++, jl+=l)
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;
        ze=z.vec[e];
        fabsze=fabs(ze);
        if(fabsze>fabs(*BIGH))
        {
          *BIGH=ze;
          *IBIGH=i+1;
          *JBIGH=j+1;
          *KBIGH=k+1;
        }
      }

  return;
}

void MF2KGMG_SCCFD_ASSEMBLE(double* BIGR0, float* RHS, float* HCOF,
                            float* HNOFLO, double* HNEW)
{
  /* Grid extents and loop variables */
  int l,m,n;
  int lm;
  int i,j,k;
  int jl,klm;
  int e;

  double *DD;         /* Diagonal */
  float *CC,*CR,*CV;  /* Conductance arrays */
  int *IBOUND;        /* Specified head flags */

  double *RES;        /* Residual vector */
  double EE,RSUM,COND;

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;
  lm=l*m;

  DD=CCFD_ptr->DD;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;
  IBOUND=CCFD_ptr->IBOUND;


  RES=r.vec;

  /*
   * Assemble diagonal of CCFD matrix
   * and residual applying specified heads
   * one element at a time.
  */

  *BIGR0=0.0;
  for(k=0, klm=0 ;k<n; k++, klm+=lm)
  {
    for(j=0, jl=0 ;j<m; j++, jl+=l)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        if(IBOUND[e]<=0)
        {
          RES[e]=0.0;
          DD[e]=1.0;
        }
        else
        {
          RSUM=-(double)RHS[e];
          EE=0.0;

          if(k>0)
          {
            COND=(double)CV[e-lm];
            EE+=COND;
            RSUM+=COND*HNEW[e-lm];
          }
          if(j>0)
          {
            COND=(double)CC[e-l];
            EE+=COND;
            RSUM+=COND*HNEW[e-l];
          }
          if(i>0)
          {
            COND=(double)CR[e-1];
            EE+=COND;
            RSUM+=COND*HNEW[e-1];
          }
          if(i<l-1)
          {
            COND=(double)CR[e];
            EE+=COND;
            RSUM+=COND*HNEW[e+1];
          }
          if(j<m-1)
          {
            COND=(double)CC[e];
            EE+=COND;
            RSUM+=COND*HNEW[e+l];
          }
          if(k<n-1)
          {
            COND=(double)CV[e];
            EE+=COND;
            RSUM+=COND*HNEW[e+lm];
          }

          if(fabs(EE)<=DBL_MIN)
          {
            IBOUND[e]=0;
            HNEW[e]=(double)*HNOFLO;
            DD[e]=1.0;
            RES[e]=0.0;
          }
          else
          {
            EE=EE-(double)HCOF[e];
            DD[e]=EE;
            RES[e]=RSUM-EE*HNEW[e];
          }
        }
        *BIGR0+=RES[e]*RES[e];
      }
    }
  }

  *BIGR0=sqrt(*BIGR0);

  return;
}

void MF2KGMG_DCCFD_ASSEMBLE(double* BIGR0, double* RHS, double* HCOF,
                            double* HNOFLO, double* HNEW)
{
  /* Grid extents and loop variables */
  int l,m,n;
  int lm;
  int i,j,k;
  int jl,klm;
  int e;

  double *DD;           /* Diagonal */
  double *CC,*CR,*CV;   /* Conductance arrays */
  int *IBOUND;          /* Specified head flags */

  double *RES;          /* Residual vector */
  double EE,RSUM,COND;  /* Work variables */

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;
  lm=l*m;

  DD=CCFD_ptr->DD;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;
  IBOUND=CCFD_ptr->IBOUND;

  RES=r.vec;

  /*
   * Assemble diagonal of CCFD matrix
   * and residual applying specified heads
   * one element at a time.
  */

  *BIGR0=0.0;
  for(k=0, klm=0 ;k<n; k++, klm+=lm)
  {
    for(j=0, jl=0 ;j<m; j++, jl+=l)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        if(IBOUND[e]<=0)
        {
          RES[e]=0.0;
          DD[e]=1.0;
        }
        else
        {
          RSUM=-(double)RHS[e];
          EE=0.0;

          if(k>0)
          {
            COND=(double)CV[e-lm];
            EE+=COND;
            RSUM+=COND*HNEW[e-lm];
          }
          if(j>0)
          {
            COND=(double)CC[e-l];
            EE+=COND;
            RSUM+=COND*HNEW[e-l];
          }
          if(i>0)
          {
            COND=(double)CR[e-1];
            EE+=COND;
            RSUM+=COND*HNEW[e-1];
          }
          if(i<l-1)
          {
            COND=(double)CR[e];
            EE+=COND;
            RSUM+=COND*HNEW[e+1];
          }
          if(j<m-1)
          {
            COND=(double)CC[e];
            EE+=COND;
            RSUM+=COND*HNEW[e+l];
          }
          if(k<n-1)
          {
            COND=(double)CV[e];
            EE+=COND;
            RSUM+=COND*HNEW[e+lm];
          }

          if(fabs(EE)<=DBL_MIN)
          {
            IBOUND[e]=0;
            HNEW[e]=(double)*HNOFLO;
            DD[e]=1.0;
            RES[e]=0.0;
          }
          else
          {
            EE=EE-(double)HCOF[e];
            DD[e]=EE;
            RES[e]=RSUM-EE*HNEW[e];
          }
        }
        *BIGR0+=RES[e]*RES[e];
      }
    }
  }

  *BIGR0=sqrt(*BIGR0);

  return;
}

