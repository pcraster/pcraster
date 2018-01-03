#include "solvers.h"
#include <limits.h>

/*
 *  Generic PCG:
 *
 *    Starting value for cfac modified: R.L. Naff, 03/25/2010
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
 *     RCLOSE        Stopping criterion for PCG.
 *
 *     pf            Print flag: If pf is non-zero, then the
 *                   l2-norm of the preconditioned residual,
 *                 the residual and the max-norm of the residual
 *                 are printed.  The average reduction is also
 *                 printed at the end of the iterations.
 *
 *     IOUT          Unit number for fortran output.
 *
 *     ow            If ow is non-zero, then the right-hand side
 *                   is overwritten with the residual.  This
 *                 saves memory.  For non-zero ow a zero
 *                 initial guess is used.
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

/* Interface to fortran print function */
void resprint(int* IOUT, int* I, double* RES, double* CFAC);

int PCG_allocate(GEN_operator* GEN_ptr, int ow, r_data* rdp)
{
  int size,total_size=0;
  PCG_operator* PCG_ptr;

  PCG_ptr=(PCG_operator*)calloc(1,sizeof(PCG_operator));
  if(PCG_ptr==NULL)
    return 0;
  GEN_assemble(GEN_ptr,PCG_ptr,PCG_eval,PCG_free);

  if(!ow)
  {
    size=r_allocate(&PCG_ptr->r,rdp);
    if(size==0)
      return 0;
    total_size=size;
  }

  size=r_allocate(&PCG_ptr->z,rdp);
  if(size==0)
    return 0;
  total_size+=size;

  size=r_allocate(&PCG_ptr->p,rdp);
  if(size==0)
    return 0;
  total_size+=size;

  PCG_ptr->IITER=100;
  PCG_ptr->RCLOSE=1.0e-10;
  PCG_ptr->pf=1;
  PCG_ptr->IOUT=0;
  PCG_ptr->ow=ow;

  return total_size;
}

void PCG_free(void* A_ptr)
{
  PCG_operator *PCG_ptr=A_ptr;

  if(!(PCG_ptr->ow))
    r_free(&PCG_ptr->r);
  r_free(&PCG_ptr->p);
  r_free(&PCG_ptr->z);
  free(PCG_ptr);
}

int PCG_assemble(GEN_operator* PCG_GEN_ptr,
                 GEN_operator* A_GEN_ptr,
                 GEN_operator* B_GEN_ptr)
{
  PCG_operator *PCG_ptr=PCG_GEN_ptr->A_ptr;

  PCG_ptr->A_ptr=A_GEN_ptr;
  PCG_ptr->B_ptr=B_GEN_ptr;

  return 1;
}

int PCG_set(GEN_operator* GEN_ptr, int IITER,
            double RCLOSE, int pf, int IOUT)
{
  PCG_operator *PCG_ptr=GEN_ptr->A_ptr;

  PCG_ptr->IITER=IITER;
  PCG_ptr->RCLOSE=RCLOSE;
  PCG_ptr->pf=pf;
  PCG_ptr->IOUT=IOUT;

  return 1;
}

int PCG_eval(r_vector* x_ptr, r_vector* b_ptr, void* A_ptr)
{
  PCG_operator *PCG_ptr=A_ptr;
  r_vector *r_ptr,*z_ptr,*p_ptr;
  double bknum,bkden,bk;
  double akden,ak;
  double RES,BIGR0=0,BIGR;
  int mu1;
  int k;

  double cfac;

  double RCLOSE;
  int IITER,pf;
  int IOUT;

  z_ptr=&PCG_ptr->z;
  p_ptr=&PCG_ptr->p;
  r_ptr=&PCG_ptr->r;

  RCLOSE=PCG_ptr->RCLOSE;
  RCLOSE=RCLOSE*RCLOSE;

  IITER=PCG_ptr->IITER;
  pf=PCG_ptr->pf;
  IOUT=PCG_ptr->IOUT;

  k=0;

  /* Calculate r = b - Ax. */
  if(PCG_ptr->ow)
  {
    r_ptr=b_ptr;
    r_zero(x_ptr);
  }
  else
  {
    GEN_eval(r_ptr,x_ptr,PCG_ptr->A_ptr);
    r1_gets_r2_minus_r1(r_ptr,b_ptr);
  }
  RES=r_dotprd(r_ptr,r_ptr);

  if(pf)
  {
    BIGR0=BIGR=sqrt(RES);
    /* cfac=BIGR/BIGR0; R.L. Naff, 03/25/2010 */
    cfac=1.0;
    resprint(&IOUT,&k,&BIGR,&cfac);
  }

  /* First Iteration */
  if(RES>0.0)
  {
    /* Solve Mz = r and computes bknum=(r,z). */
    mu1=GEN_eval(p_ptr,r_ptr,PCG_ptr->B_ptr);
    bkden=bknum = r_dotprd(r_ptr,p_ptr);

    k++;

    /* Calculate akden = (p, Ap) and ak. */
    GEN_eval(z_ptr,p_ptr,PCG_ptr->A_ptr);
    akden = r_dotprd(p_ptr,z_ptr);
    ak    = bknum/akden;

    /* Update x and r. */
    r1_gets_r1_plus_cr2(x_ptr,p_ptr,ak);
    r1_gets_r1_plus_cr2(r_ptr,z_ptr,-ak);

    /* Compute L2-norm of residual */
    RES=r_dotprd(r_ptr,r_ptr);

    if(pf)
    {
      BIGR=sqrt(RES);
      cfac=BIGR/BIGR0;
      resprint(&IOUT,&k,&BIGR,&cfac);
      BIGR0=BIGR;
    }

    while(RES>RCLOSE && k<IITER)
    {
      /* Solve Mz = r and computes bknum=(r,z). */
      mu1=GEN_eval(z_ptr,r_ptr,PCG_ptr->B_ptr);
      bknum = r_dotprd(r_ptr,z_ptr);

      k++;

      /* Calculate p = z + bk*p. */
      bk=bknum/bkden;
      r1_gets_cr1_plus_r2(p_ptr,z_ptr,bk);
      bkden=bknum;

      /* Calculate akden = (p, Ap) and ak. */
      GEN_eval(z_ptr,p_ptr,PCG_ptr->A_ptr);
      akden = r_dotprd(p_ptr,z_ptr);
      ak    = bknum/akden;

      /* Update x and r. */
      r1_gets_r1_plus_cr2(x_ptr,p_ptr,ak);
      r1_gets_r1_plus_cr2(r_ptr,z_ptr,-ak);

      /* Compute L2-norm of residual */
      RES=r_dotprd(r_ptr,r_ptr);

      if(pf)
      {
        BIGR=sqrt(RES);
        cfac=BIGR/BIGR0;
        resprint(&IOUT,&k,&BIGR,&cfac);
        BIGR0=BIGR;
      }
    }
  }

  PCG_ptr->BIGR=sqrt(RES);

  return k;
}

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

/*
 *  Local Subroutines used by MG_eval method.
 */
int compute_MNUr(int i, MG_operator* MG_ptr);
int MG_Smooth(int i, MG_operator* MG_ptr);

/* Allocate an array of r_data objects. */
int MG_r_data_allocate(MG_r_data* mgrdp, int levels)
{
  mgrdp->levels=levels;

  mgrdp->rd_list=(r_data*)calloc(levels,sizeof(r_data));
  if(mgrdp->rd_list==NULL)
    return -1;

  return levels*sizeof(r_data);
}

void MG_r_data_free(MG_r_data* mgrdp)
{
  free(mgrdp->rd_list);
}

/* Allocate an array of r_vector objects.  An r_vector object
 * is allocated on each level between i0 and i1.
 *
 * Returns number of bytes allocated or a negative value if
 * allocation failed.
 */
int mg_vector_allocate(mg_vector* mgp, int i0, int i1, MG_r_data* mgrdp)
{
  int levels=mgrdp->levels;
  int i;
  int size,total_size=0;

  mgp->mgrdp=mgrdp;
  mgp->i0=i0;
  mgp->i1=i1;

  mgp->r_list=(r_vector*)calloc(levels,sizeof(r_vector));
  if(mgp->r_list==NULL)
    return 0;
  total_size+=levels*sizeof(r_vector);

  for(i=i0;i<=i1;i++)
  {
    size=r_allocate(&mgp->r_list[i],&mgrdp->rd_list[i]);
    if(size<=0)
      return -1;
    total_size+=size;
  }

  return total_size;
}

void mg_vector_free(mg_vector* mgp)
{
  int i,i0,i1;

  i0=mgp->i0;
  i1=mgp->i1;

  for(i=i0;i<=i1;i++)
    r_free(&mgp->r_list[i]);
  free(mgp->r_list);
}

/* Allocates an array of generic operators.
 *
 * Returns number of bytes allocated or a negative value if
 * allocation failed.
 */
int MG_GEN_allocate(MG_GEN_operator* MG_GEN_ptr, MG_r_data* mgrdp)
{
  int levels=mgrdp->levels;
  int size;

  MG_GEN_ptr->mgrdp=mgrdp;

  MG_GEN_ptr->GEN_list=(GEN_operator*)calloc(levels,sizeof(GEN_operator));
  if(MG_GEN_ptr->GEN_list==NULL)
    return -1;
  size=levels*sizeof(GEN_operator);

  return size;
}

void MG_GEN_free(MG_GEN_operator* MG_GEN_ptr)
{
  int i,levels=MG_GEN_ptr->mgrdp->levels;

  for(i=0;i<levels;i++)
    GEN_free(&MG_GEN_ptr->GEN_list[i]);
  free(MG_GEN_ptr->GEN_list);
}

/* Allocates MG_operator multilevel solution vector,
 * multilevel right-hand side vector, multilevel residual vector,
 * and multilevel work vector.
 *
 * The multilevel solution and rhs vectors not allocated on finest level.
 * The multilevel residual vector not allocated on coarsest level.
 *
 * A multilevel work vector is allocated if ow is zero.  The work vector
 * is used in the smoothing procedure to store the preconditioned
 * residual.  The smoother may be able to overwrite the residual with the
 * preconditioned residual in which case ow can be set to non-zero
 * and the multilevel work vector is not allocated.
 * In either case, the multilevel work vector is not allocated on
 * the coarsest level.
 *
 * Returns number of bytes allocated or a negative value if
 * allocation failed.
 */
int MG_allocate(GEN_operator* GEN_ptr, int ow, MG_r_data* mgrdp)
{
  int size,total_size=0;
  int levels=mgrdp->levels;
  MG_operator* MG_ptr;

  MG_ptr=(MG_operator*)calloc(1,sizeof(MG_operator));
  if(MG_ptr==NULL)
    return -1;
  GEN_assemble(GEN_ptr,MG_ptr,MG_eval,MG_free);

  MG_ptr->mgrdp=mgrdp;

  /* Multilevel solution vector.  Not allocated on finest level. */
  size=mg_vector_allocate(&MG_ptr->mgp,0,levels-2,mgrdp);
  if(size<=0)
    return -1;
  total_size+=size;

  /* Multilevel rhs vector.  Not allocated on finest level. */
  size=mg_vector_allocate(&MG_ptr->mgb,0,levels-2,mgrdp);
  if(size<=0)
    return -1;
  total_size+=size;

  /* Multilevel residual vector.  Not allocated on coarsest level. */
  size=mg_vector_allocate(&MG_ptr->mgr,1,levels-1,mgrdp);
  if(size<=0)
    return -1;
  total_size+=size;

  /* Multilevel work */
  if(ow==0)
  {
    size=mg_vector_allocate(&MG_ptr->mgw,1,levels-1,mgrdp);
    if(size<=0)
      return -1;
    total_size+=size;
  }
  else
    /* Alias multilevel work vector with multilevel residual vector. */
    MG_ptr->mgw=MG_ptr->mgr;

  MG_ptr->ow=ow;

  return total_size;
}

void MG_free(void* A_ptr)
{
  MG_operator* MG_ptr=A_ptr;

  mg_vector_free(&MG_ptr->mgp);
  mg_vector_free(&MG_ptr->mgb);
  mg_vector_free(&MG_ptr->mgr);
  if(!MG_ptr->ow)
    mg_vector_free(&MG_ptr->mgw);

  free(MG_ptr);
}

/* Assign pointers to multilevel generic operators
 * assembled outside context of MG operator.
 */
int MG_assemble(GEN_operator* GEN_ptr,
                MG_GEN_operator* A_ptr,
                MG_GEN_operator* B_ptr,
                MG_GEN_operator* P_ptr,
                MG_GEN_operator* R_ptr)
{
  MG_operator* MG_ptr=GEN_ptr->A_ptr;

  MG_ptr->A_ptr=A_ptr;
  MG_ptr->B_ptr=B_ptr;
  MG_ptr->P_ptr=P_ptr;
  MG_ptr->R_ptr=R_ptr;

  return 1;
}

/* Set iteration parameters */
void MG_set(GEN_operator* GEN_ptr, int mu0, int mu1, int nu)
{
  MG_operator *MG_ptr=(MG_operator*)(GEN_ptr->A_ptr);

  /* If mu0=0, then variable mu0 is implemented (see MG_Smooth) */
  MG_ptr->mu0=mu0; /* Number of smoothing iterations */
  MG_ptr->mu1=mu1; /* Number of multigrid cycles */
  MG_ptr->nu=nu;   /* Cycle type: nu=1-> V-Cycle; nu=2-> W-Cycle */
}

/* Set aliases and compute multiple multigrid cycles */
int MG_eval(r_vector* p2_ptr, r_vector* p1_ptr, void* A_ptr)
{
  MG_operator *MG_ptr=A_ptr;
  int levels=MG_ptr->mgrdp->levels;
  int mu1=MG_ptr->mu1;

  r_vector *b_list,*p_list;

  int k;

  b_list=MG_ptr->mgb.r_list;
  p_list=MG_ptr->mgp.r_list;

  /* Alias fine-grid rhs and solution vectors */
  b_list[levels-1]=*p1_ptr;
  p_list[levels-1]=*p2_ptr;

  r_zero(&p_list[levels-1]);

  if(levels==1)
    mu1=1;

  for(k=0;k<mu1;k++)
    compute_MNUr(levels-1,MG_ptr);

  return k;
}

/* Compute recursive multigrid cycle */
int compute_MNUr(int i, MG_operator* MG_ptr)
{
  int levels=MG_ptr->mgrdp->levels;
  int k,nu=MG_ptr->nu;

  r_vector *p_list,*b_list,*r_list,*w_list;
  GEN_operator *A_list,*P_list,*R_list,*B_list;

  A_list=MG_ptr->A_ptr->GEN_list;
  P_list=MG_ptr->P_ptr->GEN_list;
  R_list=MG_ptr->R_ptr->GEN_list;
  B_list=MG_ptr->B_ptr->GEN_list;

  if(i==levels-1)
    nu=1;

  p_list=MG_ptr->mgp.r_list;
  b_list=MG_ptr->mgb.r_list;
  r_list=MG_ptr->mgr.r_list;
  w_list=MG_ptr->mgw.r_list;

  /* Compute approximate solution */
  MG_Smooth(i,MG_ptr);

  for(k=0;i>0 && k<nu;k++)
  {
    /* Restric residual */
    GEN_eval(&r_list[i],&p_list[i],&A_list[i]);
    r1_gets_r2_minus_r1(&r_list[i],&b_list[i]);
    GEN_eval(&b_list[i-1],&r_list[i],&R_list[i]);

    /* Zero initial guess */
    r_zero(&p_list[i-1]);
    compute_MNUr(i-1,MG_ptr);

    /* Add correction */
    GEN_eval(&w_list[i],&p_list[i-1],&P_list[i]);
    r1_gets_r2_plus_r1(&p_list[i],&w_list[i]);

    /* Compute approximate solution */
    MG_Smooth(i,MG_ptr);
  }

  return 1;
}

int MG_Smooth(int i, MG_operator* MG_ptr)
{
  int levels=MG_ptr->mgrdp->levels;
  int mu0=MG_ptr->mu0;
  int k;

  r_vector *p_list,*b_list,*r_list,*w_list;
  GEN_operator *A_list,*B_list;

  p_list=MG_ptr->mgp.r_list;
  b_list=MG_ptr->mgb.r_list;

  A_list=MG_ptr->A_ptr->GEN_list;
  B_list=MG_ptr->B_ptr->GEN_list;

  /* Variable number of smoothing steps */
  if(mu0==0)
    mu0=pow(2,levels-i)-1;

  if(i==0)
  {
    /* Compute coarse-grid solution */
    GEN_eval(&p_list[i],&b_list[i],&B_list[0]);
    return 1;
  }

  r_list=MG_ptr->mgr.r_list;
  w_list=MG_ptr->mgw.r_list;

  for(k=0;k<mu0;k++)
  {
    /* Compute new residual */
    GEN_eval(&r_list[i],&p_list[i],&A_list[i]);
    r1_gets_r2_minus_r1(&r_list[i],&b_list[i]);

    /* Compute new approximation */
    GEN_eval(&w_list[i],&r_list[i],&B_list[i]);
    r1_gets_r2_plus_r1(&p_list[i],&w_list[i]);
  }

  return 0;
}

