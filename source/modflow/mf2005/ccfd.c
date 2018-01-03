/* File:   ccfd.c 
 * Author: John D. Wilson
 * Date:   August, 2004
 */

#include "ccfd.h"

/*
 *  The 7-point cell-centered finite difference
 *  (CCFD) matrix.
 *
 *  The negative of the conductance associated with each face 
 *  results in the off-diagonal terms.  There are column conductances (CC),
 *  row conductances (CR), and vertical conductances (CV).
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
 *  Allocation methods return bytes allocated if successful; otherwise
 *  a negative value is returned.
 *
 *  Evaluation methods all return an integer value (usually 0)
 *  to be compatible with generic operator definition.
 *
 *  Many methods have a double precision version (DCCFD) or
 *  single precision (SCCFD) version.
 */

int CCFD_allocate(GEN_operator* GEN_ptr, r_data* rdp, int prec)
{
  int neq,size,total_size=0;
  CCFD_operator* CCFD_ptr;

  CCFD_ptr=(CCFD_operator*)calloc(1,sizeof(CCFD_operator));
  if(CCFD_ptr==NULL)
    return -1;

  size=sizeof(CCFD_operator);
  total_size+=size;

  GEN_assemble(GEN_ptr,CCFD_ptr,CCFD_eval,CCFD_free);

  CCFD_ptr->rdp=rdp;
  neq=rdp->neq;

  CCFD_ptr->DD=(double*)calloc(neq,sizeof(double));
  if(CCFD_ptr->DD==NULL)
    return -1;
  size=neq*sizeof(double);
  total_size+=size;

  if(prec==0)
  {
    CCFD_ptr->CC=calloc(neq,sizeof(float));
    if(CCFD_ptr->CC==NULL)
      return -1;
    CCFD_ptr->CR=calloc(neq,sizeof(float));
    if(CCFD_ptr->CR==NULL)
      return -1;
    CCFD_ptr->CV=calloc(neq,sizeof(float));
    if(CCFD_ptr->CV==NULL)
      return -1;
  
    size=3*neq*sizeof(float);
    total_size+=size;
  }
  else
  {
    CCFD_ptr->CC=calloc(neq,sizeof(double));
    if(CCFD_ptr->CC==NULL)
      return -1;
    CCFD_ptr->CR=calloc(neq,sizeof(double));
    if(CCFD_ptr->CR==NULL)
      return -1;
    CCFD_ptr->CV=calloc(neq,sizeof(double));
    if(CCFD_ptr->CV==NULL)
      return -1;
  
    size=3*neq*sizeof(double);
    total_size+=size;
  }

  CCFD_ptr->IBOUND=(int*)calloc(neq,sizeof(int));
  if(CCFD_ptr->IBOUND==NULL)
    return -1;
  size=neq*sizeof(int);
  total_size+=size;

  CCFD_ptr->prec=prec;

  return total_size;
}

void CCFD_free(void* A_ptr)
{
  CCFD_operator *CCFD_ptr=A_ptr;

  free(CCFD_ptr->CC);
  free(CCFD_ptr->CR);
  free(CCFD_ptr->CV);
  free(CCFD_ptr->DD);
  free(CCFD_ptr->IBOUND);
  free(CCFD_ptr);
}


int SCCFD_zero(CCFD_operator* CCFD_ptr);
int DCCFD_zero(CCFD_operator* CCFD_ptr);

int CCFD_zero(CCFD_operator* CCFD_ptr)
{
  int prec=CCFD_ptr->prec;

  if(prec==0)
    SCCFD_zero(CCFD_ptr);
  else
    DCCFD_zero(CCFD_ptr);

  return 0;
}

int SCCFD_zero(CCFD_operator* CCFD_ptr)
{
  int i,neq;
  double* DD;
  float *CC,*CR,*CV;
  int* IBOUND;

  DD=CCFD_ptr->DD;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  IBOUND=CCFD_ptr->IBOUND;

  neq=CCFD_ptr->rdp->neq;

  for(i=0;i<neq;i++)
  {
    DD[i]=0.0;
    CC[i]=0.0;
    CR[i]=0.0;
    CV[i]=0.0;
    IBOUND[i]=0;
  }

  return 0;
}

int DCCFD_zero(CCFD_operator* CCFD_ptr)
{
  int i,neq;
  double* DD;
  double *CC,*CR,*CV;
  int* IBOUND;

  DD=CCFD_ptr->DD;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  IBOUND=CCFD_ptr->IBOUND;

  neq=CCFD_ptr->rdp->neq;

  for(i=0;i<neq;i++)
  {
    DD[i]=0.0;
    CC[i]=0.0;
    CR[i]=0.0;
    CV[i]=0.0;
    IBOUND[i]=0;
  }

  return 0;
}

int SCCFD_eval(r_vector* p2_ptr, r_vector* p1_ptr, CCFD_operator* CCFD_ptr);
int DCCFD_eval(r_vector* p2_ptr, r_vector* p1_ptr, CCFD_operator* CCFD_ptr);

int CCFD_eval(r_vector* p2_ptr, r_vector* p1_ptr, void* A_ptr)
{
  CCFD_operator *CCFD_ptr=A_ptr;
  int prec=CCFD_ptr->prec;

  if(prec==0)
    SCCFD_eval(p2_ptr,p1_ptr,CCFD_ptr);
  else
    DCCFD_eval(p2_ptr,p1_ptr,CCFD_ptr);

  return 0;
}

int SCCFD_eval(r_vector* p2_ptr, r_vector* p1_ptr, CCFD_operator* CCFD_ptr)
{
  int l,m,n;
  int lm;
  int i,j,k;
  int jl,klm;
  int e;

  int* IBOUND;
  double *DD;
  float *CC,*CR,*CV;
  double *p1,*p2;
  double sum;

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;

  lm=l*m;

  IBOUND=CCFD_ptr->IBOUND;
  DD=CCFD_ptr->DD;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  p1=p1_ptr->vec;
  p2=p2_ptr->vec;

  klm=0;
  for(k=0;k<n;k++)
  {
    jl=0;
    for(j=0;j<m;j++)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(k>0)
            if(IBOUND[e-lm]>0)
              sum-=CV[e-lm]*p1[e-lm];
          if(j>0)
            if(IBOUND[e-l]>0)
              sum-=CC[e-l]*p1[e-l];
          if(i>0)
            if(IBOUND[e-1]>0)
              sum-=CR[e-1]*p1[e-1];
          if(i<l-1)
            if(IBOUND[e+1]>0)
              sum-=CR[e]*p1[e+1];
          if(j<m-1)
            if(IBOUND[e+l]>0)
              sum-=CC[e]*p1[e+l];
          if(k<n-1)
            if(IBOUND[e+lm]>0)
              sum-=CV[e]*p1[e+lm];
        }
        p2[e]=DD[e]*p1[e]+sum;
      }
      jl+=l;
    }
    klm+=lm;
  }
  return 0;
}

int DCCFD_eval(r_vector* p2_ptr, r_vector* p1_ptr, CCFD_operator* CCFD_ptr)
{
  int l,m,n;
  int lm;
  int i,j,k;
  int jl,klm;
  int e;

  int* IBOUND;
  double *DD;
  double *CC,*CR,*CV;
  double *p1,*p2;
  double sum;

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;

  lm=l*m;

  IBOUND=CCFD_ptr->IBOUND;
  DD=CCFD_ptr->DD;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  p1=p1_ptr->vec;
  p2=p2_ptr->vec;

  klm=0;
  for(k=0;k<n;k++)
  {
    jl=0;
    for(j=0;j<m;j++)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(k>0)
            if(IBOUND[e-lm]>0)
              sum-=CV[e-lm]*p1[e-lm];
          if(j>0)
            if(IBOUND[e-l]>0)
              sum-=CC[e-l]*p1[e-l];
          if(i>0)
            if(IBOUND[e-1]>0)
              sum-=CR[e-1]*p1[e-1];
          if(i<l-1)
            if(IBOUND[e+1]>0)
              sum-=CR[e]*p1[e+1];
          if(j<m-1)
            if(IBOUND[e+l]>0)
              sum-=CC[e]*p1[e+l];
          if(k<n-1)
            if(IBOUND[e+lm]>0)
              sum-=CV[e]*p1[e+lm];
        }
        p2[e]=DD[e]*p1[e]+sum;
      }
      jl+=l;
    }
    klm+=lm;
  }
  return 0;
}

void SCCFD_print(FILE* fp, CCFD_operator* CCFD_ptr);
void DCCFD_print(FILE* fp, CCFD_operator* CCFD_ptr);

void CCFD_print(FILE* fp, GEN_operator* GEN_ptr)
{
  CCFD_operator *CCFD_ptr=GEN_ptr->A_ptr; 
  int prec=CCFD_ptr->prec;

  if(prec==0)
    SCCFD_print(fp,CCFD_ptr);
  else
    DCCFD_print(fp,CCFD_ptr);
}

void SCCFD_print(FILE* fp, CCFD_operator* CCFD_ptr)
{
  int l,m,n;
  int lm;
  int i,j,k;
  int e,jl,klm;

  int* IBOUND;
  double *DD;
  float *CC,*CR,*CV;

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;

  IBOUND=CCFD_ptr->IBOUND;
  DD=CCFD_ptr->DD;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  lm=l*m;
  klm=0;
  for(k=0;k<n;k++)
  {
    jl=0;
    for(j=0;j<m;j++)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        if(k>0)
        {
          if(IBOUND[e-lm]>0)
            printf("%8d %8d %13.6e\n",e,e-lm,-CV[e-lm]);
          else
            printf("%8d %8d %13.6e\n",e,e-lm,0.0);
        }
        if(j>0)
        {
          if(IBOUND[e-l]>0)
            printf("%8d %8d %13.6e\n",e,e-l,-CC[e-l]);
          else
            printf("%8d %8d %13.6e\n",e,e-l,0.0);
        }
        if(i>0)
        {
          if(IBOUND[e-1]>0)
            printf("%8d %8d %13.6e\n",e,e-1,-CR[e-1]);
          else
            printf("%8d %8d %13.6e\n",e,e-1,0.0);
        }
        if(IBOUND[e]>0)
          printf("%8d %8d %13.6e\n",e,e,DD[e]);
        else
          printf("%8d %8d %13.6e\n",e,e,0.0);
        if(i<l-1)
        {
          if(IBOUND[e+1]>0)
            printf("%8d %8d %13.6e\n",e,e+1,-CR[e]);
          else
            printf("%8d %8d %13.6e\n",e,e+1,0.0);
        }
        if(j<m-1)
        {
          if(IBOUND[e+l]>0)
            printf("%8d %8d %13.6e\n",e,e+l,-CC[e]);
          else
            printf("%8d %8d %13.6e\n",e,e+l,0.0);
        }
        if(k<n-1)
        {
          if(IBOUND[e+lm]>0)
            printf("%8d %8d %13.6e\n",e,e+lm,-CV[e]);
        else
            printf("%8d %8d %13.6e\n",e,e+lm,0.0);
        }
      }
      jl+=l;
    }
    klm+=lm;
  }
}

void DCCFD_print(FILE* fp, CCFD_operator* CCFD_ptr)
{
  int l,m,n;
  int lm;
  int i,j,k;
  int e,jl,klm;

  int* IBOUND;
  double *DD;
  double *CC,*CR,*CV;

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;

  IBOUND=CCFD_ptr->IBOUND;
  DD=CCFD_ptr->DD;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  lm=l*m;
  klm=0;
  for(k=0;k<n;k++)
  {
    jl=0;
    for(j=0;j<m;j++)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        if(k>0)
        {
          if(IBOUND[e-lm]>0)
            printf("%8d %8d %13.6e\n",e,e-lm,-CV[e-lm]);
          else
            printf("%8d %8d %13.6e\n",e,e-lm,0.0);
        }
        if(j>0)
        {
          if(IBOUND[e-l]>0)
            printf("%8d %8d %13.6e\n",e,e-l,-CC[e-l]);
          else
            printf("%8d %8d %13.6e\n",e,e-l,0.0);
        }
        if(i>0)
        {
          if(IBOUND[e-1]>0)
            printf("%8d %8d %13.6e\n",e,e-1,-CR[e-1]);
          else
            printf("%8d %8d %13.6e\n",e,e-1,0.0);
        }
        if(IBOUND[e]>0)
          printf("%8d %8d %13.6e\n",e,e,DD[e]);
        else
          printf("%8d %8d %13.6e\n",e,e,0.0);
        if(i<l-1)
        {
          if(IBOUND[e+1]>0)
            printf("%8d %8d %13.6e\n",e,e+1,-CR[e]);
          else
            printf("%8d %8d %13.6e\n",e,e+1,0.0);
        }
        if(j<m-1)
        {
          if(IBOUND[e+l]>0)
            printf("%8d %8d %13.6e\n",e,e+l,-CC[e]);
          else
            printf("%8d %8d %13.6e\n",e,e+l,0.0);
        }
        if(k<n-1)
        {
          if(IBOUND[e+lm]>0)
            printf("%8d %8d %13.6e\n",e,e+lm,-CV[e]);
        else
            printf("%8d %8d %13.6e\n",e,e+lm,0.0);
        }
      }
      jl+=l;
    }
    klm+=lm;
  }
}

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

/* Allocate ILU structure and diagonal */
int CCFD_ILU_allocate(GEN_operator* GEN_ptr, r_data* rdp)
{
  int size,total_size=0;
  CCFD_ILU_operator* ILU_ptr;

  ILU_ptr=calloc(1,sizeof(CCFD_ILU_operator));
  if(ILU_ptr==NULL)
    return -1;
  size=sizeof(CCFD_ILU_operator);
  total_size+=size;

  GEN_assemble(GEN_ptr,ILU_ptr,CCFD_ILU_eval,CCFD_ILU_free);

  size=r_allocate(&ILU_ptr->pivots,rdp);
  if(size<0)
    return -1;
  total_size+=size;

  return total_size;
}

void CCFD_ILU_free(void* A_ptr)
{
  CCFD_ILU_operator *ILU_ptr=A_ptr;

  r_free(&ILU_ptr->pivots);
  free(ILU_ptr);
}

int SCCFD_ILU_assemble(CCFD_ILU_operator* ILU_ptr, 
                       CCFD_operator* CCFD_ptr, double w);
int DCCFD_ILU_assemble(CCFD_ILU_operator* ILU_ptr, 
                       CCFD_operator* CCFD_ptr, double w);

/*
 * Assembles the diagonal in the ILU0-D factorization.
 * The parameter w is for modified ILU0-D.  A value of 0.0
 * means unmodified and a value of 1.0 means row-sums are preserved.
 */
int CCFD_ILU_assemble(GEN_operator* ILU_GEN_ptr, 
                      GEN_operator* CCFD_GEN_ptr, double w)
                      
{
  CCFD_ILU_operator *ILU_ptr=ILU_GEN_ptr->A_ptr;
  CCFD_operator *CCFD_ptr=CCFD_GEN_ptr->A_ptr;
  int prec=CCFD_ptr->prec;
  int size;

  if(prec==0)
    size=SCCFD_ILU_assemble(ILU_ptr,CCFD_ptr,w);
  else
    size=DCCFD_ILU_assemble(ILU_ptr,CCFD_ptr,w);

  ILU_ptr->CCFD_ptr=CCFD_ptr;

  return size;
}

int SCCFD_ILU_assemble(CCFD_ILU_operator* ILU_ptr, 
                       CCFD_operator* CCFD_ptr, double w)
{
  int l,m,n;
  int lm;
  int i,j,k;
  int e,jl,klm;
  double Cx,Cy,Cz,CD,sum;

  int* IBOUND;
  double *pivots;
  double *DD;
  float *CC,*CR,*CV;

  double neq;

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;

  lm=l*m;

  IBOUND=CCFD_ptr->IBOUND;
  DD=CCFD_ptr->DD;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  pivots=ILU_ptr->pivots.vec;
  neq=CCFD_ptr->rdp->neq;

  for(k=0, klm=0 ; k<n ; k++, klm+=lm)
  {
    for(j=0, jl=0 ; j<m ; j++, jl+=l)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(i>0)
            if(IBOUND[e-1]>0)
            {
              CD=pivots[e-1];
              Cx=CR[e-1];
              sum+=Cx*Cx/CD;

              /* Add fill-in to diagonal */
              if(w>0.0)
              {
                if((j<m-1))
                  if(IBOUND[e-1+l]>0)
                  {
                    Cy=CC[e-1];
                    sum+=w*Cx*Cy/CD;
                  }
                if((k<n-1))
                  if(IBOUND[e-1+lm]>0)
                  {
                    Cz=CV[e-1];
                    sum+=w*Cx*Cz/CD;
                  }
              }
            }
          if(j>0)
            if(IBOUND[e-l]>0)
            {
              CD=pivots[e-l];
              Cy=CC[e-l];
              sum+=Cy*Cy/CD;

              /* Add fill-in to diagonal */
              if(w>0.0)
              {
                if((k<n-1))
                  if(IBOUND[e-l+lm]>0)
                  {
                    Cz=CV[e-l];
                    sum+=w*Cy*Cz/CD;
                  }
                if((i<l-1))
                  if(IBOUND[e-l+1]>0)
                  {
                    Cx=CR[e-l];
                    sum+=w*Cy*Cx/CD;
                  }
              }
            }
          if(k>0)
            if(IBOUND[e-lm]>0)
            {
              CD=pivots[e-lm];
              Cz=CV[e-lm];
              sum+=Cz*Cz/CD;

              /* Add fill-in to diagonal */
              if(w>0.0)
              {
                if((i<l-1))
                  if(IBOUND[e-lm+1]>0)
                  {
                    Cx=CR[e-lm];
                    sum+=w*Cz*Cx/CD;
                  }
                if((j<m-1))
                  if(IBOUND[e-lm+l]>0)
                  {
                    Cy=CC[e-lm];
                    sum+=w*Cz*Cy/CD;
                  }
              }
            }
        }
        pivots[e]=DD[e]-sum;
        if(pivots[e]<=0.0)
          return -1;
      }
    }
  }

  return 0;
}

int DCCFD_ILU_assemble(CCFD_ILU_operator* ILU_ptr, 
                       CCFD_operator* CCFD_ptr, double w)
{
  int l,m,n;
  int lm;
  int i,j,k;
  int e,jl,klm;
  double Cx,Cy,Cz,CD,sum;

  int* IBOUND;
  double *pivots;
  double *DD;
  double *CC,*CR,*CV; 

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;

  lm=l*m;

  IBOUND=CCFD_ptr->IBOUND;
  DD=CCFD_ptr->DD;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  pivots=ILU_ptr->pivots.vec;

  for(k=0, klm=0 ; k<n ; k++, klm+=lm)
  {
    for(j=0, jl=0 ; j<m ; j++, jl+=l)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(i>0)
            if(IBOUND[e-1]>0)
            {
              CD=pivots[e-1];
              Cx=CR[e-1];
              sum+=Cx*Cx/CD;

              /* Add fill-in to diagonal */
              if(w>0.0)
              {
                if((j<m-1))
                  if(IBOUND[e-1+l]>0)
                  {
                    Cy=CC[e-1];
                    sum+=w*Cx*Cy/CD;
                  }
                if((k<n-1))
                  if(IBOUND[e-1+lm]>0)
                  {
                    Cz=CV[e-1];
                    sum+=w*Cx*Cz/CD;
                  }
              }
            }
          if(j>0)
            if(IBOUND[e-l]>0)
            {
              CD=pivots[e-l];
              Cy=CC[e-l];
              sum+=Cy*Cy/CD;

              /* Add fill-in to diagonal */
              if(w>0.0)
              {
                if((k<n-1))
                  if(IBOUND[e-l+lm]>0)
                  {
                    Cz=CV[e-l];
                    sum+=w*Cy*Cz/CD;
                  }
                if((i<l-1))
                  if(IBOUND[e-l+1]>0)
                  {
                    Cx=CR[e-l];
                    sum+=w*Cy*Cx/CD;
                  }
              }
            }
          if(k>0)
            if(IBOUND[e-lm]>0)
            {
              CD=pivots[e-lm];
              Cz=CV[e-lm];
              sum+=Cz*Cz/CD;

              /* Add fill-in to diagonal */
              if(w>0.0)
              {
                if((i<l-1))
                  if(IBOUND[e-lm+1]>0)
                  {
                    Cx=CR[e-lm];
                    sum+=w*Cz*Cx/CD;
                  }
                if((j<m-1))
                  if(IBOUND[e-lm+l]>0)
                  {
                    Cy=CC[e-lm];
                    sum+=w*Cz*Cy/CD;
                  }
              }
            }
        }
        pivots[e]=DD[e]-sum;
        if(pivots[e]<=0.0)
          return -1;
      }
    }
  }

  return 0;
}

int SCCFD_ILU_eval(r_vector* p2_ptr, r_vector* p1_ptr, 
                   CCFD_ILU_operator* ILU_ptr);
int DCCFD_ILU_eval(r_vector* p2_ptr, r_vector* p1_ptr, 
                   CCFD_ILU_operator* ILU_ptr);

/*
 * Evaluates the ILU0-D preconditioner.  In matrix form, the 
 * ILU0-D factorization is given by
 *
 * (L+D)(I+D^{-1}U)
 *
 * CCFD_ILU_eval computes the inverse using a back solve
 * followed by a forward solve.  
 *
 * p2_ptr and p1_ptr can point to the same vector.
 *
 */
int CCFD_ILU_eval(r_vector* p2_ptr, r_vector* p1_ptr, void* A_ptr)
{
  CCFD_ILU_operator *ILU_ptr=A_ptr;
  CCFD_operator *CCFD_ptr=ILU_ptr->CCFD_ptr;
  int prec=CCFD_ptr->prec;

  if(prec==0)
    SCCFD_ILU_eval(p2_ptr,p1_ptr,ILU_ptr);
  else
    DCCFD_ILU_eval(p2_ptr,p1_ptr,ILU_ptr);

  return 0;
}

int SCCFD_ILU_eval(r_vector* p2_ptr, r_vector* p1_ptr, 
                   CCFD_ILU_operator* ILU_ptr)
{
  CCFD_operator* CCFD_ptr=ILU_ptr->CCFD_ptr;

  int l,m,n;
  int lm;
  int i,j,k;
  int jl,klm;
  int e;

  int* IBOUND;
  float *CC,*CR,*CV;
  double *pivots;
  double *p1,*p2;
  double sum;

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;

  lm=l*m;

  IBOUND=CCFD_ptr->IBOUND;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  pivots=ILU_ptr->pivots.vec;

  p1=p1_ptr->vec;
  p2=p2_ptr->vec;

  /* Back solve */
  klm=0;
  for(k=0;k<n;k++)
  {
    jl=0;
    for(j=0;j<m;j++)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(k>0)
            if(IBOUND[e-lm]>0)
              sum+=CV[e-lm]*p2[e-lm];
          if(j>0)
            if(IBOUND[e-l]>0)
              sum+=CC[e-l]*p2[e-l];
          if(i>0)
            if(IBOUND[e-1]>0)
              sum+=CR[e-1]*p2[e-1];
        }
        p2[e]=(p1[e]+sum)/pivots[e];
      }
      jl+=l;
    }
    klm+=lm;
  }

  /* Forward Solve */
  klm=(n-1)*lm;
  for(k=n-1;k>=0;k--)
  {
    jl=(m-1)*l;
    for(j=m-1;j>=0;j--)
    {
      for(i=l-1;i>=0;i--)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(k<n-1)
            if(IBOUND[e+lm]>0)
              sum+=CV[e]*p2[e+lm];
          if(j<m-1)
            if(IBOUND[e+l]>0)
              sum+=CC[e]*p2[e+l];
          if(i<l-1)
            if(IBOUND[e+1]>0)
              sum+=CR[e]*p2[e+1];
        }
        p2[e]=p2[e]+sum/pivots[e];
      }
      jl-=l;
    }
    klm-=lm;
  }

  return 0;
}

int DCCFD_ILU_eval(r_vector* p2_ptr, r_vector* p1_ptr, 
                   CCFD_ILU_operator* ILU_ptr)
{
  CCFD_operator* CCFD_ptr=ILU_ptr->CCFD_ptr;

  int l,m,n;
  int lm;
  int i,j,k;
  int jl,klm;
  int e;

  int* IBOUND;
  double *CC,*CR,*CV;
  double *pivots;
  double *p1,*p2;
  double sum;

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;

  lm=l*m;

  IBOUND=CCFD_ptr->IBOUND;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  pivots=ILU_ptr->pivots.vec;

  p1=p1_ptr->vec;
  p2=p2_ptr->vec;

  /* Back solve */
  klm=0;
  for(k=0;k<n;k++)
  {
    jl=0;
    for(j=0;j<m;j++)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(k>0)
            if(IBOUND[e-lm]>0)
              sum+=CV[e-lm]*p2[e-lm];
          if(j>0)
            if(IBOUND[e-l]>0)
              sum+=CC[e-l]*p2[e-l];
          if(i>0)
            if(IBOUND[e-1]>0)
              sum+=CR[e-1]*p2[e-1];
        }
        p2[e]=(p1[e]+sum)/pivots[e];
      }
      jl+=l;
    }
    klm+=lm;
  }

  /* Forward Solve */
  klm=(n-1)*lm;
  for(k=n-1;k>=0;k--)
  {
    jl=(m-1)*l;
    for(j=m-1;j>=0;j--)
    {
      for(i=l-1;i>=0;i--)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(k<n-1)
            if(IBOUND[e+lm]>0)
              sum+=CV[e]*p2[e+lm];
          if(j<m-1)
            if(IBOUND[e+l]>0)
              sum+=CC[e]*p2[e+l];
          if(i<l-1)
            if(IBOUND[e+1]>0)
              sum+=CR[e]*p2[e+1];
        }
        p2[e]=p2[e]+sum/pivots[e];
      }
      jl-=l;
    }
    klm-=lm;
  }
  return 0;
}


/* Symmetric Gauss-Seidel (SSGS):
 *
 * Equivalent to ILU0-D evaluation method.
 *
 */
int CCFD_SGS_allocate(GEN_operator* GEN_ptr)
{
  return 0;
}

void CCFD_SGS_free(void* A_ptr)
{
}

/*
 */
int CCFD_SGS_assemble(GEN_operator* SGS_GEN_ptr, 
                      GEN_operator* CCFD_GEN_ptr, double w)
                      
{
  GEN_assemble(SGS_GEN_ptr,CCFD_GEN_ptr->A_ptr,CCFD_SGS_eval,CCFD_SGS_free);

  return 0;
}

int SCCFS_SGS_eval(r_vector* p2_ptr, r_vector* p1_ptr, 
                   CCFD_operator* CCFD_ptr);
int DCCFD_SGS_eval(r_vector* p2_ptr, r_vector* p1_ptr, 
                   CCFD_operator* CCFD_ptr);

int CCFD_SGS_eval(r_vector* p2_ptr, r_vector* p1_ptr, void* A_ptr)
{
  CCFD_operator *CCFD_ptr=A_ptr;
  int prec=CCFD_ptr->prec;

  if(prec==0)
    SCCFS_SGS_eval(p2_ptr,p1_ptr,CCFD_ptr);
  else
    DCCFD_SGS_eval(p2_ptr,p1_ptr,CCFD_ptr);

  return 0;
}

int SCCFS_SGS_eval(r_vector* p2_ptr, r_vector* p1_ptr, 
                   CCFD_operator* CCFD_ptr)
{
  int l,m,n;
  int lm;
  int i,j,k;
  int jl,klm;
  int e;

  int* IBOUND;
  float *CC,*CR,*CV;
  double *pivots;
  double *p1,*p2;
  double sum;

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;

  lm=l*m;

  IBOUND=CCFD_ptr->IBOUND;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  pivots=CCFD_ptr->DD;

  p1=p1_ptr->vec;
  p2=p2_ptr->vec;

  /* Back Solve */
  klm=0;
  for(k=0;k<n;k++)
  {
    jl=0;
    for(j=0;j<m;j++)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(k>0)
            if(IBOUND[e-lm]>0)
              sum+=CV[e-lm]*p2[e-lm];
          if(j>0)
            if(IBOUND[e-l]>0)
              sum+=CC[e-l]*p2[e-l];
          if(i>0)
            if(IBOUND[e-1]>0)
              sum+=CR[e-1]*p2[e-1];
        }
        p2[e]=(p1[e]+sum)/pivots[e];
      }
      jl+=l;
    }
    klm+=lm;
  }

  /* Forward Solve */
  klm=(n-1)*lm;
  for(k=n-1;k>=0;k--)
  {
    jl=(m-1)*l;
    for(j=m-1;j>=0;j--)
    {
      for(i=l-1;i>=0;i--)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(k<n-1)
            if(IBOUND[e+lm]>0)
              sum+=CV[e]*p2[e+lm];
          if(j<m-1)
            if(IBOUND[e+l]>0)
              sum+=CC[e]*p2[e+l];
          if(i<l-1)
            if(IBOUND[e+1]>0)
              sum+=CR[e]*p2[e+1];
        }
        p2[e]=p2[e]+sum/pivots[e];
      }
      jl-=l;
    }
    klm-=lm;
  }
  return 0;
}

int DCCFD_SGS_eval(r_vector* p2_ptr, r_vector* p1_ptr, 
                   CCFD_operator* CCFD_ptr)
{
  int l,m,n;
  int lm;
  int i,j,k;
  int jl,klm;
  int e;

  int* IBOUND;
  double *CC,*CR,*CV;
  double *pivots;
  double *p1,*p2;
  double sum;

  l=CCFD_ptr->rdp->l;
  m=CCFD_ptr->rdp->m;
  n=CCFD_ptr->rdp->n;

  lm=l*m;

  IBOUND=CCFD_ptr->IBOUND;
  CC=CCFD_ptr->CC;
  CR=CCFD_ptr->CR;
  CV=CCFD_ptr->CV;

  pivots=CCFD_ptr->DD;

  p1=p1_ptr->vec;
  p2=p2_ptr->vec;

  /* Back Solve */
  klm=0;
  for(k=0;k<n;k++)
  {
    jl=0;
    for(j=0;j<m;j++)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(k>0)
            if(IBOUND[e-lm]>0)
              sum+=CV[e-lm]*p2[e-lm];
          if(j>0)
            if(IBOUND[e-l]>0)
              sum+=CC[e-l]*p2[e-l];
          if(i>0)
            if(IBOUND[e-1]>0)
              sum+=CR[e-1]*p2[e-1];
        }
        p2[e]=(p1[e]+sum)/pivots[e];
      }
      jl+=l;
    }
    klm+=lm;
  }

  /* Forward Solve */
  klm=(n-1)*lm;
  for(k=n-1;k>=0;k--)
  {
    jl=(m-1)*l;
    for(j=m-1;j>=0;j--)
    {
      for(i=l-1;i>=0;i--)
      {
        e=i+jl+klm;

        sum=0.0;
        if(IBOUND[e]>0)
        {
          if(k<n-1)
            if(IBOUND[e+lm]>0)
              sum+=CV[e]*p2[e+lm];
          if(j<m-1)
            if(IBOUND[e+l]>0)
              sum+=CC[e]*p2[e+l];
          if(i<l-1)
            if(IBOUND[e+1]>0)
              sum+=CR[e]*p2[e+1];
        }
        p2[e]=p2[e]+sum/pivots[e];
      }
      jl-=l;
    }
    klm-=lm;
  }
  return 0;
}


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
 * In this case, a relaxation parameter 0<= w <=1 may be used to improve
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
/* Matrix coarsening, prolongation, and restriction. */
int CCFD_RAP_assemble(GEN_operator* GEN_AH_ptr, GEN_operator* GEN_Ah_ptr);
int CCFD_P_eval(r_vector* ph_ptr, r_vector* pH_ptr, void* A_ptr);
int CCFD_R_eval(r_vector* ph_ptr, r_vector* pH_ptr, void* A_ptr);

/* Computes number of levels and r_data on each level. */
int CCFD_MG_r_data_assemble(MG_r_data* mgrdp, int SC, r_data* rdp);

/* Allocate CCFD MG structure, compute multilevel r_data, and
 * allocate multilevel generic operators.
 */
int CCFD_MG_allocate(GEN_operator* GEN_CCFD_MG_ptr,
                     GEN_operator* GEN_CCFD_ptr,
                     r_data* rdp, int SM, int SC)
{
  CCFD_MG_operator *CCFD_MG_ptr;
  CCFD_operator *CCFD_ptr=GEN_CCFD_ptr->A_ptr;
  int prec=CCFD_ptr->prec;

  int size,total_size=0;
  int i,levels;

  GEN_operator *A_list,*B_list;
  r_data* rd_list;

  CCFD_MG_ptr=(CCFD_MG_operator*)calloc(1,sizeof(CCFD_MG_operator));
  if(CCFD_MG_ptr==NULL)
    return -1;
  size=sizeof(CCFD_MG_operator);
  total_size+=size;

  GEN_assemble(GEN_CCFD_MG_ptr,CCFD_MG_ptr,CCFD_MG_eval,CCFD_MG_free);

  /* Assemble multilevel r_data */
  size=CCFD_MG_r_data_assemble(&CCFD_MG_ptr->mgrd,SC,rdp);
  if(size<0)
    return -1;
  total_size+=size;

  levels=CCFD_MG_ptr->mgrd.levels;
  rd_list=CCFD_MG_ptr->mgrd.rd_list;

  /* Allocate multilevel prolongation */
  size=MG_GEN_allocate(&CCFD_MG_ptr->MGP,&CCFD_MG_ptr->mgrd);
  if(size<0)
    return -1;
  total_size+=size;

  /* Allocate multilevel restriction */
  size=MG_GEN_allocate(&CCFD_MG_ptr->MGR,&CCFD_MG_ptr->mgrd);
  if(size<0)
    return -1;
  total_size+=size;

  /* Allocate multilevel CCFD operator */
  size=MG_GEN_allocate(&CCFD_MG_ptr->MGCCFD,&CCFD_MG_ptr->mgrd);
  if(size<0)
    return -1;
  total_size+=size;
  
  /* Allocate CCFD matrix on every level except finest level. */
  A_list=CCFD_MG_ptr->MGCCFD.GEN_list;
  for(i=0;i<levels-1;i++)
  {
    size=CCFD_allocate(&A_list[i],&rd_list[i],prec);
    if(size<0)
      return -1;
    total_size+=size;
  }
  A_list[levels-1]=*GEN_CCFD_ptr;  /* Alias fine-grid matrix */

  /* Allocate multilevel smoother */
  size=MG_GEN_allocate(&CCFD_MG_ptr->MGB,&CCFD_MG_ptr->mgrd);
  if(size<0)
    return -1;
  total_size+=size;
  B_list=CCFD_MG_ptr->MGB.GEN_list;
  
  /* Coarse-grid solver */
  if(SC!=4)
  {
    size=CCFD_ILU_allocate(&B_list[0],&rd_list[0]);
    if(size<0)
      return -1;
    total_size+=size;
  }
  else
  {
    switch(SM)
    {
      case 0:
        size=CCFD_ILU_allocate(&B_list[0],&rd_list[0]);
        if(size<0)
          return -1;
        total_size+=size;
        break;
      case 1:
        size=CCFD_SGS_allocate(&B_list[0]);
        if(size<0)
          return -1;
        total_size+=size;
        break;
      default: return -1;
    }
  }

  switch(SM)
  {
    case 0:
      for(i=1;i<levels;i++)
      {
        size=CCFD_ILU_allocate(&B_list[i],&rd_list[i]);
        if(size<0)
          return -1;
        total_size+=size;
      }
      break;
    case 1:
      for(i=1;i<levels;i++)
      {
        size=CCFD_SGS_allocate(&B_list[i]);
        if(size<0)
          return -1;
        total_size+=size;
      }
      break;
  }

  /* Generic multigrid operator */
  size=MG_allocate(&CCFD_MG_ptr->MG,0,&CCFD_MG_ptr->mgrd);
  if(size<0)
    return -1;
  total_size+=size;

  CCFD_MG_ptr->SM=SM;
  CCFD_MG_ptr->SC=SC;

  return total_size;
}

void CCFD_MG_free(void* A_ptr)
{
  CCFD_MG_operator* CCFD_MG_ptr=A_ptr;
  
  MG_GEN_free(&CCFD_MG_ptr->MGP);
  MG_GEN_free(&CCFD_MG_ptr->MGR);
  MG_GEN_free(&CCFD_MG_ptr->MGB);
  MG_GEN_free(&CCFD_MG_ptr->MGCCFD);

  GEN_free(&CCFD_MG_ptr->MG);
  MG_r_data_free(&CCFD_MG_ptr->mgrd);
  free(CCFD_MG_ptr);
}

/* Assemble multilevel operators */
int CCFD_MG_assemble(GEN_operator* GEN_CCFD_MG_ptr, double w)
{
  CCFD_MG_operator* CCFD_MG_ptr=GEN_CCFD_MG_ptr->A_ptr;

  int i,size;
  int levels=CCFD_MG_ptr->mgrd.levels;

  int SM,SC;

  GEN_operator *P_list,*R_list,*B_list,*A_list;

  P_list=CCFD_MG_ptr->MGP.GEN_list;
  R_list=CCFD_MG_ptr->MGR.GEN_list;
  B_list=CCFD_MG_ptr->MGB.GEN_list;
  A_list=CCFD_MG_ptr->MGCCFD.GEN_list;

  for(i=0;i<levels;i++)
  {
    GEN_assemble(&P_list[i],NULL,CCFD_P_eval,NULL);
    GEN_assemble(&R_list[i],NULL,CCFD_R_eval,NULL);
  }

  /* Coarsen CCFD matrices */
  for(i=levels-1;i>0;i--)
  {
    size=CCFD_RAP_assemble(&A_list[i-1],&A_list[i]);
    if(size<0)
      return -1;
  }

  SM=CCFD_MG_ptr->SM;
  SC=CCFD_MG_ptr->SC;

  /* Coarse-grid solver */
  if(SC!=4)
  {
    size=CCFD_ILU_assemble(&B_list[0],&A_list[0],w);
    if(size<0)
      return -1;
  }
  else
  {
    switch(SM)
    {
      case 0:
        size=CCFD_ILU_assemble(&B_list[0],&A_list[0],w);
        if(size<0)
          return -1;
        break;
      case 1:
        size=CCFD_SGS_assemble(&B_list[0],&A_list[0],w);
        if(size<0)
          return -1;
        break;
    }
  }

  switch(SM)
  {
    case 0:
      for(i=1;i<levels;i++)
      {
        size=CCFD_ILU_assemble(&B_list[i],&A_list[i],0.0);
        if(size<0)
          return -1;
      }
      break;

    case 1:
      for(i=1;i<levels;i++)
      {
        size=CCFD_SGS_assemble(&B_list[i],&A_list[i],w);
        if(size<0)
          return -1;
      }
  }

  /* Pass pointers of multilevel operators to MG operator */
  MG_assemble(&CCFD_MG_ptr->MG,  &CCFD_MG_ptr->MGCCFD,
              &CCFD_MG_ptr->MGB, 
              &CCFD_MG_ptr->MGP, &CCFD_MG_ptr->MGR);

  return 0;
}

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
                 int mu0, int mu1, int nu)
{
  CCFD_MG_operator* CCFD_MG_ptr=GEN_CCFD_MG_ptr->A_ptr;

  MG_set(&CCFD_MG_ptr->MG,mu0,mu1,nu);
}

int CCFD_MG_eval(r_vector* r2_ptr, r_vector* r1_ptr, void* A_ptr)
{
  CCFD_MG_operator *CCFD_MG_ptr=A_ptr;
  return GEN_eval(r2_ptr,r1_ptr,&CCFD_MG_ptr->MG);
}

/*
 *  CCFD prolongation is natrual embedding.  Three-dimensional
 *  prolongation is implemented as a series of two-dimensional 
 *  prolongations that in turn are implemented as a series of
 *  one-dimensional prolongations.
 */

/* One-dimensional prolongation */
void CCFD_Px(double* ph, double* pH, int l, int l0);

/* Two-dimensional prolongation */
void CCFD_Pxy(double* ph, double* pH, int l, int m, int l0, int m0);

/* Three-dimensional prolongation */
void CCFD_Pxyz(double* ph, double* pH, 
               int l, int m, int n, 
               int l0, int m0, int n0);

int CCFD_P_eval(r_vector* ph_ptr, r_vector* pH_ptr, void* A_ptr)
{
  int l0,m0,n0;
  int l,m,n;
  double *pH,*ph;

  l0=pH_ptr->rdp->l;
  m0=pH_ptr->rdp->m;
  n0=pH_ptr->rdp->n;

  l=ph_ptr->rdp->l;
  m=ph_ptr->rdp->m;
  n=ph_ptr->rdp->n;

  pH=pH_ptr->vec;
  ph=ph_ptr->vec;

  CCFD_Pxyz(ph,pH,l,m,n,l0,m0,n0);

  return 0;
}

void CCFD_Px(double* ph, double* pH, int l, int l0)
{
  int i,i0;

  /* Columns not coarsened */
  if(l0==l)
  {
    for(i=0;i<l;i++)
      ph[i]=pH[i];
    return;
  }
 
  for(i0=0;i0<l0-1;i0++)
  {
    i=i0+i0;
    ph[i]=pH[i0];
    ph[i+1]=pH[i0];
  }

  i=i0+i0;
  ph[i]=pH[l0-1];

  /* If number of columns even */
  if(i<l-1)
    ph[i+1]=pH[l0-1];
}

void CCFD_Pxy(double* ph, double* pH, int l, int m, int l0, int m0)
{
  int j,j0;
  int jl,j0l,j0l0;

  /* Rows not coarsened */
  if(m0==m)
  {
    for(j0=0, j0l=0, j0l0=0 ;j0<m0; j0++, j0l+=l, j0l0+=l0)
      CCFD_Px(ph+j0l,pH+j0l0,l,l0);
    return;
  }
 
  for(j0=0, j0l=0, j0l0=0 ;j0<m0-1; j0++, j0l+=l, j0l0+=l0)
  {
    jl=j0l+j0l;
    CCFD_Px(ph+jl,pH+j0l0,l,l0);
    CCFD_Px(ph+jl+l,pH+j0l0,l,l0);
  }

  j=j0+j0;
  jl=j0l+j0l;

  CCFD_Px(ph+jl,pH+j0l0,l,l0);
  
  /* If number of rows even */
  if(j<m-1)
    CCFD_Px(ph+jl+l,pH+j0l0,l,l0);
}

void CCFD_Pxyz(double* ph, double* pH, 
               int l, int m, int n, 
               int l0, int m0, int n0)
{
  int lm,l0m0;
  int k,k0;
  int klm,k0lm,k0l0m0;

  lm=l*m;
  l0m0=l0*m0;

  /* Layers not coarsened */
  if(n0==n)
  {
    for(k0=0, k0lm=0, k0l0m0=0 ;k0<n0; k0++, k0lm+=lm, k0l0m0+=l0m0)
      CCFD_Pxy(ph+k0lm,pH+k0l0m0,l,m,l0,m0);
    return;
  }
 
  for(k0=0, k0lm=0, k0l0m0=0 ;k0<n0-1; k0++, k0lm+=lm, k0l0m0+=l0m0)
  {
    klm=k0lm+k0lm;
    CCFD_Pxy(ph+klm,pH+k0l0m0,l,m,l0,m0);
    CCFD_Pxy(ph+klm+lm,pH+k0l0m0,l,m,l0,m0);
  }

  k=k0+k0;
  klm=k0lm+k0lm;

  CCFD_Pxy(ph+klm,pH+k0l0m0,l,m,l0,m0);
  
  /* If number of layers even */
  if(k<n-1)
    CCFD_Pxy(ph+klm+lm,pH+k0l0m0,l,m,l0,m0);

  return;
}

/*
 *  CCFD restriction is transpose of CCFD prolongation.  Three-dimensional
 *  restriction is implemented as a series of two-dimensional 
 *  restrictions that in turn are implemented as a series of
 *  one-dimensional restrictions.
 */
/* One-dimensional restriction */
void CCFD_Rx(double* pH, double* ph, int l, int l0);

/* Two-dimensional restriction */
void CCFD_Rxy(double* pH, double* ph, int l, int m, int l0, int m0);

/* Three-dimensional restriction */
void CCFD_Rxyz(double* pH, double* ph, 
               int l, int m, int n, 
               int l0, int m0, int n0);

int CCFD_R_eval(r_vector* pH_ptr, r_vector* ph_ptr, void* A_ptr)
{
  int l0,m0,n0;
  int l,m,n;
  double *pH,*ph;

  l0=pH_ptr->rdp->l;
  m0=pH_ptr->rdp->m;
  n0=pH_ptr->rdp->n;

  l=ph_ptr->rdp->l;
  m=ph_ptr->rdp->m;
  n=ph_ptr->rdp->n;

  pH=pH_ptr->vec;
  ph=ph_ptr->vec;

  r_zero(pH_ptr);

  CCFD_Rxyz(pH,ph,l,m,n,l0,m0,n0);

  return 0;
}

void CCFD_Rx(double* pH, double* ph, int l, int l0)
{
  int i,i0;

  /* Columns not coarsened */
  if(l0==l)
  {
    for(i=0;i<l;i++)
      pH[i]+=ph[i];
    return;
  }
 
  for(i0=0;i0<l0-1;i0++)
  {
    i=i0+i0;
    pH[i0]+=ph[i]+ph[i+1];
  }

  i=i0+i0;
  pH[l0-1]+=ph[i];

  /* If number of columns even */
  if(i<l-1) 
    pH[l0-1]+=ph[i+1];
}

void CCFD_Rxy(double* pH, double* ph, int l, int m, int l0, int m0)
{
  int j,j0;
  int jl,j0l,j0l0;

  /* Rows not coarsened */
  if(m0==m)
  {
    for(j0=0, j0l=0, j0l0=0 ;j0<m0; j0++, j0l+=l, j0l0+=l0)
      CCFD_Rx(pH+j0l0,ph+j0l,l,l0);
    return;
  }
 
  for(j0=0, j0l=0, j0l0=0 ;j0<m0-1; j0++, j0l+=l, j0l0+=l0)
  {
    jl=j0l+j0l;
    CCFD_Rx(pH+j0l0,ph+jl,l,l0);
    CCFD_Rx(pH+j0l0,ph+jl+l,l,l0);
  }

  j=j0+j0;
  jl=j0l+j0l;

  CCFD_Rx(pH+j0l0,ph+jl,l,l0);
  
  /* If number of rows even */
  if(j<m-1)
    CCFD_Rx(pH+j0l0,ph+jl+l,l,l0);
}

void CCFD_Rxyz(double* pH, double* ph, 
               int l, int m, int n, 
               int l0, int m0, int n0)
{
  int lm,l0m0;
  int k,k0;
  int klm,k0lm,k0l0m0;

  lm=l*m;
  l0m0=l0*m0;

  /* Layers not coarsened */
  if(n0==n)
  {
    for(k0=0, k0lm=0, k0l0m0=0 ;k0<n0; k0++, k0lm+=lm, k0l0m0+=l0m0)
      CCFD_Rxy(pH+k0l0m0,ph+k0lm,l,m,l0,m0);
    return;
  }
 
  for(k0=0, k0lm=0, k0l0m0=0 ;k0<n0-1; k0++, k0lm+=lm, k0l0m0+=l0m0)
  {
    klm=k0lm+k0lm;
    CCFD_Rxy(pH+k0l0m0,ph+klm,l,m,l0,m0);
    CCFD_Rxy(pH+k0l0m0,ph+klm+lm,l,m,l0,m0);
  }

  k=k0+k0;
  klm=k0lm+k0lm;

  CCFD_Rxy(pH+k0l0m0,ph+klm,l,m,l0,m0);
  
  /* If number of layers even */
  if(k<n-1)
    CCFD_Rxy(pH+k0l0m0,ph+klm+lm,l,m,l0,m0);

  return;
}

/*
 *  CCFD RAP is matrix coarsening defined by 1/2*(R*A*P).
 *
 *  Three-dimensional CCFD RAP is implemented as a series
 *  of two-dimensional RAP's which are in turn a series
 *  of one-dimensional RAP's.
 *
 *  A temporary array EEh is assembled to eliminate conductances
 *  from the diagonal corresponding to specified heads.
 *
 *  The one-dimensional RAP coarsens row-conductances if the integer
 *  j=1 and layer conductances if k=1.
 */

/* Single precision CCFD RAP */
int SCCFD_RAP_assemble(CCFD_operator* AH_ptr, CCFD_operator* Ah_ptr);

/* Double precision CCFD RAP */
int DCCFD_RAP_assemble(CCFD_operator* AH_ptr, CCFD_operator* Ah_ptr);

int CCFD_RAP_assemble(GEN_operator* GEN_AH_ptr, GEN_operator* GEN_Ah_ptr)
{
  CCFD_operator *AH_ptr=GEN_AH_ptr->A_ptr;
  CCFD_operator *Ah_ptr=GEN_Ah_ptr->A_ptr;

  if(AH_ptr->prec==0)
    SCCFD_RAP_assemble(AH_ptr,Ah_ptr);
  else
    DCCFD_RAP_assemble(AH_ptr,Ah_ptr);

  return 0;
}


/* One-dimensional RAP */
void SCCFD_RAPx(double* EEh, float* CCh, float* CRh, float* CVh, int* IBh,
                double* EEH, float* CCH, float* CRH, float* CVH, int* IBH,
                int l, int l0, double wy, int j, int k);
/* Two-dimensional RAP */
void SCCFD_RAPxy(double* EEh, float* CCh, float* CRh, float* CVh, int* IBh,
                 double* EEH, float* CCH, float* CRH, float* CVH, int* IBH,
                 int l, int m, int l0, int m0, double wz, int k);
/* Three-dimensional RAP */
void SCCFD_RAPxyz(double* EEh, float* CCh, float* CRh, float* CVh, int* IBh,
                  double* EEH, float* CCH, float* CRH, float* CVH, int* IBH,
                  int l, int m, int n, int l0, int m0, int n0);

int SCCFD_RAP_assemble(CCFD_operator* AH_ptr, CCFD_operator* Ah_ptr)
{
  int i,j,k;
  int lm;
  int e,jl,klm,neq;
  double sum;

  int* IBh;
  double *EEh,*DDh;
  float *CCh,*CRh,*CVh;
  int l,m,n;

  int* IBH;
  double *DDH;
  float *CCH,*CRH,*CVH;
  int l0,m0,n0;

  CCFD_zero(AH_ptr);

  l=Ah_ptr->rdp->l;
  m=Ah_ptr->rdp->m;
  n=Ah_ptr->rdp->n;

  IBh=Ah_ptr->IBOUND;
  DDh=Ah_ptr->DD;
  CCh=Ah_ptr->CC;
  CRh=Ah_ptr->CR;
  CVh=Ah_ptr->CV;

  l0=AH_ptr->rdp->l;
  m0=AH_ptr->rdp->m;
  n0=AH_ptr->rdp->n;

  IBH=AH_ptr->IBOUND;
  DDH=AH_ptr->DD;
  CCH=AH_ptr->CC;
  CRH=AH_ptr->CR;
  CVH=AH_ptr->CV;

  lm=l*m;
  neq=lm*n;
  EEh=(double*)calloc(neq,sizeof(double));
  if(EEh==NULL)
    return -1;

  for(k=0, klm=0 ;k<n; k++, klm+=lm)
  {
    for(j=0, jl=0 ;j<m; j++, jl+=l)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        if(IBh[e]>0)
        {
          sum=0.0;
    
          if(k>0)
            if(IBh[e-lm]>0)
              sum-=CVh[e-lm];
          if(j>0)
            if(IBh[e-l]>0)
              sum-=CCh[e-l];
          if(i>0)
            if(IBh[e-1]>0)
              sum-=CRh[e-1];
          sum+=DDh[e];
          if(i<l-1)
            if(IBh[e+1]>0)
              sum-=CRh[e];
          if(j<m-1)
            if(IBh[e+l]>0)
              sum-=CCh[e];
          if(k<n-1)
            if(IBh[e+lm]>0)
              sum-=CVh[e];
    
          EEh[e]=sum;
        }
        else
          EEh[e]=0.0;
      }
    }
  }

  SCCFD_RAPxyz(EEh,CRh,CCh,CVh,IBh,
               DDH,CRH,CCH,CVH,IBH,
               l,m,n,l0,m0,n0);
  free(EEh);

  return 0;
}

void SCCFD_RAPx(double* EEh, float* CRh, float* CCh, float* CVh, int* IBh,
                double* EEH, float* CRH, float* CCH, float* CVH, int* IBH,
                int l, int l0, double wy, int j, int k)
{
  int i,i0;

  /* Columns not coarsened */
  if(l0==l)
  {
    for(i0=0; i0<l; i0++)
    {
      EEH[i0]+=EEh[i0]*wy;
      if(IBh[i0]>0)
      {
        CRH[i0]+=CRh[i0]*wy;
        if(j == 1)
          CCH[i0]+=CCh[i0]*wy;
        if(k==1)
          CVH[i0]+=CVh[i0]*wy;
      }
    }
    return;
  }

  for(i0=0;i0<l0-1;i0++)
  {
    i=i0+i0;
    EEH[i0]+=(EEh[i]+EEh[i+1])*0.5;
    if(IBh[i]>0)
    {
      if(j==1)
        CCH[i0]+=CCh[i]*0.5;
      if(k==1)
        CVH[i0]+=CVh[i]*0.5;
    }
    if(IBh[i+1]>0)
    {
      CRH[i0]+=CRh[i+1]*0.5;
      if(j==1)
        CCH[i0]+=CCh[i+1]*0.5;
      if(k==1)
        CVH[i0]+=CVh[i+1]*0.5;
    }
  }
  i0=l0-1;
  i=i0+i0;

  /* If number of columns even */
  if(i<l-1)
  {
    EEH[l0-1]+=(EEh[i]+EEh[i+1])*0.5;
    if(IBh[i]>0)
    {
      if(j==1)
        CCH[l0-1]+=CCh[i]*0.5;
      if(k==1)
        CVH[l0-1]+=CVh[i]*0.5;
      if(IBh[i+1]>0)
      {
        if(j==1)
          CCH[l0-1]+=CCh[i+1]*0.5;
        if(k==1)
          CVH[l0-1]+=CVh[i+1]*0.5;
      }
    }
  }
  else
  {
    EEH[l0-1]+=EEh[i]*wy;
    if(IBh[i]>0)
    {
      if(j==1)
        CCH[l0-1]+=CCh[i]*wy;
      if(k==1)
        CVH[l0-1]+=CVh[i]*wy;
    }
  }

  return;
}

void SCCFD_RAPxy(double* EEh, float* CRh, float* CCh, float* CVh, int* IBh,
                 double* EEH, float* CRH, float* CCH, float* CVH, int* IBH,
                 int l, int m, int l0, int m0, double wz, int k)
{
  int j,j0;
  int jl,j0l,j0l0;

  /* Rows not coarsened */
  if(m0==m)
  {
    for(j0=0, j0l=0, j0l0=0; j0<m0-1; j0++, j0l+=l, j0l0+=l0)
      SCCFD_RAPx(EEh+j0l, CRh+j0l, CCh+j0l, CVh+j0l, IBh+j0l,
                 EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
                 l,l0,wz,1,k);

    SCCFD_RAPx(EEh+j0l, CRh+j0l, CCh+j0l, CVh+j0l, IBh+j0l,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,wz,0,k);
    return;
  }


  for(j0=0, j0l=0, j0l0=0 ;j0<m0-1; j0++, j0l+=l, j0l0+=l0)
  {
    j=j0+j0;
    jl=j0l+j0l;

    SCCFD_RAPx(EEh+jl,  CRh+jl,  CCh+jl,  CVh+jl,  IBh+jl,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,0.5,0,k);

    SCCFD_RAPx(EEh+jl+l,CRh+jl+l,CCh+jl+l,CVh+jl+l,IBh+jl+l,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,0.5,1,k);
  }

  j0=m0-1;
  j0l0=j0*l0;

  j0l=j0*l;
  j=j0+j0;
  jl=j0l+j0l;

  /* If number of rows even */
  if(j<m-1)
  {
    SCCFD_RAPx(EEh+jl,  CRh+jl,  CCh+jl,  CVh+jl,  IBh+jl,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,0.5,0,k);

    SCCFD_RAPx(EEh+jl+l,CRh+jl+l,CCh+jl+l,CVh+jl+l,IBh+jl+l,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,0.5,0,k);
  }
  else
    SCCFD_RAPx(EEh+jl,  CRh+jl,  CCh+jl,  CVh+jl,  IBh+jl,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,wz,0,k);

  return;
}

void SCCFD_RAPxyz(double* EEh, float* CRh, float* CCh, float* CVh, int* IBh,
                  double* EEH, float* CRH, float* CCH, float* CVH, int* IBH,
                  int l, int m, int n, int l0, int m0, int n0)
{
  int k,k0;
  int klm,k0lm,k0l0m0;
  int lm,l0m0;

  int i0,j0;
  int j0l0;
  int eH;

  double* DDH;

  lm=l*m;
  l0m0=l0*m0;

  /* Layers not coarsened */
  if(n0==n)
  {
    for(k0=0, k0lm=0, k0l0m0=0; k0<n0-1; k0++, k0lm+=lm, k0l0m0+=l0m0)
      SCCFD_RAPxy(EEh+k0lm,  CRh+k0lm,  CCh+k0lm,  CVh+k0lm,  IBh+k0lm,
                  EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                  l,m,l0,m0,1.0,1);

    SCCFD_RAPxy(EEh+k0lm,  CRh+k0lm,  CCh+k0lm,  CVh+k0lm,  IBh+k0lm,
                EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                l,m,l0,m0,1.0,0);
  }
  else
  {
    for(k0=0, k0lm=0, k0l0m0=0; k0<n0-1; k0++, k0lm+=lm, k0l0m0+=l0m0)
    {
      k=k0+k0;
      klm=k0lm+k0lm;

      SCCFD_RAPxy(EEh+klm,  CRh+klm,  CCh+klm,  CVh+klm,  IBh+klm,
                 EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                 l,m,l0,m0,0.5,0);

      SCCFD_RAPxy(EEh+klm+lm,CRh+klm+lm,CCh+klm+lm,CVh+klm+lm,IBh+klm+lm,
                  EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                  l,m,l0,m0,0.5,1);
    }

    k0=n0-1;
    k0l0m0=k0*l0m0;

    k0lm=k0*lm;
    k=k0+k0;
    klm=k0lm+k0lm;

    /* If number of layers even */
    if(k<n-1)
    {
      SCCFD_RAPxy(EEh+klm,   CRh+klm,   CCh+klm,   CVh+klm,   IBh+klm,
                  EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                  l,m,l0,m0,0.5,0);

      SCCFD_RAPxy(EEh+klm+lm,CRh+klm+lm,CCh+klm+lm,CVh+klm+lm,IBh+klm+lm,
                  EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                  l,m,l0,m0,0.5,0);
    }
    else
      SCCFD_RAPxy(EEh+klm,   CRh+klm,   CCh+klm,   CVh+klm,   IBh+klm,
                  EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                  l,m,l0,m0,1.0,0);

  }

  /* Assemble diagonal and ibound */
  DDH=EEH;
  for(k0=0, k0l0m0=0 ;k0<n0; k0++, k0l0m0+=l0m0)
  {
    for(j0=0, j0l0=0 ;j0<m0; j0++, j0l0+=l0)
    {
      for(i0=0;i0<l0;i0++)
      {
        eH=i0+j0l0+k0l0m0;

        if(k0>0)
          DDH[eH]+=CVH[eH-l0m0];
        if(j0>0)
          DDH[eH]+=CCH[eH-l0];
        if(i0>0)
          DDH[eH]+=CRH[eH-1];
        if(i0<l0-1)
          DDH[eH]+=CRH[eH];
        if(j0<m0-1)
          DDH[eH]+=CCH[eH];
        if(k0<n0-1)
          DDH[eH]+=CVH[eH];
        if(DDH[eH]<=DBL_MIN)
        {
          DDH[eH]=1.0;
          IBH[eH]=0;
        }
        else
          IBH[eH]=1;
      }
    }
  }

  return;
}


/* One-dimensional RAP */
void DCCFD_RAPx(double* EEh, double* CRh, double* CCh, double* CVh, int* IBh,
                double* EEH, double* CRH, double* CCH, double* CVH, int* IBH,
                int l, int l0, double wy, int j, int k);

/* Two-dimensional RAP */
void DCCFD_RAPxy(double* EEh, double* CRh, double* CCh, double* CVh, int* IBh,
                 double* EEH, double* CRH, double* CCH, double* CVH, int* IBH,
                 int l, int m, int l0, int m0, double wz, int k);

/* Three-dimensional RAP */
void DCCFD_RAPxyz(double* EEh, double* CRh, double* CCh, double* CVh, int* IBh,
                  double* EEH, double* CRH, double* CCH, double* CVH, int* IBH,
                  int l, int m, int n, int l0, int m0, int n0);


int DCCFD_RAP_assemble(CCFD_operator* AH_ptr, CCFD_operator* Ah_ptr)
{
  int i,j,k;
  int lm;
  int e,jl,klm,neq;
  double sum;

  int* IBh;
  double *EEh,*DDh;
  double *CCh,*CRh,*CVh;
  int l,m,n;

  int* IBH;
  double *DDH;
  double *CCH,*CRH,*CVH;
  int l0,m0,n0;

  CCFD_zero(AH_ptr);

  l=Ah_ptr->rdp->l;
  m=Ah_ptr->rdp->m;
  n=Ah_ptr->rdp->n;

  IBh=Ah_ptr->IBOUND;
  DDh=Ah_ptr->DD;
  CCh=Ah_ptr->CC;
  CRh=Ah_ptr->CR;
  CVh=Ah_ptr->CV;

  l0=AH_ptr->rdp->l;
  m0=AH_ptr->rdp->m;
  n0=AH_ptr->rdp->n;

  IBH=AH_ptr->IBOUND;
  DDH=AH_ptr->DD;
  CCH=AH_ptr->CC;
  CRH=AH_ptr->CR;
  CVH=AH_ptr->CV;

  lm=l*m;
  neq=lm*n;
  EEh=(double*)calloc(neq,sizeof(double));
  if(EEh==NULL)
    return -1;

  for(k=0, klm=0 ;k<n; k++, klm+=lm)
  {
    for(j=0, jl=0 ;j<m; j++, jl+=l)
    {
      for(i=0;i<l;i++)
      {
        e=i+jl+klm;

        if(IBh[e]>0)
        {
          sum=0.0;
    
          if(k>0)
            if(IBh[e-lm]>0)
              sum-=CVh[e-lm];
          if(j>0)
            if(IBh[e-l]>0)
              sum-=CCh[e-l];
          if(i>0)
            if(IBh[e-1]>0)
              sum-=CRh[e-1];
          sum+=DDh[e];
          if(i<l-1)
            if(IBh[e+1]>0)
              sum-=CRh[e];
          if(j<m-1)
            if(IBh[e+l]>0)
              sum-=CCh[e];
          if(k<n-1)
            if(IBh[e+lm]>0)
              sum-=CVh[e];
    
          EEh[e]=sum;
        }
        else
          EEh[e]=0.0;
      }
    }
  }

  DCCFD_RAPxyz(EEh,CRh,CCh,CVh,IBh,
               DDH,CRH,CCH,CVH,IBH,
               l,m,n,l0,m0,n0);

  free(EEh);

  return 0;
}

void DCCFD_RAPx(double* EEh, double* CRh, double* CCh, double* CVh, int* IBh,
                double* EEH, double* CRH, double* CCH, double* CVH, int* IBH,
                int l, int l0, double wy, int j, int k)
{
  int i,i0;

  /* Columns not coarsened */
  if(l0==l)
  {
    for(i0=0; i0<l; i0++)
    {
      EEH[i0]+=EEh[i0]*wy;
      if(IBh[i0]>0)
      {
        CRH[i0]+=CRh[i0]*wy;
        if(j == 1)
          CCH[i0]+=CCh[i0]*wy;
        if(k==1)
          CVH[i0]+=CVh[i0]*wy;
      }
    }
    return;
  }

  for(i0=0;i0<l0-1;i0++)
  {
    i=i0+i0;
    EEH[i0]+=(EEh[i]+EEh[i+1])*0.5;
    if(IBh[i]>0)
    {
      if(j==1)
        CCH[i0]+=CCh[i]*0.5;
      if(k==1)
        CVH[i0]+=CVh[i]*0.5;
    }
    if(IBh[i+1]>0)
    {
      CRH[i0]+=CRh[i+1]*0.5;
      if(j==1)
        CCH[i0]+=CCh[i+1]*0.5;
      if(k==1)
        CVH[i0]+=CVh[i+1]*0.5;
    }
  }
  i0=l0-1;
  i=i0+i0;

  /* If number of columns even */
  if(i<l-1)
  {
    EEH[l0-1]+=(EEh[i]+EEh[i+1])*0.5;
    if(IBh[i]>0)
    {
      if(j==1)
        CCH[l0-1]+=CCh[i]*0.5;
      if(k==1)
        CVH[l0-1]+=CVh[i]*0.5;
      if(IBh[i+1]>0)
      {
        if(j==1)
          CCH[l0-1]+=CCh[i+1]*0.5;
        if(k==1)
          CVH[l0-1]+=CVh[i+1]*0.5;
      }
    }
  }
  else
  {
    EEH[l0-1]+=EEh[i]*wy;
    if(IBh[i]>0)
    {
      if(j==1)
        CCH[l0-1]+=CCh[i]*wy;
      if(k==1)
        CVH[l0-1]+=CVh[i]*wy;
    }
  }

  return;
}

void DCCFD_RAPxy(double* EEh, double* CRh, double* CCh, double* CVh, int* IBh,
                 double* EEH, double* CRH, double* CCH, double* CVH, int* IBH,
                 int l, int m, int l0, int m0, double wz, int k)
{
  int j,j0;
  int jl,j0l,j0l0;

  /* Rows not coarsened */
  if(m0==m)
  {
    for(j0=0, j0l=0, j0l0=0; j0<m0-1; j0++, j0l+=l, j0l0+=l0)
      DCCFD_RAPx(EEh+j0l, CRh+j0l, CCh+j0l, CVh+j0l, IBh+j0l,
                 EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
                 l,l0,wz,1,k);

    DCCFD_RAPx(EEh+j0l, CRh+j0l, CCh+j0l, CVh+j0l, IBh+j0l,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,wz,0,k);
    return;
  }


  for(j0=0, j0l=0, j0l0=0 ;j0<m0-1; j0++, j0l+=l, j0l0+=l0)
  {
    j=j0+j0;
    jl=j0l+j0l;

    DCCFD_RAPx(EEh+jl,  CRh+jl,  CCh+jl,  CVh+jl,  IBh+jl,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,0.5,0,k);

    DCCFD_RAPx(EEh+jl+l,CRh+jl+l,CCh+jl+l,CVh+jl+l,IBh+jl+l,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,0.5,1,k);
  }

  j0=m0-1;
  j0l0=j0*l0;

  j0l=j0*l;
  j=j0+j0;
  jl=j0l+j0l;

  /* If number of rows even */
  if(j<m-1)
  {
    DCCFD_RAPx(EEh+jl,  CRh+jl,  CCh+jl,  CVh+jl,  IBh+jl,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,0.5,0,k);

    DCCFD_RAPx(EEh+jl+l,CRh+jl+l,CCh+jl+l,CVh+jl+l,IBh+jl+l,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,0.5,0,k);
  }
  else
    DCCFD_RAPx(EEh+jl,  CRh+jl,  CCh+jl,  CVh+jl,  IBh+jl,
               EEH+j0l0,CRH+j0l0,CCH+j0l0,CVH+j0l0,IBH+j0l0,
               l,l0,wz,0,k);

  return;
}

void DCCFD_RAPxyz(double* EEh, double* CRh, double* CCh, double* CVh, int* IBh,
                  double* EEH, double* CRH, double* CCH, double* CVH, int* IBH,
                  int l, int m, int n, int l0, int m0, int n0)
{
  int k,k0;
  int klm,k0lm,k0l0m0;
  int lm,l0m0;

  int i0,j0;
  int j0l0;
  int eH;

  double* DDH;

  lm=l*m;
  l0m0=l0*m0;

  /* Layers not coarsened */
  if(n0==n)
  {
    for(k0=0, k0lm=0, k0l0m0=0; k0<n0-1; k0++, k0lm+=lm, k0l0m0+=l0m0)
      DCCFD_RAPxy(EEh+k0lm,  CRh+k0lm,  CCh+k0lm,  CVh+k0lm,  IBh+k0lm,
                  EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                  l,m,l0,m0,1.0,1);

    DCCFD_RAPxy(EEh+k0lm,  CRh+k0lm,  CCh+k0lm,  CVh+k0lm,IBh+k0lm,
                EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                l,m,l0,m0,1.0,0);
  }
  else
  {
    for(k0=0, k0lm=0, k0l0m0=0; k0<n0-1; k0++, k0lm+=lm, k0l0m0+=l0m0)
    {
      k=k0+k0;
      klm=k0lm+k0lm;

      DCCFD_RAPxy(EEh+klm,   CRh+klm,   CCh+klm,   CVh+klm,   IBh+klm,
                  EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                  l,m,l0,m0,0.5,0);

      DCCFD_RAPxy(EEh+klm+lm,CRh+klm+lm,CCh+klm+lm,CVh+klm+lm,IBh+klm+lm,
                  EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                  l,m,l0,m0,0.5,1);
    }

    k0=n0-1;
    k0l0m0=k0*l0m0;

    k0lm=k0*lm;
    k=k0+k0;
    klm=k0lm+k0lm;

    /* If number of layers even */
    if(k<n-1)
    {
      DCCFD_RAPxy(EEh+klm,   CRh+klm,   CCh+klm,   CVh+klm,   IBh+klm,
                  EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                  l,m,l0,m0,0.5,0);

      DCCFD_RAPxy(EEh+klm+lm,CRh+klm+lm,CCh+klm+lm,CVh+klm+lm,IBh+klm+lm,
                  EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                  l,m,l0,m0,0.5,0);
    }
    else
      DCCFD_RAPxy(EEh+klm,   CRh+klm,   CCh+klm,   CVh+klm,   IBh+klm,
                  EEH+k0l0m0,CRH+k0l0m0,CCH+k0l0m0,CVH+k0l0m0,IBH+k0l0m0,
                  l,m,l0,m0,1.0,0);

  }

  /* Coarsen diagonal and ibound */
  DDH=EEH;
  for(k0=0, k0l0m0=0 ;k0<n0; k0++, k0l0m0+=l0m0)
  {
    for(j0=0, j0l0=0 ;j0<m0; j0++, j0l0+=l0)
    {
      for(i0=0;i0<l0;i0++)
      {
        eH=i0+j0l0+k0l0m0;

        if(k0>0)
          DDH[eH]+=CVH[eH-l0m0];
        if(j0>0)
          DDH[eH]+=CCH[eH-l0];
        if(i0>0)
          DDH[eH]+=CRH[eH-1];
        if(i0<l0-1)
          DDH[eH]+=CRH[eH];
        if(j0<m0-1)
          DDH[eH]+=CCH[eH];
        if(k0<n0-1)
          DDH[eH]+=CVH[eH];
        if(DDH[eH]<=DBL_MIN)
        {
          DDH[eH]=1.0;
          IBH[eH]=0;
        }
        else
          IBH[eH]=1;
      }
    }
  }

  return;
}

/* 
 * CCFD_MG_r_data_assemble:
 * Semi-coarsening controlled by SC.
 * Coarsest level is 1-D unless SC=4, then no coarsening.
 */
int CCFD_MG_r_data_assemble(MG_r_data* mgrdp, int SC, r_data* rdp)
{
  int l,m,n;       /* Fine-Grid Extents */
  int s;           /* level s */
  int ls,ms,ns;    /* Extents on level s */
  int neq;         /* Number of equations */
  int levels;      /* Number of levels */
  int xl,yl,zl;    /* Coarsening Flag */
  int total_size;

  neq=rdp->neq;
  l=rdp->l, m=rdp->m, n=rdp->n;
  ls=l, ms=m, ns=n;

  /* Set coarsening flags;
   * 0 means no coarsening.
   */
  switch(SC)
  {
    case 0: xl=1, yl=1, zl=1; break;
    case 1: xl=1, yl=1, zl=0; break;
    case 2: xl=0, yl=1, zl=1; break;
    case 3: xl=1, yl=0, zl=1; break;
    case 4: xl=0, yl=0, zl=0; break;
    default: return -1;
  }

  /* Compute number of levels */
  levels=1;
  if(SC!=4)
    while((ls!=1 || ms!=1) && 
          (ms!=1 || ns!=1) && 
          (ns!=1 || ls!=1))
    {
      if(xl!=0)
        ls=ls/2+ls%2;
      if(yl!=0)
        ms=ms/2+ms%2;
      if(zl!=0)
        ns=ns/2+ns%2;

      levels++;
    }

  /* Allocate multilevel r_data */
  total_size=MG_r_data_allocate(mgrdp,levels);
  if(total_size<0)
    return -1;

  /* Assemble r_data on each level */
  ls=l, ms=m, ns=n;
  for(s=levels-1;s>=0;s--)
  {
    neq=ls*ms*ns;
    mgrdp->rd_list[s].neq=neq;
    mgrdp->rd_list[s].l=ls;
    mgrdp->rd_list[s].m=ms;
    mgrdp->rd_list[s].n=ns;

    if(xl!=0)
      ls=ls/2+ls%2;
    if(yl!=0)
      ms=ms/2+ms%2;
    if(zl!=0)
      ns=ns/2+ns%2;
  }

  return total_size;
}

