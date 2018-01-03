/* File: r_vector.c */

#include "r_vector.h"

/*
 *  The r_vector is the basic data structure for
 *  representing flux or pressure nodal values.
 *  An r_vector structure contains a pointer
 *  to a double vector and a pointer to an r_data structure.
 *
 *  The r_data structure contains the length of the
 *  double vector and the number of elements in the
 *  x-direction (l), y-direstion (m) and z-direction (n).
 *
 *  Several r_vector functions are defined for
 *  adding, subtracting, multiply add, etc.
 *  Many of these functions use loop unrolling
 *  to maximize cache efficiency.
 *
 */
int r_allocate(r_vector* r_ptr, r_data* rdp)
{
  r_ptr->vec=(double*)calloc(rdp->neq,sizeof(double));
  if(r_ptr->vec==NULL)
    return 0;

  r_ptr->rdp=rdp;

  return sizeof(double)*rdp->neq;
}

void r_free(r_vector* r_ptr)
{
  if(r_ptr->vec!=NULL)
    free(r_ptr->vec);
  r_ptr->vec=NULL;
}

/* BLAS-like functions */

double r_dotprd(r_vector* r1_ptr, r_vector* r2_ptr)
{
  double dot;
  double *r1,*r2;
  int i,m,neq;

  neq=r1_ptr->rdp->neq;
  if(neq<=0)
    return 0;
  if(neq!=r2_ptr->rdp->neq)
    return 0;

  r1=r1_ptr->vec;
  r2=r2_ptr->vec;

  dot = 0.0;

  m=neq%4;

  for(i=0;i<m;i++)
    dot+=r1[i]*r2[i];
  for(i=m;i<neq;i+=4)
    dot+=r1[i]*r2[i]
        +r1[i+1]*r2[i+1]
        +r1[i+2]*r2[i+2]
        +r1[i+3]*r2[i+3];

  return dot;
}

int r_scale(r_vector* r_ptr, double c)
{
  double *r;
  int i,m,neq;

  neq=r_ptr->rdp->neq;
  r=r_ptr->vec;

  if(neq<=0) return 0;

  m=neq%4;

  for(i=0;i<m;i++)
    r[i]*=c;
  for(i=m;i<neq;i+=4)
  {
    r[i]  *=c;
    r[i+1]*=c;
    r[i+2]*=c;
    r[i+3]*=c;
  }

  return 1;
}

int r_zero(r_vector* r_ptr)
{
  double *r;
  int i,m,neq;

  neq=r_ptr->rdp->neq;
  r=r_ptr->vec;

  if(neq<=0) return 0;

  m=neq%4;

  for(i=0;i<m;i++)
    r[i]=0.0;
  for(i=m;i<neq;i+=4)
  {
    r[i]  =0.0;
    r[i+1]=0.0;
    r[i+2]=0.0;
    r[i+3]=0.0;
  }

  return 1;
}

int r1_gets_r1_plus_cr2(r_vector* r1_ptr, r_vector* r2_ptr, double c)
{
  double *r1,*r2;
  int i,m,neq;

  neq=r1_ptr->rdp->neq;
  if(neq<=0)
    return 0;
  if(neq!=r2_ptr->rdp->neq)
    return 0;

  r1=r1_ptr->vec;
  r2=r2_ptr->vec;

  m=neq%4;

  for(i=0;i<m;i++)
    r1[i]+=c*r2[i];
  for(i=m;i<neq;i+=4)
  {
    r1[i]  +=c*r2[i];
    r1[i+1]+=c*r2[i+1];
    r1[i+2]+=c*r2[i+2];
    r1[i+3]+=c*r2[i+3];
  }

  return 1;
}

int r1_gets_cr1_plus_r2(r_vector* r1_ptr, r_vector* r2_ptr, double c)
{
  double *r1,*r2;
  int i,m,neq;

  neq=r1_ptr->rdp->neq;
  if(neq<=0)
    return 0;
  if(neq!=r2_ptr->rdp->neq)
    return 0;

  r1=r1_ptr->vec;
  r2=r2_ptr->vec;

  m=neq%4;

  for(i=0;i<m;i++)
    r1[i]=c*r1[i]+r2[i];
  for(i=m;i<neq;i+=4)
  {
    r1[i]  =c*r1[i]  +r2[i];
    r1[i+1]=c*r1[i+1]+r2[i+1];
    r1[i+2]=c*r1[i+2]+r2[i+2];
    r1[i+3]=c*r1[i+3]+r2[i+3];
  }

  return 1;
}

int r1_gets_r2_minus_r1(r_vector* r1_ptr, r_vector* r2_ptr)
{
  double *r1,*r2;
  int i,m,neq;

  neq=r1_ptr->rdp->neq;
  if(neq<=0)
    return 0;
  if(neq!=r2_ptr->rdp->neq)
    return 0;

  r1=r1_ptr->vec;
  r2=r2_ptr->vec;

  m=neq%4;

  for(i=0;i<m;i++)
    r1[i]=r2[i]-r1[i];
  for(i=m;i<neq;i+=4)
  {
    r1[i]  =r2[i]  -r1[i];
    r1[i+1]=r2[i+1]-r1[i+1];
    r1[i+2]=r2[i+2]-r1[i+2];
    r1[i+3]=r2[i+3]-r1[i+3];
  }

  return 1;
}

int r1_gets_r1_minus_r2(r_vector* r1_ptr, r_vector* r2_ptr)
{
  double *r1,*r2;
  int i,m,neq;

  neq=r1_ptr->rdp->neq;
  if(neq<=0)
    return 0;
  if(neq!=r2_ptr->rdp->neq)
    return 0;

  r1=r1_ptr->vec;
  r2=r2_ptr->vec;

  m=neq%4;

  for(i=0;i<m;i++)
    r1[i]-=r2[i];
  for(i=m;i<neq;i+=4)
  {
    r1[i]  -=r2[i];
    r1[i+1]-=r2[i+1];
    r1[i+2]-=r2[i+2];
    r1[i+3]-=r2[i+3];
  }

  return 1;
}

int r1_gets_r2_plus_r1(r_vector* r1_ptr, r_vector* r2_ptr)
{
  double *r1,*r2;
  int i,m,neq;

  neq=r1_ptr->rdp->neq;
  if(neq<=0)
    return 0;
  if(neq!=r2_ptr->rdp->neq)
    return 0;

  r1=r1_ptr->vec;
  r2=r2_ptr->vec;

  m=neq%4;

  for(i=0;i<m;i++)
    r1[i]+=r2[i];
  for(i=m;i<neq;i+=4)
  {
    r1[i]  +=r2[i];
    r1[i+1]+=r2[i+1];
    r1[i+2]+=r2[i+2];
    r1[i+3]+=r2[i+3];
  }

  return 1;
}

int r1_gets_r1_plus_r2(r_vector* r1_ptr, r_vector* r2_ptr)
{
  double *r1,*r2;
  int i,m,neq;

  neq=r1_ptr->rdp->neq;
  if(neq<=0)
    return 0;
  if(neq!=r2_ptr->rdp->neq)
    return 0;

  r1=r1_ptr->vec;
  r2=r2_ptr->vec;

  m=neq%4;

  for(i=0;i<m;i++)
    r1[i]+=r2[i];
  for(i=m;i<neq;i+=4)
  {
    r1[i]  +=r2[i];
    r1[i+1]+=r2[i+1];
    r1[i+2]+=r2[i+2];
    r1[i+3]+=r2[i+3];
  }

  return 1;
}

int r_copy(r_vector* r1_ptr, r_vector* r2_ptr)
{
  double *r1,*r2;
  int i,m,neq;

  neq=r1_ptr->rdp->neq;
  if(neq<=0)
    return 0;
  if(neq!=r2_ptr->rdp->neq)
    return 0;

  r1=r1_ptr->vec;
  r2=r2_ptr->vec;

  m=neq%4;

  for(i=0;i<m;i++)
    r1[i]=r2[i];
  for(i=m;i<neq;i+=4)
  {
    r1[i]  =r2[i];
    r1[i+1]=r2[i+1];
    r1[i+2]=r2[i+2];
    r1[i+3]=r2[i+3];
  }

  return 1;
}

/* Multiply vector by diagonal matrix */
int r1_gets_r2_times_r3(r_vector* r1_ptr, r_vector* r2_ptr, void* D_ptr)
{
  r_vector* r3_ptr=(r_vector*)D_ptr;
  double *r1,*r2,*r3;
  int i,m,neq;

  neq=r1_ptr->rdp->neq;
  if(neq<=0)
    return 0;
  if(neq!=r2_ptr->rdp->neq)
    return 0;
  if(neq!=r3_ptr->rdp->neq)
    return 0;

  r1=r1_ptr->vec;
  r2=r2_ptr->vec;
  r3=r3_ptr->vec;

  m=neq%4;

  for(i=0;i<m;i++)
    r1[i]=r2[i]*r3[i];
  for(i=m;i<neq;i+=4)
  {
    r1[i]  =r2[i]  *r3[i];
    r1[i+1]=r2[i+1]*r3[i+1];
    r1[i+2]=r2[i+2]*r3[i+2];
    r1[i+3]=r2[i+3]*r3[i+3];
  }

  return 1;
}

/* Permute vector */
int r_perm(r_vector* r1_ptr,r_vector* r2_ptr,int* perm)
{
  int i;
  int neq;

  double *r1,*r2;

  r1= r1_ptr->vec;
  r2= r2_ptr->vec;

  neq=r1_ptr->rdp->neq;

  for(i= 0;i<neq;i++)
    r1[i]= r2[perm[i]-1];

  return 1;
}

/*
 * Fill vector with random values between a0 and a1.  If seed is
 * zero, then fill vector with constant a1-a0;
 */
int r_random(r_vector* r_ptr, double a0, double a1, long unsigned seed)
{
  int i;
  int neq;
  double *r;

  r=r_ptr->vec;

  neq=r_ptr->rdp->neq;

  if(seed==0)
    for(i=0;i<neq;i++)
      r[i]=a1-a0;
  else
  {
    srand(seed);
    for(i=0;i<neq;i++)
    {
      r[i]=(a1-a0)*((double) rand()/RAND_MAX)+a0;
    }
  }

  return 1;
}

void r_print(FILE* fp, r_vector* r_ptr)
{
  int neq;
  double *r;
  int i;

  neq=r_ptr->rdp->neq;
  r=r_ptr->vec;

  for(i=0;i<neq;i++)
    fprintf(fp,"%5d %15.5e\n",i,r[i]);

}

/* print cell-centered nodal values */
void p_print(FILE* fp, r_vector* r_ptr)
{
  int l,m,n;
  int lm;
  int i,j,k;
  int jl,klm;
  double *r;

  l=r_ptr->rdp->l;
  m=r_ptr->rdp->m;
  n=r_ptr->rdp->n;

  lm=l*m;

  r=r_ptr->vec;

  klm=0;
  for(k=0;k<n;k++)
  {
    jl=0;
    for(j=0;j<m;j++)
    {
      for(i=0;i<l;i++)
        fprintf(fp,"%8.2e ",r[i+jl+klm]);
      fprintf(fp,"\n");
      jl+=l;
    }
    fprintf(fp,"\n");
    klm+=lm;
  }
}

int r_write(FILE* fp, r_vector* r_ptr)
{
  fwrite((r_data *) r_ptr->rdp,sizeof(r_data),1,fp);
  fwrite((double *) r_ptr->vec,sizeof(double),r_ptr->rdp->neq,fp);
  fflush(fp);

  return 1;
}

double r_max_error(r_vector* r1_ptr, r_vector* r2_ptr)
/*
 * computes max|r1-r2|
 */
{
  int i;
  int neq;
  double* r1,*r2;
  double e,Max_e;

  neq=r1_ptr->rdp->neq;

  r1=r1_ptr->vec;
  r2=r2_ptr->vec;

  Max_e=0.0;
  for(i=0;i<neq;i++)
  {
    e=fabs(r1[i]-r2[i]);
    if(e>Max_e)
      Max_e=e;
  }

  return Max_e;
}

double r_max(r_vector* r_ptr)
/*
 * computes ri s.t. |ri|>=rj for all j.
 */
{
  int i;
  int neq;
  double* r;
  double e,Max_e;

  neq=r_ptr->rdp->neq;
  r=r_ptr->vec;

  Max_e=r[0];
  for(i=0;i<neq;i++)
  {
    e=r[i];
    if(fabs(e)>fabs(Max_e))
      Max_e=e;
  }

  return Max_e;
}

int GEN_assemble(GEN_operator* G_ptr, void* A_ptr,
                 int (*A_eval)(r_vector*,r_vector*,void*),
                 void (*A_free)(void*))
{
  G_ptr->A_ptr=A_ptr;
  G_ptr->A_eval=A_eval;
  G_ptr->A_free=A_free;
  return 1;
}

void GEN_free(GEN_operator* G_ptr)
{
  if(G_ptr->A_free != NULL)
    (*G_ptr->A_free)(G_ptr->A_ptr);
}

int GEN_eval(r_vector* r2_ptr, r_vector* r1_ptr, GEN_operator* G_ptr)
{
  if(G_ptr->A_eval==NULL)
  {
    r_copy(r2_ptr,r1_ptr);
    return 0;
  }
  return (*G_ptr->A_eval)(r2_ptr,r1_ptr,G_ptr->A_ptr);
}

/*
 *  GEN_write assembles the generic operator to a binary file pointed
 *  to by fp.  This is done by repeated evaluation of the coordinate
 *  vector p[i]=1 for i=0,..,neq.
 *
 *  row_data points to the row-space r_data and col_data points to the
 *  column-space r_data.
 */
int GEN_write(FILE* fp, GEN_operator* A_ptr,
              r_data* row_data, r_data* col_data)
{
  r_vector r1,r2;
  int rows,cols;
  int i;

  if(!r_allocate(&r2,row_data))
    return 0;
  if(!r_allocate(&r1,col_data))
    return 0;

  rows=row_data->neq;
  cols=col_data->neq;

  fwrite(&rows,sizeof(int),1,fp);
  fwrite(&cols,sizeof(int),1,fp);

  for(i=0;i<cols;i++)
  {
    r1.vec[i]=1.0;
    GEN_eval(&r2,&r1,A_ptr);
    fwrite(r2.vec,sizeof(double),rows,fp);
    r1.vec[i]=0.0;
    r_zero(&r2);
  }
  fflush(fp);

  r_free(&r1);
  r_free(&r2);

  return 1;
}


