/* File: r_vector.h */

#ifndef R_VECTOR_H
#define R_VECTOR_H

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <float.h>

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
 */

typedef struct r_data
{
  int l,m,n;
  int neq;
}r_data;

/*
 * vector of double's
 */
typedef struct r_vector
{
  double* vec;
  r_data* rdp;
}r_vector;

int r_allocate(r_vector* r_ptr, r_data* rdp);
void r_free(r_vector* r_ptr);

/*
 *  Generic operator 
 *
 *  The generic operator contains callback functions 
 *  for evaluating the action of the operator and
 *  for deallocating the object.
 *
 *  If the pointer to the evaluation function is NULL, then
 *  the identity operator is assumed.
 *
 *  If the pointer to the deallocation function is 
 *  NULL, then the function is not called.
 */
typedef struct
{
  void* A_ptr;
  int (*A_eval)(r_vector*,r_vector*,void*);
  void (*A_free)(void*);
}GEN_operator;

int GEN_assemble(GEN_operator* G_ptr, void* A_ptr, 
                 int (*A_eval)(r_vector*,r_vector*,void*),
                 void (*A_free)(void*));
void GEN_free(GEN_operator* G_ptr);
int GEN_eval(r_vector* r2_ptr, r_vector* r1_ptr, GEN_operator* G_ptr);

/*
 * GEN_write assembles the generic operator to a binary file pointed
 * to by fp.  This is done by repeated evaluation of the coordinate
 * vector p[i]=1 for i=0,..,neq.
 *  
 * row_data points to the row-space r_data and col_data points to the
 * column-space r_data.
 *
 */
int GEN_write(FILE* fp, GEN_operator* G_ptr, 
              r_data* row_data, r_data* col_data);

/* 
 * BLAS-like functions and other vector utilities. 
 */
int r1_gets_r2_minus_r3(r_vector* r1_ptr,r_vector* r2_ptr,r_vector* r3_ptr);
int r1_gets_r2_plus_r3(r_vector* r1_ptr,r_vector* r2_ptr,r_vector* r3_ptr);

int r1_gets_r2_minus_r1(r_vector* r1_ptr,r_vector* r2_ptr);
int r1_gets_r1_minus_r2(r_vector* r1_ptr,r_vector* r2_ptr);
int r1_gets_r2_plus_r1(r_vector* r1_ptr,r_vector* r2_ptr);
int r1_gets_r1_plus_cr2(r_vector* r1_ptr,r_vector* r2_ptr,double c);
int r1_gets_cr1_plus_r2(r_vector* r1_ptr,r_vector* r2_ptr, double c);
int r1_gets_cr2_minus_r1(r_vector* r1_ptr,r_vector* r2_ptr, double c);
int r_copy(r_vector* r1_ptr, r_vector* r2_ptr);
int r_scale(r_vector* r1_ptr, double c);
int r_zero(r_vector* r_ptr);
double r_dotprd(r_vector* r1_ptr, r_vector* r2_ptr);
double r_sum(r_vector* r_ptr);

/* 
 * Fill vector with random values between a0 and a1.  If seed is 
 * zero, then fill vector with constant a1-a0;
 */
int r_random(r_vector* r_ptr, double a0, double a1, long unsigned seed);

/* Computes max|r1-r2| */
double r_max_error(r_vector* r1_ptr, r_vector* r2_ptr);

/* compute ri such that |ri|>=rj for all j. */
double r_max(r_vector* r_ptr);

void r_print(FILE* fp, r_vector* r_ptr);

/* Prints cell-centered values */
void p_print(FILE* fp, r_vector* r_ptr);

int r_write(FILE* fp, r_vector* r_ptr);

#endif
