#ifndef MATHX_H
#define MATHX_H


#ifdef __cplusplus
 extern "C" {
#endif

#include <math.h>

#ifndef M_PI
# define M_PI 3.14159265358979323846264338327
#endif

#ifdef ALPHA_OSF
# ifndef M_2PI
# error "M_2PI expected to be defined on ALPHA_OSF"
# endif
#else
# define M_2PI           ((double)(2*M_PI))
#endif

#define sqr(x)   ((x)*(x))

/* rint.c*/
double Rint(double x); 
double Fdiv(double x, double y);

/* ran.c */
extern double Ran(void);
extern void SetRan(unsigned int seed);
extern void InitRanOnce(void);
extern double GasDev(void);

/* dirstat.c */
extern void DirectionalMoments(double *meanBack, double *sdBack, 
         double *skewBack, double *kurt, const double *samples, size_t n);
extern double DirectionalMean(const double *samples, size_t n);

/* dirconv.c */
extern double ScaleDeg(double x);
extern double ScaleRad(double x);
extern double Rad2Deg(double x);
extern double Deg2Rad(double x);


/* recipes.c */
extern int Ludcmp(double **a, size_t n, size_t *indx, double *d);
extern int Cholesky(const double **a, double **sqrta, size_t n, double eps);
extern double **MltSqrMm(double **result, const double **A, const double **B, size_t n);
extern double *MltSqrMv(double *result, const double **A, const double *V, size_t n);
extern double **TransposeSqr(double **result, const double **A, size_t n);
extern double **NewSqrMatrix(size_t n);
extern void FreeSqrMatrix(double **m, size_t n);





#ifdef __cplusplus
 }
#endif

#endif /* MATHX__H */
