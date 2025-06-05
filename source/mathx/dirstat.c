#include <math.h>

#include "stddefx.h"
#include "mathx.h"

/* Calculate the first four moments of a directional data sample
 * Input and result are in radians and between 0 and 2pi. The sample size must be larger
 * than one.
 */
void DirectionalMoments(double *mean,          /* write-only, mean */
                        double *sd,            /* write-only, standard deviation */
                        double *skew,          /* write-only, skewness */
                        double *kurt,          /* write-only, kurtosis */
                        const double *samples, /* array of n samples, radians */
                        size_t n)              /* sample size */
{
    double tC = NAN;
    double tD = NAN;
    double tS = NAN;
    double tS2 = NAN;
    double tC2 = NAN;
    double m2 = NAN;
    double meanIn = NAN;
    double D = NAN;
    double skewIn = NAN;
    double R = NAN;
    double R2 = NAN;
    size_t i = 0;
    const double *p = NULL;
    double t1 = NAN;
    double t2 = NAN;

    PRECOND(n > 1);

    tC = tS = tD = tC2 = tS2 = 0;

    for (i = 0, p = samples; i < n; p++, i++) {
        tC += cos(*p);
        tS += sin(*p);
        tC2 += cos(*p * 2);
        tS2 += sin(*p * 2);
    }
    tC /= n;
    tS /= n;
    R = sqrt(tC * tC + tS * tS);
    meanIn = atan2(tS / R, tC / R);

    for (i = 0, p = samples; i < n; p++, i++)
        tD += cos(*p - meanIn);

    D = 1 - tD / n;

    tC2 /= n;
    tS2 /= n;
    R2 = sqrt(tC2 * tC2 + tS2 * tS2);
    m2 = atan2(tS2 / R2, tC2 / R2);

    t1 = m2 - 2 * meanIn;
    skewIn = R2 * sin(t1) / D / sqrt(D);

    t2 = (1 - D);
    t2 *= t2;
    t2 *= t2;
    *kurt = ScaleRad((R2 * cos(t1) - t2) / sqr(D));
    *mean = ScaleRad(meanIn);
    *sd = ScaleRad(sqrt(-2 * log(1 - D)));
    *skew = ScaleRad(skewIn);
}

/* Calculate mean of directional data sample
 * Input and result are in radians and between 0 and 2pi.
 * returns mean of sample
 */
double DirectionalMean(const double *samples, /* array of n samples, in radians */
                       size_t n)              /* samples size */
{
    /* this a stripped version of
     * DirectionalMoments
     */
    double tC = NAN;
    double tS = NAN;
    double meanIn = NAN;
    double R = NAN;
    size_t i = 0;
    const double *p = NULL;

    tC = tS = 0;

    for (i = 0, p = samples; i < n; p++, i++) {
        tC += cos(*p);
        tS += sin(*p);
    }
    tC /= n;
    tS /= n;
    R = sqrt(tC * tC + tS * tS);
    meanIn = atan2(tS / R, tC / R);

    return ScaleRad(meanIn);
}
