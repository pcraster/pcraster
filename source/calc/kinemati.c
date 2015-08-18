#include "stddefx.h"

// vim: fileformat=dos
// Others need it

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "mathx.h"
#include "misc.h"
#include "calc.h"
#include "app.h"    /* AppRowProgress */

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */
#include "p_calc_list.h"

/***************/
/* EXTERNALS   */
/***************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

static    double    epsilon=1E-12; /* iteration epsilon */

/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/

/* NOT USED
   sediment budget, once used in Lisem
static double CalcSnew(
    double Qnew,
    double Qin,
    double Qold,
    double q,
    double Sin,
    double Sold,
    double s)
{
    double sum = Qin+Qold+q;
    return( sum == 0 ? 0 : ((Qnew/sum) * (Sin+Sold+s) ) );
}
*/
#define MAX_ITERS 3000

double IterateToQnew(
    double Qin, /* summed Q new in for all sub-cachments */
    double Qold,  /* current discharge */
    double q,
    double alpha,
    double beta,
    double deltaT,
    double deltaX,
    double epsilon)
{
  /* Using Newton-Raphson Method
   */
    typedef long double REAL;
    REAL Qk1;      /* Q at loop k+1 for i+1, j+1 */
    REAL ab_pQ, deltaTX, C;
    int   count;

    REAL Qkx;
    REAL fQkx;
    REAL dfQkx;
    POSTCOND(sizeof(REAL) >= 8);

    /* if no input then output = 0 */
    if ((Qin+Qold+q) == 0)  /* +q CW NEW! */
        return(0);

    /* common terms */
    ab_pQ = alpha*beta*pow(((Qold+Qin)/2),beta-1);
    deltaTX = deltaT/deltaX;
    C = deltaTX*Qin + alpha*pow(Qold,beta) + deltaT*q;

    /*  1. Initial guess Qk1.             */
    /*  2. Evaluate function f at Qkx.    */
    /*  3. Evaluate derivative df at Qkx. */
    /*  4. Check convergence.             */

    /*
     * There's a problem with the first guess of Qkx. fQkx is only defined
     * for Qkx's > 0. Sometimes the first guess results in a Qkx+1 which is
     * negative or 0. In that case we change Qkx+1 to 1e-30. This keeps the
     * convergence loop healthy.
     */
    Qkx   = (deltaTX * Qin + Qold * ab_pQ + deltaT * q) / (deltaTX + ab_pQ);
    Qkx   = MAX(Qkx, 1e-30); /* added test-case calc::KinematicTest::iterate1 */
    fQkx  = deltaTX * Qkx + alpha * pow(Qkx, beta) - C;   /* Current k */
    dfQkx = deltaTX + alpha * beta * pow(Qkx, beta - 1);  /* Current k */
    Qkx   -= fQkx / dfQkx;                                /* Next k */
    Qkx   = MAX(Qkx, 1e-30);
    count = 0;
    do {
      fQkx  = deltaTX * Qkx + alpha * pow(Qkx, beta) - C;   /* Current k */
      dfQkx = deltaTX + alpha * beta * pow(Qkx, beta - 1);  /* Current k */
      Qkx   -= fQkx / dfQkx;                                /* Next k */
      Qkx   = MAX(Qkx, 1e-30);
      count++;
    } while(fabs(fQkx) > epsilon && count < MAX_ITERS);

#ifdef DEBUG_DEVELOP
    /* Our loop should converge in around 2 steps, otherwise something's
     * wrong.
     */
    /*  test-case calc::KinematicTest::iterate2
     *  is such a case, but values are very low
     * 1e-30 is returned
     */
     if (0 && count == MAX_ITERS) {
      printf("\nfQkx %g Qkx %g\n",(double)fQkx, (double)Qkx);
      printf("Qin %g \n", Qin);
      printf("Qold %g \n", Qold);
      printf("q %g \n", q);
      printf("alpha %g \n",alpha);
      printf("beta %g \n", beta);
      printf("deltaT %g \n", deltaT);
      printf("deltaX %g \n", deltaX);
    }
#endif
    Qk1 = Qkx;
    return(MAX(Qk1,0));
}


/* Accumulates the amount of the current cell and its neighbors.
 */
static void Sum(
    int r,            /* row current cell */
    int c,            /* column current cell */
    MAP_REAL8 *Qnew,
    const MAP_REAL8 *Qold,
    const MAP_REAL8 *q,
         const MAP_UINT1 *ldd,        /* ldd map  */
    const MAP_REAL8 *alpha,
         const MAP_REAL8 *beta,
     const MAP_REAL8    *deltaT,
     const MAP_REAL8    *deltaX)
{
    REAL8     QoldVal, qVal, alphaVal, betaVal,deltaTVal,deltaXVal;

    if(
       Qold->Get(&QoldVal, r, c, Qold) &&
       q->Get(&qVal, r, c, q) &&
       alpha->Get(&alphaVal, r, c, alpha) &&
       beta->Get(&betaVal, r, c, beta) &&
       deltaT->Get(&deltaTVal, r, c, deltaT) &&
       deltaX->Get(&deltaXVal, r, c, deltaX))
    {
        REAL8   QnewVal,QnewUps,Qin=0.0;
        int     i;
        UINT1     lddVal;

        /* get Qin = sum of Upstream NB Qnew */
        FOR_ALL_LDD_NBS(i)
        {
            int rNB, cNB;
            rNB = RNeighbor(r, i);
            cNB = CNeighbor(c, i);

            if ( ldd->Get(&lddVal, rNB, cNB, ldd) &&
                FlowsTo(lddVal, rNB, cNB, r, c)   )
            {    /* (r,c) is in map and no MV */
                if(Qnew->Get(&QnewUps, rNB, cNB, Qnew))
                    Qin += QnewUps;
                else
                /* neighbor has MV output value
                 * no need to examine others.
                 */
                {
                     Qnew->PutMV(r, c, Qnew);
                    return;
                }
            }
        }

        QnewVal = IterateToQnew(Qin,QoldVal,qVal,alphaVal,betaVal,
                deltaTVal, deltaXVal, epsilon);
        Qnew->Put(QnewVal, r, c, Qnew);
    }
    else
        Qnew->PutMV(r, c, Qnew);
}

static int KinematicOneCatchment(
      int r,    /* r- Y-coordinate of catchment OutflowPoint */
      int c,    /* r- X-coordinate of catchment OutflowPoint */
    MAP_REAL8   *Qnew, /* -w discharge at time j+1 */
 const  MAP_REAL8   *Qold, /* discharge at time j   */
 const  MAP_REAL8   *q,    /* overland water that flows in channel*/
 const MAP_UINT1    *ldd,
 const MAP_REAL8    *alpha, /* see kinematic wave formula */
 const MAP_REAL8    *beta,
 const MAP_REAL8    *deltaT,
 const MAP_REAL8    *deltaX)
{
    NODE     *list;

    PRECOND(ldd->GetGetTest(ldd) == GET_MV_TEST);

    list = LinkChkNd(NULL, r, c);     /* pit is 1st element */
    if(list == NULL)
        return 1;        /* memory allocation failed */

    while(list  != NULL)
    {
        r = list->rowNr;    /* row of cell to check */
        c = list->colNr;    /* column of cell to check */

        if ( IS_VISITED(list) )
        { /* it's catchment is processed
           * ups NBs contain inflow
           */
          Sum(r, c, Qnew,Qold,q, ldd, alpha, beta, deltaT,deltaX);
          list = RemFromList(list);
        }
        else
        { /* add ups NB cell to process first */
            if ((list = AddUpsNbsMarkFirst(list, ldd)) == NULL)
                return 1; /* memory error */
        }
    }
    return 0;
}

int Kinematic(
    MAP_REAL8   *Qnew, /* -w discharge at time j+1 */
 const MAP_UINT1    *ldd,
 const  MAP_REAL8   *Qold, /* discharge at time j   */
 const  MAP_REAL8   *q,    /* overland water that flows in channel*/
 const MAP_REAL8    *alpha, /* see kinematic wave formula */
 const MAP_REAL8    *beta,
 const MAP_REAL8    *deltaT,
 const MAP_REAL8    *deltaX)
{
/*
 * q has to be:         q(j+1,i+1) + q (j,i+1) / 2
 * alpha has to be:     (n * (b+2*h)^(2/3) / sqrt(S) )^ 0.6
 * beta has to be for manning's equation:  3/5
 */
    int nrRows = ldd->NrRows(ldd);
    int nrCols = ldd->NrCols(ldd);
    UINT1      lddVal;
    int r, c; /* (r,c) becomes co-ordinate of outflowpoint */

    for (r = 0; r < nrRows; r++)
     for (c = 0; c < nrCols; c++)
      if(ldd->Get(&lddVal, r, c, ldd))
      {
        if (lddVal == LDD_PIT) /* found catchment outlet */
          KinematicOneCatchment(r,c,Qnew,Qold,q,ldd,alpha,beta,deltaT,deltaX);
      }
      else
        Qnew->PutMV(r,c,Qnew);
    return 0;
}
