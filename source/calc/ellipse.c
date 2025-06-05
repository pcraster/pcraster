#include "stddefx.h"


/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include "app.h"    /* appUnitTrue, appOutput */
#include "calc.h"
#include "mathx.h"  /* sqr,sqrt */
#include "misc.h"
#include "table.h"
#include <math.h>
#include <string.h> /* memmove */

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */

/***************/
/* EXTERNALS   */
/***************/

/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/* DATA (LIBRARY_INTERNAL)
 */
typedef struct DATA {
    REAL8 count; /* counted value */
    INT4 value;  /* class value */
} DATA;

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/******************/
/* IMPLEMENTATION */
/******************/


/*
set a 6
set b 6

for { set y [ expr $b +1 ] } { $y >= [ expr -$b-1] } { incr y -1 } {
 for { set x [ expr $a +1 ] } { $x >= [ expr -$a-1] } { incr x -1 } {
  puts -nonewline [ format "%1d " [ expr (double($x*$x)/($a*$a) + double($y*$y)/($b*$b) ) <= 1.0 ]]
 }
 puts ""
}

*/

static double Weight(int pw,    /* half pixel window size */
                     int r,     /* row delta index */
                     int c,     /* column delta index */
                     double bw) /* border weight */
{
    REAL8 w = 1;
    if (bw > 0) { /* determine border weigths */
        if (ABS(r) == pw)
            w *= bw;
        if (ABS(c) == pw)
            w *= bw;
    }
    return w;
}

typedef struct HOR_CUT_LINE { /* horizontal cut line */
    union {
        REAL4 f;
        INT4 i;
    } start, end; /* if start = MV , not yet initialized */
} HOR_CUT_LINE;

static void
Add2Lines(HOR_CUT_LINE *l, int nrLines, int xCeil, REAL8 c, REAL8 s, REAL8 x, REAL8 y)
{
    REAL8 xRot = (x * c) - (y * s);
    REAL8 yRot = (x * s) - (y * c);
    int xInd = ((int)floor(xRot)) + (int)xCeil;
    POSTCOND(xInd >= 0 && xInd < nrLines);
    if (IS_MV_REAL4(&(l[xInd].start.f))) {
        l[xInd].start.f = POSSIBLE_DATA_LOSS(REAL4, yRot);
        l[xInd].end.f = POSSIBLE_DATA_LOSS(REAL4, yRot);
    }
    l[xInd].start.f = POSSIBLE_DATA_LOSS(REAL4, MIN(l[xInd].start.f, yRot));
    l[xInd].end.f = POSSIBLE_DATA_LOSS(REAL4, MAX(l[xInd].end.f, yRot));
    (void)nrLines; // shut up compiler
}

static int BuildCircle(REAL8 radius)
{
    int i = 0;
    int nrLines = 0;
    int xFloor = 0;
    REAL8 xIncr = NAN;
    REAL8 lineStart = NAN;
    REAL8 lineEndIncl = NAN;
    HOR_CUT_LINE *l = NULL;
    PRECOND(radius != 0);
    radius /= (Side() * 2);

    xFloor = (size_t)floor(radius);
    radius *= radius;
    nrLines = (xFloor * 2) + 1;
    l = (HOR_CUT_LINE *)ChkMalloc(sizeof(HOR_CUT_LINE) * nrLines);
    for (i = 0; i < nrLines; i++) {
        /* mark not initialized */
        SET_MV_REAL4(&(l[i].start.f));
    }

    for (xIncr = 0; xIncr <= xFloor; xIncr += 1) {
        REAL8 y = floor(sqrt(radius - sqr(xIncr)));
        Add2Lines(l, nrLines, xFloor, 1, 0, xIncr, y);
        Add2Lines(l, nrLines, xFloor, 1, 0, xIncr, -y);
        Add2Lines(l, nrLines, xFloor, 1, 0, -xIncr, y);
        Add2Lines(l, nrLines, xFloor, 1, 0, -xIncr, -y);
    }
    for (i = 0; i < nrLines; i++) {
        /* mark not initialized */
        if (!IS_MV_REAL4(&(l[i].start.f)))
            break;
    }
    POSTCOND(i < nrLines);
    lineStart = i;
    for (i = nrLines - 1; i >= 0; i--) {
        /* mark not initialized */
        if (!IS_MV_REAL4(&(l[i].start.f)))
            break;
    }
    POSTCOND(i >= 0);
    lineEndIncl = i;

    for (i = (int)lineStart; i <= (int)lineEndIncl; i++) {
        PRECOND(!IS_MV_REAL4(&(l[i].start.f)));
        l[i].start.i = (int)Rint(l[i].start.f);
        l[i].end.i = (int)Rint(l[i].end.f);
    }
    return 1;
}

/* Calculus 9-3 Ellipse
 * consider  (x^2/a^2)+(y^2)/(b^2)=1
 * where (-a,0),(a,0) is the major axis
 * and
 * where (-b,0),(b,0) is the minor axis
 * ->  a > 0
 * ->  b > 0
 */
int EllipseAverage(MAP_REAL8 *average,      /* write-only output average map  */
                   const MAP_REAL8 *val,    /* input value map */
                   const MAP_REAL8 *xmajor, /* input window size map */
                   const MAP_REAL8 *yminor, /* input window size map */
                   const MAP_REAL8 *angle)  /* input window size map */
{
    int r = 0;
    int c = 0;
    int nrRows = 0;
    int nrCols = 0;

    val->SetGetTest(GET_MV_TEST, val);
    xmajor->SetGetTest(GET_MV_TEST, xmajor);
    yminor->SetGetTest(GET_MV_TEST, yminor);
    angle->SetGetTest(GET_MV_TEST, angle);

    nrRows = val->NrRows(val);
    nrCols = val->NrCols(val);

    for (r = 0; r < nrRows; r++)
        for (c = 0; c < nrCols; c++) {
            REAL8 value = NAN;
            REAL8 xmajorV = NAN;
            REAL8 yminorV = NAN;
            REAL8 angleV = NAN;
            if (xmajor->Get(&xmajorV, r, c, xmajor) && yminor->Get(&yminorV, r, c, yminor) &&
                angle->Get(&angleV, r, c, angle)) {
                REAL8 count = 0;
                REAL8 winTotal = 0;
                int rWin = 0;
                int cWin = 0;
                int pw = 0;
                REAL8 bw = NAN; /* border weigth */

                BuildCircle(fabs(xmajorV));
                return 0;
                average->PutMV(r, c, average);

                /* Calculate in window */
                for (rWin = -pw; rWin <= pw; rWin++)
                    for (cWin = -pw; cWin <= pw; cWin++) {
                        if (val->Get(&value, rWin + r, cWin + c, val)) {
                            REAL8 w = Weight(pw, rWin, cWin, bw);
                            winTotal += value * w;
                            count += w;
                        }
                    }
                if (count > 0)
                    average->Put(winTotal / count, r, c, average);
                else
                    average->PutMV(r, c, average);
            } else
                /* MV or winSize <= 0 */
                average->PutMV(r, c, average);
        }
    return 0;
}
