#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "calc.h"
#include "app.h"
#include "misc.h"
#include "mathx.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */

/***************/
/* EXTERNALS   */
/***************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

typedef struct TT_CELL {
  int r,c;
  int nextLddDir;
  struct TT_CELL *prev;
}TT_CELL;

/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/

/* make new cell at head of
 * list and return the new
 * list
 */
static TT_CELL *NewCell(
  int r,
  int c,
  TT_CELL *list)
{
  TT_CELL *cell = (TT_CELL *)ChkMalloc(sizeof(TT_CELL));
  cell->r = r;
  cell->c = c;
  cell->nextLddDir=1;
  /* add new cell to head of list */
  cell->prev=list;
  /* return new cell */
  return cell;
}

/* remove the cell at the top
 * head of the list and return
 * the new list
 */
static TT_CELL *RemoveCell(
  TT_CELL *list)
{
  TT_CELL *l = list->prev;
  Free(list);
  return l;
}

static void AddAmount(
   MAP_REAL8 *m,
   const TT_CELL   *c,
   double    v)
{
  double addVal;
  m->Get(&addVal,c->r,c->c,m);
  addVal+=v;
  m->Put( addVal,c->r,c->c,m);
}

static double timeToCell(
  const MAP_REAL8 *accumTT,
  const TT_CELL   *c,
  double    startCummTT)
{
  double timeTo;
  accumTT->Get(&timeTo,c->r, c->c,accumTT);
  return startCummTT - timeTo;
}

static int Compute(
     int rPit,      /* row of pit cell */
     int cPit,      /* col of pit cell */
     MAP_REAL8 *state,    /* write-only output map  */
     MAP_REAL8 *flux,    /* write-only output map  */
     MAP_REAL8 *removed,    /* write-only output map  */
     const MAP_UINT1 *ldd,     /* ldd map    */
     const MAP_REAL8 *amount,    /* amount to be transported  */
     const MAP_REAL8 *accumTT,    /* accumTT */
     const MAP_REAL8 *fraction)    /* fraction */
{
  TT_CELL *list;

  list = NewCell(rPit,cPit,NULL);
  if (list == NULL)
    return 1;
  flux->Put(0.0,rPit,cPit,flux);
  state->Put(0.0,rPit,cPit,state);
  removed->Put(0.0,rPit,cPit,removed);

  while(list != NULL)
  {
    int r = list->r;
    int c = list->c;

    if (list->nextLddDir > 9)
    { /* this criteria ensures that we compute
       * the cell only once
       * route cell amount down, through the
       * list, the list describes the complete
       * path down to the pit
       */
       double matTravelling;
       TT_CELL *current=list; /* XXXXstream cell of deposition point */
       TT_CELL *down=list->prev; /* downstream cell of deposition point */
       double  startCummTT;
       double  timeToCurrent, timeToDown;
       amount->Get(&matTravelling,current->r,current->c,amount);
       if (matTravelling <= 0)
          goto done;
       accumTT->Get(&startCummTT,current->r,current->c,accumTT);

       while (current) {
          double localFraction, localFlux;

          timeToCurrent = timeToCell(accumTT, current, startCummTT);

          down = current->prev;
          if (down) {
           timeToDown= timeToCell(accumTT, down, startCummTT);
          }

          if (timeToCurrent >= 1.0)
            break;

          if (down && timeToDown >= 1.0)
            break;

          /* else just passing */

          /* TODO better test on MV and not in [0, 1] */
          if (!fraction->Get(&localFraction,current->r,current->c,fraction))
             localFraction = 1;
          localFraction = MIN(MAX(0, localFraction), 1);

          localFlux = localFraction * matTravelling;

          /* add material to flux of all passing cells
           */
          AddAmount(flux,current,localFlux);
          AddAmount(removed,current,matTravelling-localFlux);

          matTravelling = localFlux;

          current = down;
       }

       /* ARRIVAL */

       if (timeToCurrent < 1.0 && down) {
         /* divide: leave some in current*/
         double localFraction;
         double difference = timeToDown-timeToCurrent;
         double currentStateRatio = 1-((1-timeToCurrent)/difference);
         double localFlux;
         double toCurrentStateVal = matTravelling * currentStateRatio;

         AddAmount(state, current, toCurrentStateVal);

         matTravelling -= toCurrentStateVal;

         if (!fraction->Get(&localFraction,current->r,current->c,fraction))
            localFraction = 1;
         localFraction = MIN(MAX(0, localFraction), 1);

         localFlux = localFraction * matTravelling;

         AddAmount(flux,current,localFlux);
         AddAmount(removed,current,matTravelling-localFlux);

         /* then this is what remains for down */
         matTravelling = localFlux;
       }
       if (down!=NULL)
         AddAmount(state, down, matTravelling);
done:
        /* remove the cell */
        list = RemoveCell(list);
    }
    else
    {
     /* add a next incoming route to
      * the list from the current point
      */
      int rNB,cNB;
      UINT1 lddNBval;
      /* find ONE incoming route,
       * where all maps are  defined
       */
      for(   ;list->nextLddDir <= 9; list->nextLddDir++ )
      {
      double a;
      if (list->nextLddDir == LDD_PIT)
        list->nextLddDir++;
      rNB = RNeighbor(r,list->nextLddDir);
      cNB = CNeighbor(c,list->nextLddDir);
      if ( amount->Get(&a,rNB,cNB,amount)  &&
           accumTT->Get(&a,rNB,cNB,amount)  &&
           fraction->Get(&a,rNB,cNB,fraction)  &&
           /* above get's only for MV skipping */
           ldd->Get(&lddNBval,rNB,cNB,ldd) &&
           (10-lddNBval) == list->nextLddDir) /* <- cell flows into this one */
        {
          list->nextLddDir++;
          list = NewCell(rNB,cNB,list);
          if (list == NULL)
            return 1;
          flux->Put(0.0,rNB,cNB,flux);
          state->Put(0.0,rNB,cNB,state);
          removed->Put(0.0,rNB,cNB,removed);
          // if not breaking then go on searching for an inflow cell
          break;
        }
      }
    }
  }
  return 0;
}

int TravelTime(
     MAP_REAL8 *state,        /* write-only output map  */
     MAP_REAL8 *flux,         /* write-only output map  */
     MAP_REAL8 *removed,      /* write-only output map  */
     const MAP_UINT1 *ldd,    /* ldd map    */
     const MAP_REAL8 *amount, /* amount to be transported  */
     const MAP_REAL8 *accumTT,   /* accumTT */
     const MAP_REAL8 *fraction /*fraction in [0, 1] */
     )
{
  REAL8   accumTTVal;
  UINT1   lddVal;
  REAL8   amountVal;
  REAL8   fractionVal;
  int   r, c ,nrRows, nrCols;

  nrRows = ldd->NrRows(ldd);
  nrCols = ldd->NrCols(ldd);

  /* algorithm wants map->Get() to return FALSE in case of a MV */
  state->SetGetTest(GET_MV_TEST, state);
  flux->SetGetTest(GET_MV_TEST, flux);
  ldd->SetGetTest(GET_MV_TEST, ldd);
  amount->SetGetTest(GET_MV_TEST, amount);
  accumTT->SetGetTest(GET_MV_TEST, accumTT);
  fraction->SetGetTest(GET_MV_TEST, fraction);

  /* Fill outIdBuf with 0, this is the initial value */
  for(r = 0; r < nrRows; r++)
   for(c = 0; c < nrCols; c++)
  {
    if(ldd->Get(&lddVal, r, c, ldd)&&
     (amount->Get(&amountVal, r, c, amount))&&
     (accumTT->Get(&accumTTVal, r, c, accumTT))
     && (fraction->Get(&fractionVal, r, c, fraction))
     ) {
      if (lddVal == LDD_PIT)
        Compute(r,c,state,flux,removed,ldd,amount,accumTT ,fraction);
    } else {
         // some input MV so make it MV
         state->PutMV(r, c, state);
         flux->PutMV(r, c, flux);
         removed->PutMV(r, c, removed);
    }
  }
  /* TODO this should not be needed but something goes wrong */
  for(r = 0; r < nrRows; r++)
   for(c = 0; c < nrCols; c++)
  {
    if(!(ldd->Get(&lddVal, r, c, ldd)&&
     (amount->Get(&amountVal, r, c, amount))&&
     (accumTT->Get(&accumTTVal, r, c, accumTT))
     && (fraction->Get(&fractionVal, r, c, fraction)))
     ) {
         state->PutMV(r, c, state);
         flux->PutMV(r, c, flux);
         removed->PutMV(r, c, removed);
    }
  }
  return 0;      /* successful terminated */
}
