#include "stddefx.h"

/*
 * lddlist.c
 */


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "calc.h"

/* global header (opt.) and lddlist's prototypes "" */
#include "p_calc_list.h"


/* headers of this app. modules called */
#include "calc.h"

/***************/
/* EXTERNALS   */
/***************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/

/* removes first item of the list and replace it by its upstream neighbours
 * returns 0 or 1 in case of a memory error
 */
int ReplaceFirstByUpsNbs(
  NODE **list,         /* modified list, can become NULL if list
                        * has 1 node that has no UNBs.
                        */
  const MAP_UINT1 *ldd)
{
  int r,c,i;
  PRECOND(list != NULL);
  r = (*list)->rowNr;
  c = (*list)->colNr;
  *list = RemFromList(*list);
  FOR_ALL_LDD_NBS(i)
  {
    int   rNB = RNeighbor(r, i);
    int   cNB = CNeighbor(c, i);
    UINT1   nbVal;
    if ( ldd->Get(&nbVal,rNB,cNB,ldd)
        && FlowsTo(nbVal,rNB,cNB,r,c))
       if ( (*list = LinkChkNd(*list, rNB, cNB)) == NULL)
         return 1;
  }
  return 0;
}

/* mark first item as visited and put its upstream neighbours in front as unvisited
 * returns updated or NULL in case of a memory error (list is freed then)
 */
NODE *AddUpsNbsMarkFirst(
  NODE *list,          /* list to append to */
  const MAP_UINT1 *ldd)
{
  int r,c,i;
  PRECOND(list != NULL);
  r = list->rowNr;
  c = list->colNr;
  SET_VISITED(list);
  FOR_ALL_LDD_NBS(i)
  {
    int   rNB = RNeighbor(r, i);
    int   cNB = CNeighbor(c, i);
    UINT1   nbVal;
    if ( ldd->Get(&nbVal,rNB,cNB,ldd)
        && FlowsTo(nbVal,rNB,cNB,r,c))
       if ( (list = LinkChkNd(list, rNB, cNB)) == NULL)
         return NULL;
  }
  return list;
}
