#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"

/* global header (opt.) and test's prototypes "" */
#include "p_calc_list.h"

/* headers of this app. modules called */

/***************/
/* EXTERNALS   */
/***************/

#ifdef DEBUG_DEVELOP

/* maximum number of nodes once allocated in list module of calc lib
 * maximum number of nodes once allocated in list module of calc lib.
 * Only in DEBUG_DEVELOP mode.
 */
 int  maxListNodes = 0;
/* current number of nodes once allocated in list module of calc lib
 * current number of nodes once allocated in list module of calc lib.
 * Only in DEBUG_DEVELOP mode.
 */
 int  currListNodes = 0;

#endif

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/******************/
/* IMPLEMENTATION */
/******************/

/* Reverses a list.
 * The list that is the input will be reversed.
 * Returns pointer to reversed list.
 */
NODE *RevList(NODE *list)  /* read-write list to reverse */
{
  NODE *e[3];

  e[0]  = NULL;
  e[1]  = list;

  while (e[1] != NULL)
  {
    e[2]       = e[1]->prev;
    /* action on e[1] if required */
    e[1]->prev = e[0];
    e[0]       = e[1];
    e[1]       = e[2];
  }
  return(e[0]);
}

NODE *NewNode(
  int rowNr,   /* row of cell for new node */
  int colNr)   /* column of cell for new node */
{
  NODE *n = (NODE *)ChkMalloc(sizeof(NODE));
  if (n != NULL)
  { n->rowNr = rowNr; n->colNr = colNr;
#ifdef DEBUG_DEVELOP
          currListNodes++;
#endif
  }
  return n;
}

/* Puts a new element at the beginning of the NODE list.
 * The input is the old list and the new element that has to be linked,
 * the output is the new list. Deallocates list if new allocation fails.
 * Returns list with new item at beginning or NULL in case of error.
 *
 */
 NODE * LinkToList(  NODE *list,  /* read-write list of cells */
      int rowNr,   /* row of cell to be linked */
      int colNr)   /* column of cell to be linked */
{
  NODE *new = NewNode(rowNr,colNr);
  if (new == NULL)
    return NULL;    /* memory allocation failed */
  new->prev = list;
  list = new;
  POSTCOND(list != NULL);
  return list;
}

/* Deallocates a list, needed when the algorithm did not return 0.
 * The list is deallocated and the empty list is returned.
 * Returns NULL.
 */
 NODE *FreeList(NODE *list)   /* read-write list to deallocate */
 {
   while(list != NULL)
     list = RemFromList(list);
   POSTCOND(list == NULL);
   return list;
 }

/* Links new element to list with its REAL8 value.
 * It receives a row number, a column number a value and the old list.
 * The cell is linked in front of the list. Each cell also has a
 * demVal-field, which contains the value of the linked cell that has
 * to be known.
 * Frees whole list in case of failed allocation.
 * Returns NULL if allocation failed, the new list otherwise.
 *
 */
 NODE *LinkChkReal(
   NODE *list,   /* read-write list */
   int rowNr,  /* row from cell to be linked */
   int colNr,  /* column from cell to be linked */
  REAL8 value)  /* a value to link */
 {
  NODE *tmp;
  tmp = LinkToList(list, rowNr, colNr);
  if(tmp == NULL)      /* allocation failed */
  {
    list = FreeList(list);
    return NULL;
  }
  list = tmp;
  list->val.Real = value;    /* real8 value of cell */
  return(list);
 }

/* Links element and direction to list, used during depth-first search.
 * Because of the random access on the map, the nd-field (next
 * direction) is needed. Each time a neighbor and all its upstream
 * elements are checked, the nd-field is incremented.
 * Frees whole list in case of failed allocation.
 * nd is set to 0, UNVISITED
 * Returns NULL if allocation failed, the new list otherwise.
 *
 */
 NODE *LinkChkNd(
   NODE *list,    /* read-write list */
   int rowNr,  /* row from cell to be linked */
   int colNr)  /* column from cell to be linked */
 {
  NODE *tmp;
  tmp = LinkToList(list, rowNr, colNr);
  if(tmp == NULL)    /* allocation failed */
  {
    list = FreeList(list);
    return NULL;
  }
  list = tmp;
  list->val.visited = FALSE;  /* no directions checked yet */
  return(list);
 }

/* Removes the first element of the list.
 * It has a list as input and it removes the first element of this list.
 * Returns the list without the first element.
 *
 */
NODE *RemFromList(NODE *list)    /* read-write list of cells */
{
  NODE *tmp;
  PRECOND(list != NULL);  /* can not remove from empty list */
  tmp = list;
  list = list->prev;
  Free((NODE *) tmp);

#ifdef DEBUG_DEVELOP
  PRECOND(currListNodes > 0);
  maxListNodes = MAX(maxListNodes, currListNodes);
  currListNodes--;
#endif
  return list;
}

/* Checks whether a cell is already in a list.
 * Inputs are the list, the row number and the
 * column number of the cell that has to be checked.
 * A pointer p to the list is used, to check the whole list.
 * Returns TRUE if cell was already in list, FALSE otherwise.
 *
 */
 BOOL InList(   NODE *list,  /* Read-only list */
     int r,    /* row number cell to check */
     int c)    /* column number from cell to check */
{
  NODE *p;    /* pointer to list of structs */
  p = list;
  while(p != NULL)
  {
    if(p->rowNr == r && p->colNr == c)
    /* element in list same coordinates as cell (r,c) */
      return TRUE;
    p = p->prev;  /* check previous element in list */
  }
  return FALSE;
}
