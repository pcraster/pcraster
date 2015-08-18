#ifndef  __LIST_H__
#define  __LIST_H__

typedef struct NODE
{
     int rowNr;
     int colNr;
     /* pointer to previous element: */
     struct NODE *prev;
     union {
     BOOL visited;
     REAL8 Real;
     } val;
} NODE;


/* list.c */
#ifdef DEBUG_DEVELOP
 extern int maxListNodes;
 extern int currListNodes;
#endif

extern NODE *RevList(NODE *list);
extern NODE *LinkToList(NODE *list, int rowNr, int colNr);
extern NODE *NewNode(int rowNr, int colNr);
extern NODE *FreeList(NODE *list);
extern NODE *LinkChkReal(NODE *list, int rowNr, int colNr, REAL8 value);
extern NODE *LinkChkNd(NODE *list, int rowNr, int colNr);
extern NODE *RemFromList(NODE *list);
extern BOOL InList(NODE *list, int r, int c);

#define SET_VISITED(l)         (l)->val.visited = TRUE
#define IS_VISITED(l)          ((l)->val.visited)

/* lddlist.c */
extern int ReplaceFirstByUpsNbs(NODE **list, const MAP_UINT1 *ldd);
extern NODE *AddUpsNbsMarkFirst(NODE *list, const MAP_UINT1 *ldd);

#endif /* __LIST_H__*/
