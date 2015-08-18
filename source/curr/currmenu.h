#ifndef  CURRMENU_H 
#define  CURRMENU_H

/* libs ext. <>, our ""  */
#include <curses.h>

/*************/
/* EXTERNALS */
/*************/
#define KEY_RETURN 13		/* KEY_ENTER unreliable */
#define CURR_KEY_ENTER 10     /* probed on linux */
#define CURR_KEY_ESC 27		/* not defined (unreliable) */
#define KEY_INSERT 126		

typedef struct CURR_SELECT_BOX{
 	char **items;		/* read-only items to select */
 	int nrItems;		/* nr. items in list */
 	int nrRulesVis;		/* nr. of lines visible in window */
 	int currPosition;	/* position of cursor in items */
 	int firstItemVis;       /* first item visibles */
 	const int *otherKeys;		/* other legal keys */
 	int nrKeys;		/* number of keys */
 	int posX;		/* start X position of window */
 	int posY;		/* start Y position of window */
 	WINDOW *window;		/* window */
 	const char *otherMsg;
}CURR_SELECT_BOX;

typedef CURR_SELECT_BOX CURR_RADIO_SELECT_BOX;

typedef struct CURR_MULTI_SELECT_BOX{
 	BOOL *highlighted;	/* list of highlighted items */
 	CURR_SELECT_BOX *box;	/* select box */
}CURR_MULTI_SELECT_BOX;

/******************/
/* IMPLEMENTATION */ 
/******************/

/* currmenu.c */
extern void ParkCursor(void);
extern void CurrInitScreen(void);
extern void CurrEndCurses(void);
extern int CurrScreenCols(void);
extern int CurrScreenLines(void);
/* libs ext. <>, our ""  */
/* currmenu.c contains a now obsolote
 * redefinition of this one
 * first we implemented wprintw
 * extern int Curr_wprintw(WINDOW *w, const char *fmt, ...);
 */
#define Curr_wprintw wprintw

extern BOOL CurrIsEnterKey(int c);
extern void CurrErrorMenu(const char *s);
extern void CurrQuitMenu(void);

extern int CurrGetRadioSelection(const char **values, 
                        int nrValues, int y, int x);
extern BOOL CurrGetString(char *s, int len, int y, int x);
extern BOOL CurrGetStringCheck(char *input, int len, int y, int x, 
	BOOL (*Test)(const char *s));
extern void CurrFreeRadioSelectBox(CURR_RADIO_SELECT_BOX *b);
extern void CurrFreeMultiSelectBox(CURR_MULTI_SELECT_BOX *b);

extern CURR_SELECT_BOX *CurrNewItemsInBox(
	CURR_RADIO_SELECT_BOX *b,
	const char **items,
	int nrItems);

extern CURR_RADIO_SELECT_BOX *CurrInitRadioSelectBox(
	int beginY,
	int beginX,
	int nrRulesVis,
	int nrColsVis,
	const char **items,
	int nrItems,
	const int *otherKeys,
	size_t nrKeys,
	const char *otherMsg);

extern CURR_MULTI_SELECT_BOX *CurrInitMultiSelectBox(
	int beginY,
	int beginX,
	int nrRules,
	int nrCols,
	const char **items,
	int nrItems,
	const int *otherKeys,
	int nrKeys);

extern BOOL CurrRadioSelectItem(
	int *itemOrKey,
	CURR_RADIO_SELECT_BOX *b);

extern void CurrRadioIncSelectedItem(
	CURR_RADIO_SELECT_BOX *b);
extern void CurrRadioPrintItems(
	CURR_RADIO_SELECT_BOX *b);

int CurrPromptYesNo(const char *q, const char *escMsg);

extern int CurrMultiSelectItem(
	int itemOrKey,
	CURR_MULTI_SELECT_BOX *mBox);

typedef int (*KEY_FUNC)(void *par);

typedef struct KEY_2_COM{
	int key;		/* key that was pressed */
	KEY_FUNC function;	/* function that belongs to it */
}KEY_2_COM;


/* keyfunc.c */
KEY_FUNC FindKeyFunc(
	int key,		   	/* key, pressed by user */
	const KEY_2_COM *keyFuncs, 	/* functions to execute */
	size_t nrKeys);			/* number of keys in keyFuncs */

#endif
