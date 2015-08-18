#include "stddefx.h"

/*
 *
 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <ctype.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include "misc.h"

/* apps. of this module called */
#include "currmenu.h"

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/
#define KEY_RETURN 13

static  WINDOW *msgBox=NULL;
static  char *locBuf=NULL;

/* strings for status bar */
static const char quitStr[] = "Q quit without writing";
static const char backStr[] = "n back to menu";
static const char writeStr[]  = "y write & quit";

/******************/
/* IMPLEMENTATION */
/******************/

/* Fixes KEY_RETURN bug of (n)curses.
 * Returns TRUE if the key is the enter key, FALSE otherwise.
 * 
 */
BOOL CurrIsEnterKey(int key)		/* key to detect */
{
 /* EXAMPLES * .  so examples/currisenter.tr */
	return key == KEY_RETURN || key == KEY_ENTER || key == CURR_KEY_ENTER;
}

static BOOL CurrentVis(
	const CURR_SELECT_BOX *b)   /* read-write radio select box */
{
	PRECOND(b->currPosition >= 0);
	PRECOND(b->currPosition <  b->nrItems);
	PRECOND(b->firstItemVis >= 0);
	PRECOND(b->firstItemVis <= b->nrItems-b->nrRulesVis);
	return b->currPosition >= b->firstItemVis &&
	      (b->currPosition <  (b->firstItemVis + b->nrRulesVis));
}

/* Scrolls one page up if possible.
 * This is done by modifying the given select box
 */
static void PageUp(
	CURR_SELECT_BOX *b)   /* read-write radio select box */
{
        b->firstItemVis = MAX(0, b->firstItemVis-b->nrRulesVis);
	if (! CurrentVis(b))
		b->currPosition = b->firstItemVis+(b->nrRulesVis - 1);
}

/* Scrolls one page down if possible.
 * This is done by modifying the given select box
 */
static void PageDown(
	CURR_SELECT_BOX *b)   /* read-write radio select box */
{
        b->firstItemVis = MIN(b->nrItems     - b->nrRulesVis, 
                              b->firstItemVis+ b->nrRulesVis);
	if (! CurrentVis(b))
		b->currPosition = b->firstItemVis;
}

/* Scrolls one line up if possible
 * This is done by modifying the given select box
 */
static void LineUp(
	CURR_SELECT_BOX *b)   /* read-write select box */
{
	if(b->currPosition > 0)
	{
		b->currPosition--;
	/*
		if (b->firstItemVis > b->currPosition)
	 */
	 	if (! CurrentVis(b))
		   b->firstItemVis = MAX(0,b->currPosition-b->nrRulesVis+1);
	}
}

/* Scrolls one line down if possible
 * This is done by modifying the given select box.
 */
static void LineDown(
	CURR_SELECT_BOX *b)   /* read-write select box */
{
	if(b->currPosition < (b->nrItems-1))
	{
		b->currPosition++;
	 	if (! CurrentVis(b))
		 b->firstItemVis = MIN(b->nrItems-b->nrRulesVis,
		                       b->currPosition);
	}
}


/* Makes whole window empty.
 */
static void CurrEmptyWindow(WINDOW *w)	/* window to erase */
{
	if(w != NULL)
	{
		werase(w);
		wrefresh(w);
	}
}

/* Makes whole standard screen empty.
 */
static void CurrEmptyScreen(void)
{
	erase();
	refresh();
}

/* Puts new items in select box.
 * cursor remains at same position.
 * Returns NULL in case of an error, pointer to box otherwise.
 * 
 * EXAMPLES
 * .SO examples/currnewitems.tr
 */
CURR_SELECT_BOX *CurrNewItemsInBox(
	CURR_RADIO_SELECT_BOX *b,   /* read-write radio select box */
	const char **items,	      /* items to be put in box */
	int nrItems)		      /* number of items */
{
	int i;

	PRECOND(nrItems > 0);
	PRECOND(b != NULL);

	if(b->items != NULL)
	{
		for(i = 0; i < b->nrItems; i++)
			Free(b->items[i]);
		Free(b->items);
	}

	b->nrItems = nrItems;
	if((b->items = ChkMalloc(sizeof(char *) * nrItems)) == NULL)
		return NULL;
	for(i = 0; i < b->nrItems; i++)
	{
		b->items[i] = StrcpyChkMalloc(items[i]);
		if(b->items[i] == NULL)
			return NULL;
	}
	return b;
}

/* Initializes the screen for the items that can be selected.
 * Creates the select box.
 * Returns NULL in case of an error, pointer to new box otherwise
 */
static CURR_SELECT_BOX *CurrInitSelectBox(
	int beginY,		/* start y-coordinate */
	int beginX,		/* start x-coordinate */
	int nrRulesVis,  	/* number of rules visible in the window */
	int nrColsVis,		/* number of cols visible in the window */
 	const char **items,		/* items to select */
 	int nrItems,		/* number of items */
 	const int *otherKeys,	/* other legal keys */
 	int nrKeys,		/* number of other legal keys */
 	const char *otherMsg)
{
	CURR_SELECT_BOX *b;

	/* window should fit in screen */
	PRECOND(0 <= beginY && beginY + nrRulesVis <= LINES);
	PRECOND(0 <= beginX && beginX + nrColsVis <= COLS);

	if((b = ChkMalloc(sizeof(CURR_SELECT_BOX))) == NULL)
		return NULL;

	/* fill box with parameters of window */
	b->posY = beginY;		/* start y_position of window */
	b->posX = beginX;		/* start X-position of window */
	b->nrRulesVis = MIN(nrRulesVis,nrItems);		
	                               /* number of rules in window */
	b->items = NULL;
	b->nrItems = 0;
	b->otherMsg = otherMsg;

	/* define and initialize the window */
	b->window = newwin(nrRulesVis, nrColsVis, beginY, beginX);
	werase(b->window);

	/* put keys in box */
	b->nrKeys = nrKeys;
	if (b->nrKeys == 0)
	 b->otherKeys = NULL;
	else
	 if ((b->otherKeys = MemcpyChkMalloc(otherKeys, 
	            sizeof(int) * nrKeys)) == NULL)
		return NULL;

	/* put items in box */
	if (CurrNewItemsInBox(b, items, nrItems) == NULL)
	{
		Free(b);
		return NULL;
	}

	/* initialize the current position of the cursor on screen */
	b->currPosition = 0;
	b->firstItemVis = 0;
	return b;
}

/* Deletes and deallocates a select box.
 */
static void CurrFreeSelectBox(
	CURR_SELECT_BOX *b)	/* write and free select box */
{
	Free((void *)b->items);
	Free((void *)b->otherKeys);
	Free((void *)b);
}

/* Deletes and deallocates a radio select box.
 * 
 */
void CurrFreeRadioSelectBox(
	CURR_RADIO_SELECT_BOX *b)  /* write and free radio select box */
{
 /* EXAMPLES * .  so examples/currradio.tr */
	CurrFreeSelectBox(b);
}

/* Deletes and deallocates a multi select box.
 * 
 */
void CurrFreeMultiSelectBox(
	CURR_MULTI_SELECT_BOX *b)  /* write and free multi select box */
{
 /* EXAMPLES  .  so examples/currmulti.tr */
	Free(b->highlighted);
	CurrFreeSelectBox(b->box);
}

/* Initializes a radio box.
 * First item is selected as highlighted item 
 * Returns NULL in case of an error, pointer to radiobox otherwise.
 * 
 */
CURR_RADIO_SELECT_BOX *CurrInitRadioSelectBox(
	int beginY,		/* start y-coordinate */
	int beginX,		/* start x-coordinate */
	int nrRulesVis,  		/* number of rules in the window */
	int nrCols,		/* number of cols in the window */
 	const char **items,		/* items to select */
 	int nrItems,		/* number of items */
 	const int *otherKeys,	/* other legal keys */
 	size_t nrKeys,		/* number of other keys */
 	const char *otherMsg)   /* other message */
{
  /* EXAMPLES * .  so examples/currradio.tr */
	CURR_RADIO_SELECT_BOX *radioBox;
	PRECOND(nrItems > 0);
	/* initialize the select box of the radio select box */
	radioBox = CurrInitSelectBox(beginY, beginX, nrRulesVis,
		nrCols, items, nrItems, otherKeys, nrKeys, otherMsg);
	return radioBox;
}

#ifdef NEVER
/* Initializes a multi select box.
 * First item is selected as highlighted item 
 * Returns NULL in case of an error, pointer to radiobox otherwise.
 * 
 */
CURR_MULTI_SELECT_BOX *CurrInitMultiSelectBox(
	int beginY,		/* start y-coordinate */
	int beginX,		/* start x-coordinate */
	int nrRulesVis,  		/* number of rules in the window */
	int nrCols,		/* number of cols in the window */
 	const char **items,		/* items to select */
 	int nrItems,		/* number of items */
 	const int *otherKeys,	/* other legal keys */
 	int nrKeys)		/* number of other keys */
{
 /* EXAMPLES * .  so examples/currmulti.tr */
	CURR_MULTI_SELECT_BOX *multiBox;
	CURR_SELECT_BOX *b;
	int i;

	PRECOND(nrItems > 0);
	/* initialize the select box of the multi select box */
	b = CurrInitSelectBox(beginY, beginX, nrRulesVis,
		nrCols, items, nrItems, otherKeys, nrKeys, NULL);
	if(b == NULL)
		return NULL;
!!!!!!! mulktibox never initili
	multiBox->box = b;

	/* Put string of highlighted items */
	if((multiBox->highlighted = 
	   ChkMalloc(sizeof(BOOL) * nrItems)) == NULL)
		return NULL;
	for(i = 0; i < nrItems; i++)
		multiBox->highlighted[i] = FALSE;
	return multiBox;
}

#endif

/* Prints rules in window as necessary for multi select box.
 * i.e. highlight at current position and bold at selected items.
 */
static void PrintSelectRules(
	const BOOL *highlighted,     /* array for highlighted items */
	const CURR_SELECT_BOX *b)  /* box to print items from */
{
	int i;

	/* Clear window */
	werase(b->window);

	/* the current highlighted item should be put in reverse mode.
	 */
	for(i = b->firstItemVis; i < b->nrRulesVis+b->firstItemVis; i++)
	{
		/* highlight items that were selected (bold) */
		if(highlighted != NULL && highlighted[i]) /* CW ? i */
			wattrset(b->window, A_BOLD);

		/* reverse item at position of cursor */
		if(b->currPosition == i)
		{
			wattrset(b->window, A_REVERSE);
			/* c = i; */
		}

		Curr_wprintw(b->window,"%s\n", b->items[i]);
		wattrset(b->window, A_NORMAL);
	}
      /* does only hold if we are selecting
	POSTCOND(c - b->firstItemVis >= 0 && (c-b->firstItemVis) < b->nrRulesVis);
       */
	wrefresh(b->window);
        ParkCursor();

	/* display all modifications to window */
}

void CurrRadioPrintItems(
	CURR_SELECT_BOX *b)  /* box to print items from */
{
	int c = b->currPosition;
	b->currPosition = -1;
	PrintSelectRules(NULL,b);
	b->currPosition = c;
}

static void PrintSelectMsgBox(const char *otherMsg)
{
  PRECOND(msgBox != NULL);
  werase(msgBox);

  Curr_wprintw(msgBox," ACTIONS: keyboard-keys=action\n");
  Curr_wprintw(msgBox," Enter=Select; ArrowDown,j=LineDown ArrowUp,k=LineUp;\n");
  if (otherMsg != NULL)
   Curr_wprintw(msgBox," %s\n",otherMsg);
  wrefresh(msgBox);
}

/* Puts radio select window on screen and makes it possible to select item.
 * The bar is scrolled when arrows used, bar is alway visible.
 * Writes item if selected, key otherwise.
 * Returns 1 if item was selected, 0 if key was used.
 */
static BOOL CurrSelectItem(
     int *itemOrKey,		/* write-only selected item or key */
     BOOL *highlighted,		/* to print highlighted */
     CURR_SELECT_BOX *b)  	/* read-write radio select box */
{
	int 			c;
	KEY_FUNC 		f;
	const KEY_2_COM keys[] = {
		{ KEY_NPAGE,	(KEY_FUNC)PageDown},
		{ KEY_PPAGE, 	(KEY_FUNC)PageUp},
		{'j', 		(KEY_FUNC)LineDown},
		{ KEY_SR, 	(KEY_FUNC)LineDown},
		{ KEY_DOWN, 	(KEY_FUNC)LineDown},
		{ 'k', 		(KEY_FUNC)LineUp},
		{KEY_SF, 	(KEY_FUNC)LineUp},
		{KEY_UP, 	(KEY_FUNC)LineUp}
	 /*
 	  *     { 'D' , 	(KEY_FUNC)PageDown},
          *	{ 'U', 		(KEY_FUNC)PageUp}
	  */
	};

	PrintSelectRules(highlighted, b);
	PrintSelectMsgBox(b->otherMsg);

	while((c = getch()))
	{
		int i;
		if(CurrIsEnterKey(c))
			break;		/* enter key used->quit */
		for(i = 0; i < b->nrKeys; i++)
		{
			if(b->otherKeys[i] == c)
			{
				*itemOrKey = c; /* key = c */
				return FALSE;	/* no item selected */
			}
		}

		f = FindKeyFunc(c, keys, ARRAY_SIZE(keys));
		if (f != NULL)
		   f(b);		/* perform scrolling */
	        /* otherwise key not mapped loop again */
#	ifdef NEVER
		if(f == NULL)
		{
			*itemOrKey = c;	/* key = c */
			return FALSE;	/* no item selected */
		}
		else
#	endif
		POSTCOND(CurrentVis(b));

		/* redraw window */
		werase(b->window);
		PrintSelectRules(highlighted, b);
	}
	*itemOrKey = b->currPosition;
	return TRUE;
}

/* Puts radio select window on screen and makes it possible to select item.
 * The bar is scrolled when arrows used, bar is alway visible.
 * Writes item if selected, key otherwise.
 * Returns TRUE if item was selected, FALSE if key was used.
 * 
 */
BOOL CurrRadioSelectItem(
     int *itemOrKey,		/* write-only selected item or key */
     CURR_RADIO_SELECT_BOX *b)  /* read-write radio select box */
{
 /* EXAMPLES * .  so examples/currradio.tr */
 	return(CurrSelectItem(itemOrKey, NULL, b)); 
}

void CurrRadioIncSelectedItem(
     CURR_RADIO_SELECT_BOX *b)  /* read-write radio select box */
{
 	LineDown(b); 
 	if (b->nrRulesVis > 1 && b->firstItemVis == b->currPosition
 	     && b->firstItemVis > 0)
 	     b->firstItemVis--;
}


/* HighLights a selected item.
 * Returns NULL in case of an error, pointer to select box otherwise.
 */
static void HighLight(CURR_MULTI_SELECT_BOX *b)	/* read-write box */
{
	b->highlighted[b->box->currPosition] = 
	!b->highlighted[b->box->currPosition];
}

/* Puts multi select window on screen and lets user select item.
 * The bar is scrolled when arrows used, bar is always visible.
 * Toggles highlighting when enter key pressed at position cursor.
 * Returns key that was pressed.
 * 
 */
int CurrMultiSelectItem(
     int itemOrKey,		   /* write-only selected item or key */
     CURR_MULTI_SELECT_BOX *mBox)  /* read-write multi select box */
{
 /* EXAMPLES * .  so examples/currmulti.tr */
	while(CurrSelectItem(&itemOrKey, mBox->highlighted, mBox->box))
		HighLight(mBox);
	return itemOrKey;
}

/* Prints error message in case of an illegal option at bottom screen.
 * Waits until user pressed key again.
 * 
 */
void CurrErrorMenu(const char *s)	/* error message string to print */
{
 /* EXAMPLES * .  so examples/currerror.tr */
	WINDOW *bottomLines = newwin(2, COLS, LINES - 2, 0);

	/* make all of screen empty */
	CurrEmptyScreen();

	/* Print message */
	werase(bottomLines);
	Curr_wprintw(bottomLines, "%s\n", s);
	Curr_wprintw(bottomLines, "press a key to continue...");
	wrefresh(bottomLines);

	/* wait for key to be pressed */
	getch();
	werase(bottomLines);
}

/* Erases screen and gives warning that quit option is used.
 * 
 */
void CurrQuitMenu(void)	
{
 /* EXAMPLES * .  so examples/currquit.tr */
	WINDOW *bottomLines = newwin(2, COLS, LINES - 2, 0);
	werase(bottomLines);
	wattrset(bottomLines, 0);
	Curr_wprintw(bottomLines, "Are you sure ?\n");
	wattrset(bottomLines, A_REVERSE);
	Curr_wprintw(bottomLines, "%s, %s, %s", backStr, writeStr, quitStr);
	wattrset(bottomLines, 0);
	wrefresh(bottomLines);
}

#ifdef NEVER
/* print variable arg list to window
 */
int Curr_wprintw(WINDOW *w, const char *fmt, ...)
{
 /* emunlation of
  * extern int wprintw(WINDOW *,char *,...)
  */
    va_list marker;
    int r;

    va_start( marker,fmt);
    r = vwprintw(w,fmt,marker);
    va_end( marker );
    return r;
}
#endif

void ParkCursor(void)
{
	wmove(msgBox,2,0);
	wrefresh(msgBox);
}

int CurrScreenLines(void)
{
	return LINES;
}

int CurrScreenCols(void)
{
	return COLS;
}

/* Initializes the screen and uses the initial settings
 * Start curses screen management.
 * Startup sequence is initscr, nonl, cbreak, keypad, noecho refresh.
 * Ending is done with CurrEndCurses.
 *
 */
void CurrInitScreen(void)
{
 /* EXAMPLES * so examples/currisenter.tr DISBALED */
	initscr();
/*    	nonl();			   additional screen optimization */
    	cbreak();		/* return unnecessary */
	keypad(stdscr, (bool)TRUE);	/* for function keys */
    	noecho();
    	refresh();
        msgBox = newwin(3,COLS,LINES-3,0);
        locBuf = (char *)ChkMalloc((size_t)(COLS+1));
}

/* Closes the window, cleans structures etc.
 * 
 */
void CurrEndCurses(void)
{
 /* EXAMPLES * so examples/currisenter.tr (DISABLED) */
 
    	delwin(msgBox);
    	Free(locBuf);
    	clear();
    	refresh();
    	endwin();
}

/* Returns FALSE if c is function key, TRUE otherwise. */
static int IsPrint(int c)	/* key to check */
{
	int strip = 0xFF & c;
	return(isprint(strip) && c != KEY_DC);
}

/* Deletes character out of string at current position */
static void DelChar(
	char *s,		/* read-write string to modify */
	int len,		/* maximal length  of string */
	int col)		/* location of cursor */
{
	int i;
	PRECOND(0 <= col && col < len);

	if(s[col] != '\0')
	{
		for(i = col; i < len - 1; i++)
			*(s + i) = *(s + i + 1);
	}
}

/* Inserts a character in a string at current position
 * Returns new position in string
 */
static void InsertChar(
	char *s,		/* read-write string to modify */
	int len, 		/* length of string */
	int col)		/* location of cursor */
{
	int i;

	PRECOND(0 <= col && col < len);

	/* perform shift of characters on right of cursor */
	for(i = len-1; col < i; i--)
		s[i] = s[i-1];

	/* last character is end of string */
	s[len-1] = '\0';
}

/* Makes string empty.
 */
static void MakeEmpty(
	char *s,		/* read-write string */
	int len)		/* length of string */
{
	int i;
	for(i = 0; i < len; i++)
		s[i] = '\0';
}

static void PadString(
	char *s,
	int len)
{
	int i= strlen(s);
	PRECOND(i <= len);
	for(  ; i < len; i++)
		s[i] = ' ';
	s[i] = '\0';
}

static void PrintGetStringMsgBox(void)
{
  PRECOND(msgBox != NULL);
  werase(msgBox);

  Curr_wprintw(msgBox," ACTIONS: keyboard-keys=action\n");
# ifndef DJGPP
  Curr_wprintw(msgBox," ArrowLeft=GoOneLeft; ArrowRight=GoOneRight; Del,BackSpace=DeleteOneLeft\n");
# else
  Curr_wprintw(msgBox," ArrowLeft=GoOneLeft; ArrowRight=GoOneRight; Del=DeleteCharacter\n");
# endif
  Curr_wprintw(msgBox," Enter=StopEdit; Esc=CancelEdit\n");
  wrefresh(msgBox);
}

static void PrintGetRadioSelection(void)
{
  PRECOND(msgBox != NULL);
  werase(msgBox);

  Curr_wprintw(msgBox," ACTIONS: keyboard-keys=action\n");
  Curr_wprintw(msgBox," ArrowLeft=PrevSelection; ArrowRight=NextSelection;\n");
  Curr_wprintw(msgBox," Enter=StopSelection; Esc=CancelSelection\n");
  wrefresh(msgBox);
}

/* Gets selection from user, untill return is given.
 * Before a return is given, the a number of possible
 * values can be selected 
 * RETURNS selected item or 0 if canceled (0 is
 * also the first item that is initially selected
 */
int CurrGetRadioSelection(
	const char **values,	/* array of possible values
	                         * all non-empty strings!
	                         */
	int nrValues,		/* number of possible values 
	                         */
	int y,			/* y - position to put dialog  */
	int x)			/* x - position put dialog */
{
	int c,i,len = strlen(values[0]);
	WINDOW *inputWin;

	for(i=1; i < nrValues; i++)
	  len = MAX((size_t)len,strlen(values[i]));
	inputWin = newwin(1, len+1, y, x); /* one more to put cursor */
	PRECOND(len != 0); 	/* can not modify an empty string */

	PrintGetRadioSelection();

	/* initialize cursor before beginning of string */
	/* move(y, x); seems that wmove does the job
	 */

	/* Get data from user until return or enter key pressed */
	i = 0;
	c = 'q'; /* init round */
	while(1)
	{
		if(CurrIsEnterKey(c))
			break;
		switch(c)
		{
			case KEY_LEFT:
				i = (i) ? i-1 : nrValues-1;
				break;
			case KEY_RIGHT:
				i = (i == (nrValues-1)) ? 0 : i+1;
				break;
			case CURR_KEY_ESC:
				return 0;
		}
		/* print string in window */
	        wattrset(inputWin, A_REVERSE);
		werase(inputWin);
		Curr_wprintw(inputWin, " %s", values[i]);
		wmove(inputWin, 0, 0);
		wrefresh(inputWin);
		c = getch();
	}
	wattrset(inputWin, 0);
	CurrEmptyWindow(inputWin);
	return i;
}

/* Gets input from user, untill return is given.
 * Before a return is given, the input can be modified using
 * backspace, delete, insert etc.
 * If first key, that user presses is printable, the already
 * existing string is erased.
 * RETURNS TRUE if string is modified, FALSE if edit is canceled
 *
 */
BOOL CurrGetString(
	char *input,		/* read-write string to get */
	int len,		/* num of chars allowed in s
	                         * equals size entry box 
	                         * so input must be at least len+1 chars 
	                         */
	int y,			/* y - position of string to get */
	int x)			/* x - position of string to get */
{
 /* EXAMPLES * so examples/currgetstring.tr (disabled) */
	int c = 0, row, col;
	WINDOW *inputWin = newwin(1, len, y, x);
	char *s=locBuf;
	strcpy(s,input);

	PRECOND(len != 0); 	/* can not modify an empty string */

	PrintGetStringMsgBox();

	/* initialize cursor position at beginning of string */
	row = 0;
	col = 0;
	move(y, x);

	wattrset(inputWin, A_REVERSE);
	werase(inputWin);
	PadString(s,len);
	Curr_wprintw(inputWin, "%s", s);
	wmove(inputWin, row, col);
	wrefresh(inputWin);

	/* Get data from user until return or enter key pressed */
	c = getch();
	if(IsPrint(c))
		MakeEmpty(s, len);
	while(1)
	{
		if(CurrIsEnterKey(c))
			break;
		switch(c)
		{
			case KEY_BACKSPACE:
				if(0 < col)
				{
					col--;
					DelChar(s, len, col);
				}
				break;
			case KEY_LEFT:
				if(0 < col)
					col--;
				break;
			case KEY_RIGHT:
				if(col < (len-1))
					col++;
				break;
			case KEY_DC:
				if(0 <= col && col < len)
					DelChar(s, len, col);
				break;
			case CURR_KEY_ESC:
				return FALSE;
			default:
				if(IsPrint(c) && c != KEY_IC && 
				c != KEY_INSERT && col < (len-1))
				{
				 	InsertChar(s, len, col);
					s[col] = c;
					col++;
				}
				else
					beep();
				break;
		}
		POSTCOND(strlen(s) <= (size_t)len);

		/* print string in window */
		werase(inputWin);
		PadString(s,len);
		Curr_wprintw(inputWin, "%s", s);
		wmove(inputWin, row, col);
		wrefresh(inputWin);
		c = getch();
	}
	/* remove spaces at beginning and end of string */
	s = LeftRightTrim(s);
	wattrset(inputWin, 0);
	CurrEmptyWindow(inputWin);
	strcpy(input,s);
	return TRUE;
}

/* Gets input from user, untill return is given, and verifying input.
 * Before a return is given, the input can be modified using
 * backspace, delete, insert etc.
 * If first key, that user presses is printable, the already
 * existing string is erased.
 * RETURNS TRUE if string is modified, FALSE if edit is canceled
 *    or string is unmodified due to incorrect input
 *
 */
BOOL CurrGetStringCheck(
	char *input,		/* read-write string to get */
	int len,		/* num of chars allowed in s
	                         * equals size entry box 
	                         * so input must at least be len+1 chars 
	                         */
	int y,			/* y - position of string to get */
	int x,			/* x - position of string to get */
	BOOL (*Test)(const char *s))
{
 /* EXAMPLES * so examples/currgetstring.tr (disabled) */
	char *s=ChkMalloc((size_t)(len+1));
	if(s == NULL)
	{
		beep();
		return FALSE;
	}
	strcpy(s,input);
       if (CurrGetString(s,len,y,x))
       {
        if (! Test(s))
        { /* illegal value */
	  WINDOW *inputWin = newwin(1, len, y, x);
          beep();
	  werase(inputWin);
	  PadString(s,len);
	  Curr_wprintw(inputWin, "%s", s);
	  wrefresh(inputWin);
	  Free(s);
	  return FALSE;
	}
	else
	{
		strcpy(input,s);
		Free(s);
		return TRUE;
	}
      }
      Free(s);
      return FALSE;
}

int CurrPromptYesNo(
	const char *q, /* question */
	const char *escMsg) /* message for escape, 
	                     * NULL if ESC is illegal response
	                     */
{
  int i = 0;
  PRECOND(msgBox != NULL);

  werase(msgBox);

 while(i == 0)
 {
  Curr_wprintw(msgBox," ACTIONS: keyboard-keys=action\n");
  if (escMsg != NULL)
   Curr_wprintw(msgBox," y=Yes; n=No; Esc=%s\n", escMsg);
  else
   Curr_wprintw(msgBox," y=yes; n=no;\n");
  Curr_wprintw(msgBox," %s : ",q);
  wrefresh(msgBox);

  switch(getch()) {
   case 'y':
   case 'Y': return 'y';
   case 'n':
   case 'N': return 'n';
   case CURR_KEY_ESC:
             return CURR_KEY_ESC;
  }
  i = 0;
 }
 return i;
}
