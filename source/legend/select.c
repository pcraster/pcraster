#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "csf.h"
#include "misc.h"
#include "currmenu.h"		/* Curr... */

/* apps. of this module called */
#include "select.h"

/*************/
/* EXTERNALS */
/*************/
extern WINDOW *getWin;		/* window to enter data for user */

/**********************/
/* LOCAL DECLARATIONS */
/**********************/
static char separator;		/* separator between id and description */
static char **legItems = NULL;
static CURR_RADIO_SELECT_BOX *legBox;

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */ 
/******************/

static int PrintHeader(
	const char **mapNames,
	int         nrMapNames,
	INT4   l, /* low value, MV_INT4 if all mvs */
	INT4  h)  /* high value. undefined if l == MV_INT4 */

{
	int i, r = 2 + ((nrMapNames > 1) ? 1 : 0); /* nrLines */
	int n,p = 0;
	WINDOW *headWin = newwin(
			r ,               /* nrLines */
			CurrScreenCols(), /* all cols */
			0,0);             /* upper left corner */
	POSTCOND(headWin != NULL);
	werase(headWin);
	PRECOND(nrMapNames > 0);
	Curr_wprintw(headWin,"legend of map: %s  range: ", mapNames[0]);
	if (l == MV_INT4)
	 Curr_wprintw(headWin,"all missing values\n");
	else
	 Curr_wprintw(headWin,"%d to %d\n", l, h);
	if (nrMapNames > 1)
	{
	 Curr_wprintw(headWin, "also update of maps:%n",&p);
	 for (i=1; i < nrMapNames; i++)
	 {
	 	if ((p+strlen(mapNames[i])+6) >= (size_t)CurrScreenCols())
	 	{ 
	 	  Curr_wprintw(headWin," ..etc\n");
	 	  break;
	 	}
	 	Curr_wprintw(headWin," %s%n", mapNames[i],&n);
	 	p += n;
	 }
	 Curr_wprintw(headWin,"\n");
	}
	Curr_wprintw(headWin,"\n"); /* empty line */
	wrefresh(headWin);
	delwin(headWin);
	return r;
}

static void WriteItems(
	const CSF_LEGEND *leg,
	int nrLeg,
	int numLen)
{
	int i;
	sprintf(legItems[0],"%-*s : %s",numLen,"NAME", leg[0].descr); 
	for (i=1; i < nrLeg; i++)
	 sprintf(legItems[i],"%-*d : %s",numLen,leg[i].nr, leg[i].descr); 
}

static int PrintLegend(
	int startY,
	CSF_LEGEND *leg, 
	int nrLeg)
{
	const char *otherMsg="q=Quit; u=UndoLastEdit; ";
	const int otherKeys[] = { 'q' , 'u' };
	int i;
	int colXStart = 3; 
	int prefLen,numLen = 4; /* for NAME */
	int boxCols;
	char buf[64];
	char lastEditStr[CSF_LEGEND_DESCR_SIZE];
	int lastEdit = -1; /* no edits */

	if (nrLeg == 1)
		numLen = 3;
	else
	{
	 sprintf(buf,"%d",leg[1].nr);
	 numLen = MAX(numLen, (int)strlen(buf));
	 sprintf(buf,"%d",leg[nrLeg-1].nr);
	 numLen = MAX(numLen, (int)strlen(buf));
	}

	boxCols = CSF_LEGEND_DESCR_SIZE+numLen + 3 + 1; /* 3 for " : " */
	if (legItems != NULL)
		Free2d((void **)legItems, (size_t)nrLeg);
	legItems = (char **) Malloc2d((size_t)nrLeg,(size_t)boxCols, sizeof(char));
	if (legItems == NULL)
		return 0;
	WriteItems(leg, nrLeg, numLen);
	legBox = CurrInitRadioSelectBox(startY,colXStart, LINES-startY-4, 
			boxCols, (const char **)legItems, nrLeg, 
			otherKeys, ARRAY_SIZE(otherKeys), otherMsg);
	prefLen = numLen +3; /* prefix len */
	if (legBox == NULL)
		return 0;
	i = 0;
	while(i == 0)
	{
	   if (CurrRadioSelectItem(&i,legBox))
	   {
		char orig[CSF_LEGEND_DESCR_SIZE];
		strcpy(orig,leg[i].descr);
		if (CurrGetString(leg[i].descr, CSF_LEGEND_DESCR_SIZE-1,
		  startY+i-legBox->firstItemVis,colXStart+prefLen))
		{
			lastEdit = i;
			strcpy(lastEditStr, orig); 
			WriteItems(leg, nrLeg, numLen);
			if (CurrNewItemsInBox(legBox,
				(const char **)legItems,nrLeg) == NULL)
				return 0;
			CurrRadioIncSelectedItem(legBox);
		}
	   } else switch(i) { /* it's an otherKey */
	    case 'q' : if (lastEdit != -1)
	    	       {
	                 CurrRadioPrintItems(legBox);
			 switch(CurrPromptYesNo("Write modified legend to maps?",
			                        "ResumeEditing"))
			 {
			  case 'y' : return 1;
			  case 'n' : return 2;
			 }
	    	       }
	    	       else
	    	       	 return 2;
	    	       break;
	    case 'u' : if (lastEdit != -1)
	    		{
			 strcpy(leg[lastEdit].descr,lastEditStr); 
			 WriteItems(leg, nrLeg, numLen);
			 if (CurrNewItemsInBox(legBox,
			     (const char**)legItems,nrLeg) == NULL)
				return 0;
	    		}
	               break;
	    default  : PRECOND(FALSE);
	    }
	  i = 0;
	}
	POSTCOND(FALSE);
	return 0; /* never reached */
}

/* Puts main menu at screen and returns when user wants to quit */
extern int Menu(
	CSF_LEGEND *leg,
	int       nrLeg,
	const char **mapNames,
	int         nrMapNames)
{
	int headerLines,  c;
	PRECOND(nrLeg >= 1);
	PRECOND(nrMapNames > 0);

	/* Initialize the screen */
	separator = '.';
	CurrInitScreen();

	headerLines = PrintHeader(mapNames, nrMapNames, 
	              nrLeg == 1 ? MV_INT4 : leg[1].nr, leg[nrLeg-1].nr);
	POSTCOND(headerLines >= 2);

        c = PrintLegend(headerLines, leg, nrLeg);

	if (legItems != NULL)
		Free2d((void **)legItems, (size_t)nrLeg);

	/* Uninitialize the screen, reset graphic mode etc. */
	CurrEndCurses();
	return c;
}
