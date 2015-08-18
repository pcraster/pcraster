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
#include "app.h"
#include "misc.h"
#include "currmenu.h"		/* Curr... */

/* apps. of this module called */
#include  "mapattr.h"
#include  "mclone.h"
#include  "mboth.h"

/*************/
/* EXTERNALS */
/*************/
extern WINDOW *getWin;		/* window to enter data for user */

/**********************/
/* LOCAL DECLARATIONS */
/**********************/
static CURR_RADIO_SELECT_BOX *attrBox;
static BOOL allowEdit[NR_MENU_ATTRS];

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */ 
/******************/

static int PrintHeader(
	const char *mapName)

{
	int r = 2; /* nrLines */
	WINDOW *headWin = newwin(
			r ,               /* nrLines */
			CurrScreenCols(), /* all cols */
			0,0);             /* upper left corner */
	POSTCOND(headWin != NULL);
	werase(headWin);
	if (currAttr->cloneCreation)
	 Curr_wprintw(headWin,"creation of map: %s\n", mapName);
	else
	 Curr_wprintw(headWin,"change of attributes of map: %s\n", mapName);
	wrefresh(headWin);
	delwin(headWin);
	return r;
}

static void WriteItems(
	char  **attrItems,
	const ATTRIBUTES *attr,
	int nrAttr)
{
	int i;
	for (i=0; i < nrAttr; i++)
	{ const char *allow = allowEdit[i] ? "": " (FIXED)";
	 sprintf(attrItems[i],"%*s : %s%s",itemNamesLen,itemNames[i], 
	 	AttrStr(attr,i), allow); 
	}
}

static void SetAllowEdits(void)
{
	int i;
	for(i = ATTR_nrRows ; i <= ATTR_cellRepr; i++)
		allowEdit[i] = currAttr->cloneCreation;
	for(i = ATTR_projection ; i <= ATTR_gisFileId; i++)
		allowEdit[i] = TRUE;
}

static int PrintLegend(
	int startY,
	ATTRIBUTES *attr, 
	int nrAttr)
{
	char **attrItems;
	const char *otherMsg="q=Quit; u=UndoLastEdit; ";
	const char *endMsg = attr->cloneCreation ?
			 "Create map?" : "Write map attributes?";
	const int otherKeys[] = { 'q' , 'u' };
	int i;
	int colXStart = 3; 
	int prefLen; /* for NAME */
	int boxCols;
	REAL8 lastEditOrig;
	int lastEdit = -1; /* no edits */

	SetAllowEdits();
	boxCols = CurrScreenCols()-6; /* 3 on both sides */
	attrItems = (char **) Malloc2d((size_t)nrAttr,(size_t)boxCols+20, sizeof(char));
	                                   /* ^- save side + 20 */
	if (attrItems == NULL)
		return 0;
	WriteItems(attrItems, attr, nrAttr);
	attrBox = CurrInitRadioSelectBox(startY,colXStart, LINES-startY-4, 
			boxCols, (const char **)attrItems, nrAttr, 
			otherKeys, ARRAY_SIZE(otherKeys), otherMsg);
	if (attrBox == NULL)
		return 0;
	prefLen = itemNamesLen +3; /* prefix len, 3 for " : " */
	i = 0;
	while(i == 0)
	{
	   if (CurrRadioSelectItem(&i,attrBox))
	   {
		REAL8 orig,edit;
		char editBuf[128];
		if ( !allowEdit[i]) 
		 { i =0 ; continue; }
		GetAttrDouble(&orig, attr, i);
		GetAttrDouble(&edit, attr, i);
		if (EditItem(&edit, strcpy(editBuf, AttrStr(attr,i)),i, 
		  startY+i-attrBox->firstItemVis,colXStart+prefLen))
		{
			lastEdit = i;
			lastEditOrig = orig; 
		        SetAttrDouble(attr,&edit, i);
			WriteItems(attrItems,attr, nrAttr);
			if (CurrNewItemsInBox(attrBox,
				(const char **)attrItems,nrAttr) == NULL)
				return 0;
			CurrRadioIncSelectedItem(attrBox);
		}
	   } else switch(i) { /* it's an otherKey */
	    case 'q' : if (lastEdit != -1)
	    	       {
	                 BOOL valid;
	                 CurrRadioPrintItems(attrBox);
			 valid =  (attr->nrRows != MV_UINT4 
			        && attr->nrCols != MV_UINT4);
			 if (!valid)
			 {
			  if (CurrPromptYesNo("No rows or columns set, Abort?",
			           "ResumeEditing") == 'y') return 2;
			 }
			 else switch (CurrPromptYesNo(endMsg,
			           "ResumeEditing")) {
			     case 'y' : return 1;
			     case 'n' : return 2;
			   }
			 break;
	    	       }
	    	       return 2;
	    case 'u' : if (lastEdit != -1)
	    		{
		        SetAttrDouble(attr,&lastEditOrig, lastEdit);
		        if (lastEdit == ATTR_valueScale)
		            attr->cellRepr = AppDefaultCellRepr(attr->valueScale);
			WriteItems(attrItems,attr, nrAttr);
			if (CurrNewItemsInBox(attrBox,
				(const char **)attrItems,nrAttr) == NULL)
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
extern int MakeCloneMenu(
	ATTRIBUTES *new,
	const char *mapName)
{
	int headerLines,  c;

	currAttr = new;
	/* Initialize the screen */
	CurrInitScreen();

	headerLines = PrintHeader(mapName);
	POSTCOND(headerLines >= 2);

        c = PrintLegend(headerLines, new, NR_MENU_ATTRS);

	/* Uninitialize the screen, reset graphic mode etc. */
	CurrEndCurses();
	return c;
}

