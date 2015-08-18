#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "csf.h"
#include "app.h"

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */ 
/******************/

/* open a clone file
 * AppOpenClone tests if a clone file is specified and if it's a
 * a valid CSF file. If not an error message is printed. If all
 * tests are passed then the map is opened read-only.
 * returns 
 *  the map pointer or NULL is an error message is printed.
 */ 
MAP *AppOpenClone(
	char **cloneFileName, /* write-only the name of the resulting clone, pointer assignment only */
	const char *cmdLineClone)   /* clone on the command-line, NULL if not given. 
	                             * This one has priority over the global one
	                             */
{
	MAP *clone = NULL;
	/* open the clone map */
	if(cmdLineClone == NULL)
	{       /* check if the global clone is set and use */
		if (appClone == NULL)
		{
			Error("no clone map specified with: --clone CloneName");
	         	return NULL;
		}
		*cloneFileName = appClone;
	}
	else
		*cloneFileName = (char *)cmdLineClone;

	/* Check clone file  */
	switch(FileStat(*cloneFileName)) {
	 case 0: clone = Mopen(*cloneFileName, M_READ);
	         if (clone != NULL)
	               break;
	         if (Merrno != NOT_CSF) /* else fall through */
	         	Mperror(*cloneFileName);
	 case 1: Error("clone map '%s' is not a map",*cloneFileName);
	         return NULL;
	 case 2: Error("clone map '%s' does not exist",*cloneFileName);
	         return NULL;
	}
	return clone;
}
