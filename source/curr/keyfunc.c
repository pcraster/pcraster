#include "stddefx.h"

/*
 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <stdlib.h>
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

/******************/
/* IMPLEMENTATION */
/******************/

/* Searches for a function that belongs to a given key.
 * The function is searched in an given array of combinations of
 * keys and functions.
 * Returns function that fits key, NULL if no function fits.
 * 
 * EXAMPLES
 * .so examples/findkeyf.tr
 */
KEY_FUNC FindKeyFunc(
	int key,		   /* key, pressed by user */
	const KEY_2_COM *keyFuncs, /* array of keys and functions */
	size_t nrKeys)		   /* number of keys in keyFuncs */
{
	size_t i;

	/* search for function according to key */
	for(i = 0; i < nrKeys; i++)
  	{
  	 	if(key == keyFuncs[i].key)
			return keyFuncs[i].function;
	}

	/* No function belongs to key that was pressed */
	return NULL;
}
/* end of file */
