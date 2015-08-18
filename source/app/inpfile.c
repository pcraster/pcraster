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

/* test if a file exists and is a regular file
 * AppInputTest tests if a file exists and is a regular file
 * (e.g. not a directory)
 * If not an error message is printed. 
 * returns 
 *  0 if file is OK, 1 if an error message is printed.
 */ 
int AppInputTest(
	const char *inputFile) /* the input file */
{
	/* Check input file  */
	switch(FileStat(inputFile)) {
	 case 1: Error("input file '%s' is not a (regular) file",inputFile);
	         return 1;
	 case 2: Error("input file '%s' does not exist",inputFile);
	         return 1;
	}
	return 0;
}
