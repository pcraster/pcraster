#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h" 

/* global header (opt.) and fileset's prototypes "" */


/* headers of this app. modules called */ 

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

/* checks confict on set of input and output files
 * CheckFileSets checks if the set of output files
 * contains different names and that if an output
 * file is also in the input file set.
 * returns
 * 
 * 0 if no conflict
 * 
 * 1 if a file is part of the input and output files
 * 
 * 2 if a file is defined twice as output file
 *
 * example
 * .so examples/chkfiles.tr
 */
int CheckFileSets(
	const char **conflictFileName, /* write-only, this file causes the error */
	const char **outputFiles, /* array of output file names */
	int nrOutputFiles,        /* size of outputFiles array */
	const char **inputFiles, /* array of input file names */
	int nrInputFiles)        /* size of inputFiles array */
{
	int i,j;

	if (nrOutputFiles > 1)
	 for(i=0; i < nrOutputFiles; i++)
	  for(j=i+1; j < nrOutputFiles; j++)
            if ( FileNamesEq(outputFiles[i], outputFiles[j]) )
            {
             *conflictFileName = outputFiles[i];
             return 2;
            }
	for(i=0; i < nrOutputFiles; i++)
	 for(j=0; j < nrInputFiles; j++)
           if ( FileNamesEq(outputFiles[i], inputFiles[j]) )
           {
            *conflictFileName = outputFiles[i];
            return 1;
           }
	return 0;
}

#ifdef NEVER
/* a program 
 * usage: program out1 out2 inputFiles
 */
int main(
	int argc,
	char *argv[]
	)
{
	const char *errorFile;
	if (argc < 4)
	{
		(void)fprintf(stderr,"USAGE: program out1 out2 inputFiles\n");
		EXIT(1);
	}

	switch(CheckFileSets(&errorFile, (const char **)(argv+1), 2, 
	                                 (const char **)(argv+3), argc-3))
	{
	 case 0: /* OK */ break;
	 case 1: Error("'%s' is used both as input and output", errorFile);
	         break;
	 case 2: Error("'%s' is used twice as output", errorFile);
	         break;
	}

	EXIT(0);
	return 0;
}
#endif
