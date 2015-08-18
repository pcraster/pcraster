#ifndef ASC2MAP_H
#define ASC2MAP_H

typedef enum ASC_TYPE { ASC_PLAIN=0, ASC_ARCINFO=1,ASC_GENAMAP=2 } ASC_TYPE;

int Asc2Map(
	MAP         *out,		/* write-only output file */
	const char *inputFile,		/* input file */
	const char *mv,        /* missing value */
	int  sepChar,
	ASC_TYPE   t,                   /* special format */ 
	int        header,
	int        rowHeader);

#endif
