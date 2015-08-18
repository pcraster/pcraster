typedef enum HEADER {
	HEAD_NONE=0,
	HEAD_ARCINFO=1,
	HEAD_ROWCOL=2,
	HEAD_COLROW=3
} HEADER;	


/* map2asc.c */
extern int Map2Asc(MAP *inputMap, char *outputFile, const char *mv, char *fmt, const char *sep, int nrCellsOnLine, 
	HEADER head, BOOL colWise);
