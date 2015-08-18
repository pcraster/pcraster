
typedef struct INP_MAP {
	MAP   *m; 
	char  type; /* just a 'x','y' or 'v' */
	CSF_VS vs;  /* value scale */
	char  usrFmt[128],valFmt[128],mvStr[128];
	int  cacheRow;
	REAL8 *r; /* cached row */
} INP_MAP;

/* map2col.c */
extern int Map2Col(INP_MAP *maps, const char *outputFile, size_t nrMaps, size_t xcoord, size_t ycoord, const char *mv, const char *separator, BOOL geoEas, BOOL colWise, BOOL printMV, const char *inputColumnFile);
