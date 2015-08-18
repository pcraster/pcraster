#ifndef ARCGRID_H

typedef struct ARC_INFO_GRID_ASCII
{
	size_t nrCols, nrRows;
	int xCorner, yCorner; /* TRUE/FALSE if x/y or corner or centre */
	int mvGiven;          /* TRUE/FALSE if NODATA_value is in file */
	double xLL, yLL;
	double cellSize, mv;
} ARC_INFO_GRID_ASCII;

/* arcgrid.c */
extern int ReadArcInfoGridAsciiHeader(ARC_INFO_GRID_ASCII *a, FILE *f);
extern int ReadGenamapAuditHeader(size_t *nrRows, size_t *nrCols, FILE *f);

#endif 
