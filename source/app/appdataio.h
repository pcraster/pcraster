#ifndef INCLUDED_APPDATAIO
#define INCLUDED_APPDATAIO

/* stuf to process to data 
 * depending on the option settings
 */

#ifdef __cplusplus
 extern "C" {
#endif

#include "csf.h"

extern REAL4 AppCastREAL4(double newS);

extern int AppRgetRowCol(
	const MAP *m,	/* map handle */
	double x,      	/* x of true co-ordinate */
	double y,      	/* y of true co-ordinate */
	int *row,    	/* write-only row number. */
	int *col);   	/* write-only column number. */

extern int AppRgetCoords(
	const MAP *m,	/* map handle */
	int row,      /* Row number (relates to y position). */
	int col,      /* Column number (relates to x position). */
	double *x,      /* write-only. Returns x of true co-ordinate */
	double *y);      /* write-only. Returns y of true co-ordinate */


extern void AppPutVal(
 	MAP *outputFile,	/* map to write */
 	size_t row,		/* row of pixel to put */
 	size_t col,		/* column of pixel to put */
 	CSF_VS valueScale,	/* value scale */ 
 	double val,		/* value to put */
 	 BOOL mv);		/* true if value is mv */

extern int AppCheckVal(
	const char *v,	
 	CSF_VS valueScale,
 	CSF_CR cellRepr);
extern int AppCheckValNum(
	REAL8 v,		
 	CSF_VS valueScale,
 	CSF_CR cellRepr);

extern BOOL AppIsClassified(CSF_VS vs);

/* apptime.c */
extern void AppDynamicProgress(void);
extern void AppEndDynamicProgress(void);

/* apptxt.c */
extern int AppDetectColumnFile( BOOL *geoEas, const char *fileName, int sepChar);
/* appcr.c */
extern CSF_CR AppDefaultCellRepr(CSF_VS vs);

/* appclone.c */
extern MAP *AppOpenClone(char **cloneFileName, const char *cmdLineClone);

/* inpfile.c */
extern int AppInputTest(const char *inputFile);

/* readcols.c */
#define COL_NRCOLS 4

/* readcols.c */
extern int AppReadColumnFile(REAL8 ***recs, size_t *nrRecs, size_t *nrRecordsRead, size_t *nrMVvalueColumn, size_t *nrMVcoordColumn, BOOL *geoeas, const char *inputFile, const char *mv, CSF_VS vs, CSF_CR cr, const size_t *colNr, int sepChar, BOOL skipMVrecords);
extern int AppReadTimeSeriesFile(REAL8 ***recs, size_t *nrSteps, size_t *nrCols, BOOL *geoeas, const char *inputFile, const char *mv, CSF_VS vs, CSF_CR cr, int sepChar);
extern void AppFreeColumnData(REAL8 **columnData, size_t nrRecs);
extern void AppFreeTimeSeriesData(REAL8 **timeSeriesData, size_t nrRecs);

/* appvers.c */
extern int LimitedVersionCheck(int nrRows, int nrCols, int nrSteps, 
                         int nrLookupRecs, int nrXYZRecs, int nrOps);

void AppLogStart(int argc,  /* actually const */ char **argv); 
void AppLogError(const char *errorMsg);
void AppLogFile(const char *asciiFile);

#ifdef __cplusplus
 }
#endif

#endif /* APP_DATAIO */
