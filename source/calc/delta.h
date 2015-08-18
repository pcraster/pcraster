#ifndef  __DELTA_H__ 
#define  __DELTA_H__ 

extern void CalcDeltaXY(
		double *dzDx, /* write-only, difference in X */
		double *dzDy, /* write-only, difference in Y */
		const MAP_REAL8 *dem,
		int r,		/* row nr. center cell */
		int c);		/* col nr. center cell */
extern void ZevenbergenGrid(REAL8 *Z, const MAP_REAL8 *dem, int r, int c);
#endif
