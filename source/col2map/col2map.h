#ifndef COL2MAP_H
#define COL2MAP_H

typedef enum COMP_CELL { /* how to compute cell if mutiple records */
		AVERAGE=0,	/* average value */
		DIR_AVERAGE=1,	/* directional average value */
		HIGHEST=2,	/* highest value */
		MAJORITY=3,	/* most occurring value */
		LOWEST=4,	/* lowest value */
		MINORITY=5,	/* least occurring value */
		TOTAL=6,	  /* least occurring value */
		NOTHING=7	/* not set yet */
} COMP_CELL;

extern int Col2Map(MAP *out, const char *inputFile, 
                   COMP_CELL compCell, const char *mv, 
                   const size_t *colNr,
                   int sepChar);

#define POS_X  0
#define POS_Y  1
#define POS_V  2

#endif /* COL2MAP_H */
