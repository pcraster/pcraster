#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"	/* appUnitTrue, appOutput */
#include "mathx.h"	/* Ran */

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "p_calc_list.h"	/* RemFromList */

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/*
i.o.v. Frank Langeveld
Deze versie van pcrcalc bevat de door jouw gewenste functie: brenner .
De naam is afkomstig van een hier relevante filmklassieker.
De argumenten:
result =  brenner(nrBirds,initCost, friction, maxRange, dispRange, habQuality)
     result   boolean spatial
     nrBirds  scalar spatial
              Bevat het aantal juvenilen dat gaat uitvliegen
     initCost gelijk aan initial cost parameter van spread
     friction gelijk aan friction parameter van spread
     maxRange scalar, maximale afstand een vogel kan afleggen (zie spreadmax hieronder)
     dispRange scalar, range parameter in probability map : prob = 0.1 ** (Distance/dispRange)
     habQual   scalar, habitat quality in  [0,1] schaal

Aannames:
	De vogels kunnen niet worden toegewezen aan een plek waar reeds vogels zijn:
	 - een eerder geplaatst vogeltje
	 - de plaatsen waar nrBirds > 0, hier zitten de ouders immers?
Werking
 Vanuit ieder nest punt  wordt met behulp van spreadmax een afstand kaart dist berekend.
 De probability P is dan P = (0.1**(dist/dispRange) ) * habQual
 Vervolgens wordt er een uniform veld getrokken.
 Daarna worden alle mogelijke bestemmingen geselecteerd door:
  P > uniformMap and dist > 0
 De dist > 0 clausule sluit alle bestemmingen uit verder dan de maxRange afstand (zie spreadmax) en
 de bron cel.
 Uit de mogelijke bestemming wordt een willekeurige gekozen. Als het aantal mogelijke bestemmingen
 nul is dan verdwijnt de vogel.

Door met het verschil tussen maxRange en dispRange te spelen kan je het uitstervings effect door
afstand modelleren. Als de maxRange klein is ten opzichte van dispRange is de kans groter dat ze
nooit een cell bereiken. Het aantal cellen met de waarde 1 in de resultaat kaart is dus altijd 
<= maptotal(nrBirds).

Brenner gebruikt intern de nieuwe een tijdelijke ( naam en precieze werking staat nog niet vast)
functie spreadmax. Syntax is gelijk aan de spread/spreadzone combinatie pluts een extra argument
de maximale afstand. Punten voorbij de maximale afstand krijgen voorlopig de waarde 0 (komt handig
uit in de brenner functie
*/

/******************/
/* IMPLEMENTATION */
/******************/
static int DoBirds(
     int *rTo, int *cTo,
	MAP_INT4   *point,
       MAP_REAL8  *dist,
     MAP_UINT1 *occupied,		/* read-write output map  */
     const MAP_REAL8 *dispRange,    
     const MAP_REAL8 *habQual)          /* habitat quality */
{
     	int 	r, nrRows = point->NrRows(point);
     	int 	c, nrCols = point->NrCols(point);
	double  maxUniform=0;

	*rTo=-1,*cTo=-1;

	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
        {
		REAL8 dispRangeVal;
		REAL8 habVal;
		REAL8 distVal;
		UINT1 occVal;
		if (occupied->Get(&occVal,r,c,occupied) && occVal == 0)
	 { /* unoccupied place */
		  if ( dispRange->Get(&dispRangeVal,r,c,dispRange) &&
                       habQual->Get(&habVal,r,c,habQual) &&
                       dist->Get(&distVal,r,c,dist) &&
                       distVal > 0 ) /* should move and 0 is not reached area */
              {
			REAL8 draw = Ran();
		if (draw <= maxUniform) /* <= eliminates Ran() == 0 */
				continue;
               if (draw > (pow(0.1,distVal/dispRangeVal)*habVal))
				continue;
			*rTo = r;
			*cTo = c;
			maxUniform = draw;
 	     }
	}
       }
	return 0;
}

/* Do the bird fly
 */
int BirdsSpread(
     MAP_UINT1 *occupied,		/* read-write output map  */
     const MAP_REAL8 *nrBirds,		/* scalar type thing */
     const MAP_REAL8 *cost,		/* initial costs */
     const MAP_REAL8 *friction,		/* friction of each cell */
     const MAP_REAL8 *maxRange,         /* maximum range birds can fly */
     const MAP_REAL8 *dispRange,        /* range par in prob formuala  */
     const MAP_REAL8 *habQual)          /* habitat quality [0,1] */
{
          REAL8 nrBirdsVal;
     	int     rTo,cTo;
     	int 	r, nrRows = nrBirds->NrRows(nrBirds);
     	int 	c, nrCols = nrBirds->NrCols(nrBirds);
	MAP_REAL8  *dist = CreateSpatialREAL8(CR_REAL4,(size_t)nrRows,(size_t)nrCols);
	MAP_INT4   *zone = CreateSpatialINT4(CR_INT4  ,(size_t)nrRows,(size_t)nrCols);
	MAP_INT4   *point= CreateSpatialINT4(CR_UINT1, (size_t)nrRows,(size_t)nrCols);

	/* algorithm wants points->Get() and all others to
 	* return FALSE if a value is a missing value
 	*/
	occupied->SetGetTest(GET_MV_TEST, occupied);
	friction->SetGetTest(GET_MV_TEST, friction);
	cost->SetGetTest(GET_MV_TEST, cost);
	nrBirds->SetGetTest(GET_MV_TEST, nrBirds);
	maxRange->SetGetTest(GET_MV_TEST, maxRange);
	dispRange->SetGetTest(GET_MV_TEST, dispRange);
	habQual->SetGetTest(GET_MV_TEST, habQual);
	dist->SetGetTest(GET_MV_TEST, dist);
	zone->SetGetTest(GET_MV_TEST, zone);
	point->SetGetTest(GET_MV_TEST, point);

	/* Fill occupied with 0, if there are no birds
         * 1 if there are birds initially 
	 * will be reset later
         */
	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
       {
          point->Put(0,r,c,point);
          if (nrBirds->Get(&nrBirdsVal,r,c,nrBirds))
		occupied->Put((UINT1)(nrBirdsVal ? 1 : 0), r, c, occupied);
          else
		occupied->PutMV(r, c, occupied);	
        }

	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
          if (nrBirds->Get(&nrBirdsVal,r,c,nrBirds) && nrBirdsVal > 0)
          {
                point->Put(1,r,c,point); /* set point */
     	        if (SpreadMax(dist,zone,point,cost,friction,maxRange))
			return 1;
		while(nrBirdsVal > 0)
		{
                 if (DoBirds(&rTo,&cTo,point, dist,occupied, dispRange, habQual))
			return 1;
                 nrBirdsVal-=1;
	         if (rTo >= 0) /* reached point, died otherwise */
                 	occupied->Put(1,rTo,cTo,occupied);
                 point->Put(0,r,c,point); /* undo set point */
		}
          }

	DeleteMAP_REAL8(dist);
	DeleteMAP_INT4(zone);
	DeleteMAP_INT4(point);

	/* reset occupied for initial birds
         */
	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
          if (nrBirds->Get(&nrBirdsVal,r,c,nrBirds) && nrBirdsVal > 0)
		occupied->Put(0, r, c, occupied);

	return 0;		/* successful terminated */ 
}
