#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ARGORDER
#include "calc_argorder.h"
#define INCLUDED_CALC_ARGORDER
#endif

// Library headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_LIMITS
#include <limits>
#define INCLUDED_LIMITS
#endif
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif
// Module headers.



/*!
  \file
  This file contains the implementation of the ArgOrderAndAddArea class.
*/

namespace calc {
 namespace detail {
   struct AreaClaimReached {
     typedef std::map<UINT4,ArgOrderIdInfo> State;
     typedef std::vector<size_t>::iterator  CellIter;
     State     state;
     CellIter  posAreaClaimed;
     AreaClaimReached(State const& state_, CellIter posAreaClaimed_):
       state(state_),posAreaClaimed(posAreaClaimed_)
     {
     }
   };
 }
}

namespace calc {

/*!
 * remove non assignable classes if no args are assignable
 * then set result to the correct value
 */
std::vector<ArgOrderIdInfo> ArgOrderAndAddArea::initArgs(
    std::vector<ArgOrderIdInfo> const& args,
    INT4 * result,
    size_t len)
{
  // the exec loop requires to have assignable always
  // here we check if there is something to assign
  std::vector<ArgOrderIdInfo> assignables;
  for(ArgConstIter a=args.begin(); a!=args.end(); ++a) {
    if (a->areaLimit() >= 1)
      assignables.push_back(*a);
  }

  if (!assignables.empty())
    return assignables;

  // else, none are assignable
  // make result all 0 or MV
  for(CellIndex c=0; c < len; ++c) {
   result[c]=0;
   // check if some are MV
   for(ArgConstIter a=args.begin(); a!=args.end(); ++a)
     if (pcr::isMV(a->chance()[c])) {
       pcr::setMV(result[c]);
       break;
     }
   }

   return assignables;
}

/*!
 *  \param args is changed on return
 */
void ArgOrderAndAddArea::argOrderAreaLimited(
 std::vector<ArgOrderIdInfo>& args,
             INT4* result,
             size_t len)
{
 args=initArgs(args,result,len);

 if (args.empty()) {
   // done, result set in initArgs
   return;
 }

 /* index into the chance grids,
    at each iteration the found one and the one started
    from are swapped within cellsToSort
  */
 std::vector<CellIndex>  cellsToSort;
 cellsToSort.reserve(len);

 typedef std::vector<CellIndex>::iterator CellIter;

 // initialize
 //  - MV in input -> MV in result
 //  - non-MV cells are stored in cellsToSort
 //  ! only initialize when we have args
 //    ad 1) would fail miserably otherwise
 if (!args.empty())
  for(CellIndex c=0; c < len; ++c) {
    // check if some are MV
    ArgIter argIter=args.begin();
    for( ; argIter!=args.end(); ++argIter)
      if (pcr::isMV(argIter->chance()[c]))
        break;

    if (argIter == args.end()) {  // ad 1)
      cellsToSort.push_back(c);
    } else {
      // some are MV
      pcr::setMV(result[c]);
    }
 }

 CellIter findStart=cellsToSort.begin();

 while(findStart!=cellsToSort.end()) {
   REAL4    maxValue= -std::numeric_limits<REAL4>::max();
   ArgIter  maxArg=args.end();
   CellIter maxCell=cellsToSort.end();
   for(ArgIter argIter=args.begin(); argIter!=args.end(); ++argIter)
    for(CellIter cellIter=findStart; cellIter != cellsToSort.end(); ++cellIter) 
    {
       // MV's already skipped at initialisation
       if (argIter->chance()[*cellIter] > maxValue) {
         maxArg  =argIter;
         maxValue=argIter->chance()[*cellIter];
         maxCell =cellIter;
       }
    }

   // always something assignable
   PRECOND(maxArg!=args.end());

   result[*maxCell] = maxArg->id();

   // maxCell is now done and should go before the next findStart
   //  swap the cell indices to achieve that
   std::swap(*findStart,*maxCell);
   ++findStart;

   maxArg->incrementAreaAssigned();
   if (maxArg->areaLimit() <= maxArg->areaAssigned()) {
     args.erase(maxArg);
     // all areas exhausted, we are done
     if (args.empty())
       break; // out of while loop
   }
 }
 // if all areas exhausted then we assign the remainder 0
 for( ; findStart != cellsToSort.end(); ++findStart)
   result[*findStart] = 0;

}

void ArgOrderAndAddArea::argOrder(
 std::vector<ArgOrderIdInfo> const& args,
             INT4* result,
             size_t len)
{
 for(size_t c=0; c < len; ++c) {
    pcr::setMV(result[c]);

    REAL4 maxValue = -std::numeric_limits<REAL4>::max();
    ArgConstIter maxIter=args.end();
    for(ArgConstIter a=args.begin(); a!=args.end(); ++a) {
     if (pcr::isMV(a->chance()[c])) {
       // done for this cell: one or MV's at input, keep MV in result
       maxIter=args.end();
       break;
     }
     if (a->chance()[c] > maxValue) {
        maxValue = a->chance()[c];
        maxIter  = a;
     }
    }

    if (maxIter != args.end())
      result[c]=maxIter->id();
 }
}


void ArgOrderAndAddArea::argOrderAddAreaLimited(
 std::vector<ArgOrderIdInfo> const& argsIn,
 const INT4 * currentId,
 INT4 * result,
 size_t len)
{
 std::vector<ArgOrderIdInfo> argVector =initArgs(argsIn,result,len);
 typedef std::map<UINT4, ArgOrderIdInfo> ArgMap;
 ArgMap args;
 for(size_t i=0; i < argVector.size(); ++i)
   args.insert(std::make_pair(argVector[i].id(),argVector[i]));

 if (args.empty()) {
   // done, result set in initArgs
   return;
 }

 // index into the chance grids,
 // at each iteration the found one and the one started
 // from are swapped within cellsToSort
 std::vector<CellIndex>  cellsToSort;
 cellsToSort.reserve(len);

 typedef std::vector<CellIndex>::iterator CellIter;

 // initialize
 //  - MV in input -> MV in result
 //  - non-MV cells are stored in cellsToSort
 //  ! only initialize when we have args
 //    ad 1) would fail miserably otherwise
 if (!args.empty())
  for(CellIndex c=0; c < len; ++c) {
    // check if some are MV
    ArgIter argIter=argVector.begin();
    for( ; argIter!=argVector.end(); ++argIter)
      if (pcr::isMV(argIter->chance()[c]))
        break;

    if (argIter == argVector.end() && currentId[c]!=MV_INT4) {  // ad 1)
      cellsToSort.push_back(c);
      result[c] = 0; // initial non-assigned
    } else {
      // some are MV
      pcr::setMV(result[c]);
    }
 }


 size_t cellsTakenPrev=1;
 size_t cellsTakenNow= 0;
 while(cellsTakenPrev!=cellsTakenNow) {
  for(CellIter findStart=cellsToSort.begin(); findStart!=cellsToSort.end();++findStart) {
   REAL4    maxValue= -std::numeric_limits<REAL4>::max();
   ArgOrderIdInfo* maxArg=0;
   CellIter maxCell=cellsToSort.end();
   for(CellIter cellIter=findStart; cellIter != cellsToSort.end(); ++cellIter) {
    for(ArgMap::iterator argIter=args.begin(); argIter!=args.end(); ++argIter) {
       // MV's already skipped at initialisation
       if (argIter->second.chance()[*cellIter] > maxValue) {
        // larger value found
        if (argIter->second.areaLimit() >  argIter->second.areaAssigned()) {
         // areaLimit not yet reached
         maxArg  =&(argIter->second);
         maxValue=argIter->second.chance()[*cellIter];
         maxCell =cellIter;
        }
       }
    }
   } // eofor find max

   // none found, all assigned
   if (maxValue== -std::numeric_limits<REAL4>::max())
     break;

     result[*maxCell] = maxArg->id();
     // do this before swap
     ArgMap::iterator currentIdPtr= args.find(currentId[*maxCell]);

     // maxCell is now done and should go before the next findStart
     //  swap the cell indices to achieve that
     std::swap(*findStart,*maxCell);
     maxArg->incrementAreaAssigned();

     if(currentIdPtr != args.end()) {
       // will overwrite an id with claims
       currentIdPtr->second.incrementAreaTaken();
     }

   } // eofor
   cellsTakenPrev=cellsTakenNow;
   cellsTakenNow=0;
   // for(CellIndex c=0; c < len; ++c)
   //   std::cerr << result << "[" << c << "]= " << result[c] << "\n";
   for(ArgMap::iterator argIter=args.begin(); argIter!=args.end(); ++argIter) {
     cellsTakenNow+=argIter->second.areaTaken();
     argIter->second.resetForSweep();
   }

 } // eowhile claims
}

} // namespace calc
