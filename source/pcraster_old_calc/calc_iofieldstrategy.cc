#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#include "calc_iofieldstrategy.h"
#define INCLUDED_CALC_IOFIELDSTRATEGY
#endif

// Library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// PCRaster library headers.
#ifndef INCLUDED_API
#include "api.h"        // BootTestApi
#define INCLUDED_API
#endif


// Module headers.
#ifndef INCLUDED_CALC_RUNDIRECTORY
#include "calc_rundirectory.h"
#define INCLUDED_CALC_RUNDIRECTORY
#endif

#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif
#ifndef INCLUDED_CALC_FIELDMAPINPUTPARAMETER
#include "calc_fieldmapinputparameter.h"
#define INCLUDED_CALC_FIELDMAPINPUTPARAMETER
#endif
#ifndef INCLUDED_CALC_PARSPAR
#include "calc_parspar.h"
#define INCLUDED_CALC_PARSPAR
#endif

#ifndef INCLUDED_CALC_IOCSFFIELDSTRATEGY
#include "calc_iocsffieldstrategy.h"
#define INCLUDED_CALC_IOCSFFIELDSTRATEGY
#endif
#ifndef INCLUDED_CALC_IOESRIFIELDSTRATEGY
#include "calc_ioesrifieldstrategy.h"
#define INCLUDED_CALC_IOESRIFIELDSTRATEGY
#endif
#ifndef INCLUDED_CALC_IOBANDFIELDSTRATEGY
#include "calc_iobandfieldstrategy.h"
#define INCLUDED_CALC_IOBANDFIELDSTRATEGY
#endif

/*!
  \file
  This file contains the implementation of the IoFieldStrategy class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

/*!
 * \brief create the correct IoFieldStrategy related to the global option
 * appIOStrategy
 *
 * Caller must delete.
 */
calc::IoFieldStrategy* calc::IoFieldStrategy::createOnGlobalOption()
{
 switch(appIOstrategy) {
   case APP_IO_ESRIGRID: return new IoEsriFieldStrategy();
   case APP_IO_PCRASTER: return new IoCsfFieldStrategy();
   case APP_IO_BANDMAP:  return new IoBandFieldStrategy();
 }
 PRECOND(false);
 return 0;
}



//------------------------------------------------------------------------------
// DEFINITION OF IOFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::IoFieldStrategy::IoFieldStrategy()
{
}

//! dtor
calc::IoFieldStrategy::~IoFieldStrategy()
{
}

void calc::IoFieldStrategy::setupClone()
{
   setupFormatSpecificClone();

   BootTestApi(d_commonRS.cellSize(), d_commonRS.projection() == geo::YIncrT2B);
}

void calc::IoFieldStrategy::setupFormatSpecificClone()
{
}

//! compare different formats on their common features
void calc::IoFieldStrategy::checkCommonCloneEqual(
    const std::string& mapFileName,
    const geo::RasterSpace& newMap)  const
{
    if (d_commonRS.nrRows()   != newMap.nrRows() ||
        d_commonRS.nrCols()   != newMap.nrCols() ||
        d_commonRS.cellSize() != newMap.cellSize())
         throwCloneDiffers(d_cloneNameCommon,mapFileName);
}

//! throw com::Exception, if stuff differs
void calc::IoFieldStrategy::throwCloneDiffers(
    const std::string& map1,
    const std::string& map2) const
{
    throw com::Exception("location attributes of "+ quote(map1)+
                " and "+quote(map2)+" are different");
}

//! Remove an output object to ensure proper re-creation.
/*! This method ensures that an object with name \a objName can
    can be created after calling this methode.
    In normal Csf output it does not do anything,
    since the map and tss creation routines will simply create single
    files, destroying old ones under that name. 
    \todo
      check for read-only-ness here?
 */
void calc::IoFieldStrategy::removeOutputObject(const std::string& ) const
{
}


//!  inits and verify against the clone of the script
calc::FieldMapInputParameter* calc::IoFieldStrategy::createFieldMapInputParameter(
    const calc::ParsPar &par)
{
    VS vs;
    std::vector<std::string>n;
    n.push_back(par.externalName());
    IoFieldStrategy* s=checkInputMap(vs,n[0]);
    s->checkClone(n[0]);
    return new FieldMapInputParameter(par,false,vs,n,*s);
}

//! set and check against common clone
/*! A map named mapFileName and loc. attributes mapRs
    is set as common clone, if no clone yet set.
    The same map is checked against a possible already set clone.
 */
void calc::IoFieldStrategy::setAndCheckCommon(
    const std::string& mapFileName,
    const geo::RasterSpace& mapRs)
{
    if (!d_commonRS.nrRows()) { // not yet initialized
        d_cloneNameCommon = mapFileName;
        d_commonRS        = mapRs;
    }
    checkCommonCloneEqual(mapFileName,mapRs); // pcrcalc/test289
}

//! overwrite if one needs a format specific filename validator
void calc::IoFieldStrategy::validateFileName(const std::string& ) const
{
}

std::string calc::IoFieldStrategy::pathTimestep1(
    const RunDirectory& rd,
    const std::string&  stackName) const
{
  bool found;
  return rd.inputFilePath(found,makeStackItemName(stackName,1));
}

pcrxml::IoStrategy::EnumType calc::IoFieldStrategy::xmlType() const
{
 switch(strategyType()) {
   case APP_IO_ESRIGRID: return pcrxml::IoStrategy::EsriGrid;
   case APP_IO_PCRASTER: return pcrxml::IoStrategy::PCRaster;
   case APP_IO_BANDMAP:  return pcrxml::IoStrategy::Band;
 }
 PRECOND(false);
 return pcrxml::IoStrategy::PCRaster;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
