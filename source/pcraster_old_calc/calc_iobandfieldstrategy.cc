#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_IOBANDFIELDSTRATEGY
#include "calc_iobandfieldstrategy.h"
#define INCLUDED_CALC_IOBANDFIELDSTRATEGY
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_GEO_BANDMAP
#include "geo_bandmap.h"
#define INCLUDED_GEO_BANDMAP
#endif

// Module headers.
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif
#ifndef INCLUDED_CALC_BANDMAP
#include "calc_bandmap.h"
#define INCLUDED_CALC_BANDMAP
#endif
#ifndef INCLUDED_CALC_INPUTSPATIAL
#include "calc_inputspatial.h"
#define INCLUDED_CALC_INPUTSPATIAL
#endif
#ifndef INCLUDED_CALC_STACKINFO
#include "calc_stackinfo.h"
#define INCLUDED_CALC_STACKINFO
#endif
#ifndef INCLUDED_CALC_FILEWRITER
#include "calc_filewriter.h"
#define INCLUDED_CALC_FILEWRITER
#endif
/*
#ifndef INCLUDED_CALC_BANDSTACKREADER
#include "calc_bandstackreader.h"
#define INCLUDED_CALC_BANDSTACKREADER
#endif
*/


/*!
  \file
  This file contains the implementation of the IoBandFieldStrategy class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOBANDFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF IOBANDFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::IoBandFieldStrategy::IoBandFieldStrategy()
{
}

//! dtor
calc::IoBandFieldStrategy::~IoBandFieldStrategy()
{
}

//! return PCRaster strategy type
APP_IO_STRATEGY calc::IoBandFieldStrategy::strategyType() const
{
 return APP_IO_BANDMAP;
}

//! detect correct BAND map
calc::IoFieldStrategy* calc::IoBandFieldStrategy::checkInputMap(
    VS&                vs,
    const std::string& fName)
{
  // load value as band map
  BandMap map(fName);
  vs = map.vs();
  return this;
}

//! return a newly allocated BAND stack reader
/*!
   caller must delete
 */
const calc::StackReader* calc::IoBandFieldStrategy::createStackReader(
    const RunDirectory& /* rd */,
    const std::string&  /* stackName */)
{
  PRECOND(false); // not implemented
  return 0;
}

//! check against clone, if not clone not yet set, set clone to mapFileName
/*! precondition: \a mapFileName is known to be an existing band map
 *  \note implemented just like Esri strategy even though we do not support fall
 *  back to PCRaster/csf format
 */
void calc::IoBandFieldStrategy::checkClone(const std::string& mapFileName)
{
    geo::BandMap map(mapFileName);
    geo::RasterSpace mapRs(map.rasterSpace());

    setAndCheckCommon(mapFileName,mapRs);

    if (!d_rasterSpaceBand.nrRows()) { // not yet initialized
        d_cloneNameBand = mapFileName;
        d_rasterSpaceBand = mapRs;
    }

    if (!(mapRs == d_rasterSpaceBand))
        throwCloneDiffers(d_cloneNameBand,mapFileName);
}

//! return a newly created BandMap
calc::GridMap *calc::IoBandFieldStrategy::createMap(
    const std::string& fileName, VS vs) const
{
  return new BandMap(fileName, rasterSpace(), vs);
}

//! return prefix-ItemNumber name
std::string calc::IoBandFieldStrategy::makeStackItemName(
    const std::string& /* iname */,
    int                /* atTimeStep */) const
{
  POSTCOND(false);
  return "";
}

//! adjust min max for every map part of the map stack
void calc::IoBandFieldStrategy::setStackInfo(const StackInfo& /* s */ ) const
{
  POSTCOND(false);
}

//! return new InputBandMap object
calc::Spatial *calc::IoBandFieldStrategy::newInputMap(const std::string& mapName,VS vs,
    const Compressor& c) const
{
  return new InputSpatial<BandMap>(mapName,vs,c);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
