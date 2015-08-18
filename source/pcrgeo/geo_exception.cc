#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_EXCEPTION
#include "geo_exception.h"
#define INCLUDED_GEO_EXCEPTION
#endif



geo::NotA_PCRasterMap::NotA_PCRasterMap(const std::string& fileName) :
  com::FileFormatError(fileName,"file is not a PCRaster map")
{}

geo::NotA_PCRasterMap::~NotA_PCRasterMap()
{}

