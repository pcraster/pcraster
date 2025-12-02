#include "stddefx.h"
#include "geo_exception.h"

geo::NotA_PCRasterMap::NotA_PCRasterMap(const std::string &fileName)
    : com::FileFormatError(fileName, "file is not a PCRaster map")
{
}

geo::NotA_PCRasterMap::~NotA_PCRasterMap()
{
}
