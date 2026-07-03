#include "geo_exception.h"
#include "com_exception.h"
#include <string>

geo::NotA_PCRasterMap::NotA_PCRasterMap(const std::string &fileName)
    : com::FileFormatError(fileName, "file is not a PCRaster map")
{
}

geo::NotA_PCRasterMap::~NotA_PCRasterMap()
{
}
