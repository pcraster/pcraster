#include "stddefx.h"
#include "geo_rasterfileconverter.h"
#include "com_exception.h"
#include "com_file.h"
#include "com_csfcell.h"
#include "geo_bandmap.h"

#include <iostream>


/*!
  \file
  This file contains the implementation of the RasterFileConverter class.
*/



//------------------------------------------------------------------------------

/*
namespace geo {

class RasterFileConverterPrivate
{
public:

  RasterFileConverterPrivate()
  {
  }

  ~RasterFileConverterPrivate()
  {
  }

};

} // namespace geo
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERFILECONVERTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTERFILECONVERTER MEMBERS
//------------------------------------------------------------------------------

//!
/*!
 * \param existingFile  existing file, format is "auto-sensed"
 */
geo::RasterFileConverter::RasterFileConverter(const com::PathName& existingFile)
{
  try {
    d_bandMap = new BandMap(existingFile);
  } catch(...) {
    throw com::FileError(existingFile,"unsupported format");
  }
}



geo::RasterFileConverter::~RasterFileConverter()
{
  delete d_bandMap;
}

//! write to \a ofs
void geo::RasterFileConverter::writeAscii(std::ostream& ofs) const
{
  REAL4  val[16000];
  PRECOND(d_bandMap->nrCells()<16000);
  d_bandMap->getCellsAsREAL4(val);
  size_t i=0;

  for(size_t r=0; r < d_bandMap->nrRows(); r++) {
   for(size_t c=0; c < d_bandMap->nrCols(); c++) {
     PRECOND(i < d_bandMap->nrCells());
     if (pcr::isMV(val[i]))
       ofs << "-999";
     else
       ofs << val[i];
     if (c != d_bandMap->nrCols()-1)
       ofs << " ";
     i++;
   }
   ofs << '\n';
  }
}

//! write to std::cout
void geo::RasterFileConverter::writeAscii() const
{
  writeAscii(std::cout);
}

//! write to \a resultFile
void geo::RasterFileConverter::writeAscii(const com::PathName& resultFile) const
{

  std::ofstream fileStream;
  com::open(fileStream,resultFile);
  writeAscii(fileStream);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



