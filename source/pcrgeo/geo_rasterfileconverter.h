#ifndef INCLUDED_GEO_RASTERFILECONVERTER
#define INCLUDED_GEO_RASTERFILECONVERTER

#include "stddefx.h"

#include <iostream>



namespace com {
  class PathName;
}



namespace geo {


class  BandMap;

//! Convert file raster (grid) format to another format
class RasterFileConverter
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  RasterFileConverter&           operator=           (const RasterFileConverter&);

  //! Copy constructor. NOT IMPLEMENTED.
                   RasterFileConverter               (const RasterFileConverter&);

  BandMap          *d_bandMap{nullptr};

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RasterFileConverter               (const com::PathName& existingFile);

  /* virtual */    ~RasterFileConverter              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void writeAscii(std::ostream& ofs) const;
  void writeAscii(const com::PathName& resultFile) const;
  void writeAscii() const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
