#ifndef INCLUDED_CALC_AREAMAP
#define INCLUDED_CALC_AREAMAP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.



namespace pcrxml {
  class CheckContext;
  class AreaMapScript;
  class ComputationMask;
}



namespace calc {

class Field;

//! The area map describing the RasterSpace and a possible mask
/*!
 * AreaMap is configured by:
 *  - the areamap section of a script,
 *  - the --clone setting,
 *  - a geo::RasterSpace or
 *  - a pcrxml::AreaMapScript setting
 *
 *  \todo what is when called is becoming really fuzzy, refactor code flow
 *
 *  Default the AreaMap is not set. If only a RasterSpace is set, the mask
 *  is all 1. If a mask is set but no RasterSpace the Rasterspace default to
 *  a 1 column with nrRows equal to the size of the mask.
 */
class AreaMap
{
public:
  typedef std::vector<bool> Mask;

private:
  //! to check the order crap AreaMap got into
  bool d_setComputationMaskCallAllowed;

  geo::RasterSpace    d_rs;
   //! 0 means MV, 1 means value in mask
  Mask                d_mask;

  pcrxml::AreaMapScript      *d_areaMap;
  pcrxml::ComputationMask    *d_computationMask;


  void             syncMask              ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AreaMap               ();
                   AreaMap               (const geo::RasterSpace& rs);
                   AreaMap               (pcrxml::AreaMapScript const&  areaMap);
  //  Assignment operator.
  AreaMap&         operator=             (AreaMap const& rhs);

  // Copy constructor.
                   AreaMap               (AreaMap const& rhs);

  /* virtual */    ~AreaMap              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             transferMask          (const Field* f);
  void             setRasterSpace        (const geo::RasterSpace& rs);
  void             setMaskOnCoordinates  ();
  void             setComputationMask    (pcrxml::ComputationMask const&  computationMask);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool                      isSet            () const;
  bool                      hasCoordinateMask() const;
  pcrxml::CheckContext*     createXMLContext ()  const;
  const Mask&               mask             () const;
  const geo::RasterSpace&   rasterSpace      () const;
  void                      throwIfNotSet    () const;

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



} // namespace calc

#endif
