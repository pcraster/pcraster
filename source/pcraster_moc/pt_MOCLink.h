#ifndef INCLUDED_PT_MOCLINK
#define INCLUDED_PT_MOCLINK



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

// PCRaster library headers.
#ifndef INCLUDED_CALC_MODELLINK
#include "calc_modellink.h"
#define INCLUDED_CALC_MODELLINK
#endif

// Module headers.



namespace pt {
  // MOCLink declarations.
  class ParticleTracker;
}



namespace pt {



//! ModelLink to Method Of Characteristics partical tracking code.
/*!
  This link adds functionality for particle tracking to PCRaster.

  \todo More docs.
*/
class MOCLink: public calc::ModelLink
{

private:
   //! the rasterspace
   geo::RasterSpace d_rs;

  //! Object for the actual particle tracking.
  ParticleTracker* d_tracker;

  //! Assignment operator. NOT IMPLEMENTED.
  MOCLink&         operator=           (const MOCLink&);

  //! Copy constructor. NOT IMPLEMENTED.
                   MOCLink             (const MOCLink&);

  const std::string& name              () const;

  void             initCheck           (calc::ModelLinkMethodSignature&
                                        signature) const;

  void             initExecute         ( const geo::RasterSpace& clone,
                                        calc::ModelLinkMethodSignature&
                                        signature);

  bool             methodCheck         (const std::string& methodName,
                                        calc::ModelLinkMethodSignature&
                                        signature) const;

  void             methodExecute       (const std::string& methodName,
                                        calc::ModelLinkMethodSignature&
                                        signature);

  void             transportCheck      (calc::ModelLinkMethodSignature&
                                        signature) const;

  void             transportExecute    (calc::ModelLinkMethodSignature&
                                        signature) const;

  void             adjustConcentrationCheck(calc::ModelLinkMethodSignature&
                                        signature) const;

  void             adjustConcentrationExecute(calc::ModelLinkMethodSignature&
                                        signature) const;

protected:

public:

                   MOCLink             ();

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


  /* virtual */    ~MOCLink            ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace pt

#endif
