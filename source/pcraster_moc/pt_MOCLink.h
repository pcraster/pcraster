#ifndef INCLUDED_PT_MOCLINK
#define INCLUDED_PT_MOCLINK

#include "stddefx.h"
#include "geo_rasterspace.h"
#include "calc_modellink.h"



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
  ParticleTracker* d_tracker{nullptr};

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
