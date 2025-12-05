#ifndef INCLUDED_MLDD_MLDDLINK
#define INCLUDED_MLDD_MLDDLINK

#include "stddefx.h"
#include "geo_rasterspace.h"
#include "geo_simpleraster.h"
#include "calc_modellink.h"



namespace mldd {
  // MlddLink declarations.
}



namespace mldd {

class Mldd;

//! Old style ModelLink to multi flow mldd module
class MlddLink: public calc::ModelLink
{

  Mldd *d_mldd;

private:

  typedef calc::ModelLinkMethodSignature Sig;


  //! Assignment operator. NOT IMPLEMENTED.
  MlddLink&         operator=           (const MlddLink&);

  //! Copy constructor. NOT IMPLEMENTED.
                   MlddLink             (const MlddLink&);

  const std::string& name              () const;

protected:

  void             initCheck           (Sig& sig) const;

  void             initExecute         (const geo::RasterSpace& clone,
                                        Sig& sig);

  bool             methodCheck         (const std::string& methodName,
                                        Sig& sig) const;

  void             methodExecute       (const std::string& methodName,
                                        Sig& sig);

  void             upstreamCheck       (Sig& sig) const;
  void             accufluxCheck       (Sig& sig) const;
  void             diffuseCheck        (Sig& sig) const;
  void             setDemCheck         (Sig& sig) const;
  void             getDemCheck         (Sig& sig) const;
  void             addStreamCheck      (Sig& sig) const;
  void             setStreamCheck      (Sig& sig) const;
  void             getStreamCheck      (Sig& sig) const;
  void             getWeightCheck      (Sig& sig) const;
  void             removeStreamCheck   (Sig& sig) const;

  void             upstreamExecute     (Sig& sig) const;
  void             accufluxExecute     (Sig& sig) const;
  void             diffuseExecute      (Sig& sig) const;
  void             setDemExecute       (Sig& sig) const;
  void             getDemExecute       (Sig& sig) const;
  void             addStreamExecute    (Sig& sig) const;
  void             setStreamExecute    (Sig& sig) const;
  void             getStreamExecute    (Sig& sig) const;
  void             getWeightExecute    (Sig& sig) const;
  void             removeStreamExecute (Sig& sig) const;

public:

                   MlddLink             ();

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


  /* virtual */    ~MlddLink            ();

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
