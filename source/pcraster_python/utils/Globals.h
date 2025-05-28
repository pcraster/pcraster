#ifndef INCLUDED_PYTHON_UTILS_GLOBALS
#define INCLUDED_PYTHON_UTILS_GLOBALS

#include "dal_Client.h"
#include "geo_rasterspace.h"
#include "pcraster_python_utils_export.h"


namespace dal {
  class RasterDal;
}
namespace calc {
  class RunTimeEngine;
}


namespace pcraster {
namespace python {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class PCRASTER_PYTHON_UTILS_EXPORT Globals: public dal::Client
{

private:

  //! Clone.
  geo::RasterSpace _cloneSpace;

  //! Runtime enginge.
  calc::RunTimeEngine* _rte{nullptr};

  dal::RasterDal* _rasterDal{nullptr};

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Globals             ();

                   ~Globals            () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             init                ();

  void             setCloneSpace       (geo::RasterSpace const& space);

  void             setRandomSeed       (unsigned int seed);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  geo::RasterSpace const& cloneSpace   ();

  calc::RunTimeEngine& rte             ();

  dal::RasterDal&  rasterDal           ();

};

PCRASTER_PYTHON_UTILS_EXPORT extern Globals globals;

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace python
} // namespace pcraster

#endif