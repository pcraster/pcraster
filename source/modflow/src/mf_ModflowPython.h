#ifndef INCLUDED_MODFLOW_MODFLOW_PYTHON
#define INCLUDED_MODFLOW_MODFLOW_PYTHON

#include "pcrmodflow.h"

#include <vector>


namespace mf {
  // Modflow declarations.
}



namespace mf {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class PCRModflowPython: public PCRModflow
{

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PCRModflowPython    (size_t rows, size_t cols, double cellsize, double xll, double yll);

                   PCRModflowPython    (const geo::RasterSpace &raster);

  /* virtual */    ~PCRModflowPython   () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             set_row_width       (std::vector<float> const& arguments);

  void             set_col_width       (std::vector<float> const& arguments);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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

} // namespace mf

#endif
