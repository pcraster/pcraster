#ifndef INCLUDED_GEO_ASSIGNFILTER
#define INCLUDED_GEO_ASSIGNFILTER

#include "stddefx.h"
#include "geo_filter.h"



namespace geo {
  // AssignFilter declarations.
}



namespace geo {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class AssignFilter: public Filter<int, int>
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  AssignFilter&    operator=           (const AssignFilter&);

  //! Copy constructor. NOT IMPLEMENTED.
                   AssignFilter        (const AssignFilter&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AssignFilter        (const SimpleRaster<double>& weights);

  /* virtual */    ~AssignFilter       () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  int              calcUL              (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const override;

  int              calcUR              (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const override;

  int              calcLR              (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const override;

  int              calcLL              (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const override;

  int              calcTop             (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const override;

  int              calcBottom          (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const override;

  int              calcLeft            (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const override;

  int              calcRight           (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const override;

  int              calcInterior        (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const override;

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
