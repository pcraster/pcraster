#ifndef INCLUDED_GEO_ASSIGNFILTER
#define INCLUDED_GEO_ASSIGNFILTER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_FILTER
#include "geo_filter.h"
#define INCLUDED_GEO_FILTER
#endif



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

  /* virtual */    ~AssignFilter       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  int              calcUL              (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const;

  int              calcUR              (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const;

  int              calcLR              (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const;

  int              calcLL              (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const;

  int              calcTop             (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const;

  int              calcBottom          (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const;

  int              calcLeft            (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const;

  int              calcRight           (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const;

  int              calcInterior        (const SimpleRaster<int>& source,
                                        size_t row,
                                        size_t col) const;

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
