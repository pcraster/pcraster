#ifndef INCLUDED_AG_VECTORDATASOURCES
#define INCLUDED_AG_VECTORDATASOURCES



// External headers.

// Project headers.

// Module headers.
#include "ag_DataObjectBase.h"
#include "ag_Vector.h"



namespace ag {
  // VectorDataSources declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class VectorDataSources: public DataObjectBase<Vector>
{

  friend class VectorDataSourcesTest;

private:

  DataInfo<Vector> openData            (std::string const& name,
                                        dal::DataSpace const& space) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VectorDataSources   ();

  /* virtual */    ~VectorDataSources  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

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

} // namespace ag

#endif
