#ifndef INCLUDED_AG_TABLEDATASOURCES
#define INCLUDED_AG_TABLEDATASOURCES



// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_DataObjectBase.h"
#include "ag_Table.h"



namespace ag {
  // TableDataSources declarations.
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class TableDataSources: public DataObjectBase<Table>
{

  friend class TableDataSourcesTest;

private:

protected:

  DataInfo<Table>  openData            (std::string const &name,
                                        dal::DataSpace const &space) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TableDataSources    ();

  /* virtual */    ~TableDataSources   ();

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
