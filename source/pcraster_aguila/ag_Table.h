#ifndef INCLUDED_AG_TABLE
#define INCLUDED_AG_TABLE



// Library headers.
#include <cassert>

// PCRaster library headers.
#include "dal_DataSource.h"
#include "dal_DataSpaceAddress.h"
#include "dal_Dimension.h"
#include "dal_Table.h"
#include "pcrtypes.h"

// Module headers.
#include "ag_Dataset.h"



namespace dal {
  class DataSpace;
  class Table;
}
namespace ag {
  // Table declarations.
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \todo Rename to TimeSeries or similar. Table is much to general.
*/
class Table: public Dataset
{

  friend class TableTest;

private:

  dal::Table*      d_table;

  size_t           d_timeCol;

  size_t           d_attrCol;

  CSF_VS           d_valueScale;

  bool             isRead              (dal::DataSpaceAddress const& address) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Table               (std::string const& name,
                                        dal::DataSpace const& space);

  /* virtual */    ~Table              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             read                (dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  CSF_VS           valueScale          () const;

  bool             isRead              () const;

  size_t           nrCols              () const;

  size_t           nrRecs              () const;

  size_t           timeCol             () const;

  size_t           attrCol             () const;

  template<typename T>
  T const*         col                 (size_t col) const;

  template<typename T>
  bool             value               (T& result,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

  /// dal::DataSource const& source        () const;

  dal::Table const& table              () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
inline T const* Table::col(size_t col) const
{
  assert(d_table);
  return d_table->template col<T>(col).elements();
}

template<typename T>
bool Table::value(
         T& result,
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  pcr::setMV(result);

  if(Dataset::isRead(space, address)) {
    assert(dataSource().dataSpace().hasTime());
    size_t index = dataSource().dataSpace().indexOf(dal::Time);
    size_t step = address.template coordinate<size_t>(index);

    dal::Array<UINT4> const& steps(d_table->col<UINT4>(d_timeCol));
    dal::Array<UINT4>::const_iterator it =
         std::find(steps.begin(), steps.end(), step);

    if(it != steps.end()) {
      size_t record = std::distance(steps.begin(), it);

      if(!pcr::isMV(d_table->template cell<T>(record, d_attrCol))) {
        result = d_table->template cell<T>(record, d_attrCol);
      }
    }
  }

  return !pcr::isMV(result);
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
