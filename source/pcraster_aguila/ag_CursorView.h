#ifndef INCLUDED_AG_CURSORVIEW
#define INCLUDED_AG_CURSORVIEW



// Library headers.
#include <vector>
#include <boost/tuple/tuple.hpp>

// PCRaster library headers.
#include "dal_DataSpaceAddress.h"

// Module headers.
#include "ag_Visualisation.h"



class QLabel;
namespace dal {
  class Dimension;
}
namespace ag {
  // CursorView declarations.
  class DataSourceTable;
  class DimensionCoordinateEdit;
  class DataObject;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \todo Move all table stuff to DataSourceTable class. That should be a
        Visualisation too.
*/
class CursorView: public Visualisation<>
{

  friend class CursorTest;

private:

  typedef boost::tuple<QLabel*, DimensionCoordinateEdit*, QLabel*> DimensionTuple;

  Q_OBJECT

  std::vector<DimensionTuple> d_dimensionTuples;

  DataSourceTable* d_datasetTable;

  //! Assignment operator. NOT IMPLEMENTED.
  CursorView&     operator=           (CursorView const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CursorView         (CursorView const& rhs);

  DimensionCoordinateEdit* dimensionCoordinateEdit(
                                        size_t index);

  QLabel*          valueLabel          (size_t index);

  size_t           tupleIndex          (dal::Dimension const* dimension) const;

  void             createInterface     ();

  void             updateCoordinates   ();

  void             rescan              ();

  void             process             ();

  void             visualise           ();

  dal::DataSpaceAddress addressWithUpdatedCoordinate(
                                        dal::Dimension const* dimension,
                                        size_t index) const;

  void             updateEditLabel     (dal::Dimension const* dimension,
                                        dal::DataSpaceAddress const& address);

private Q_SLOTS:

  void             updateCoordinate    (dal::Dimension const* dimension,
                                        size_t index);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CursorView          (DataObject* object,
                                        QWidget* parent);

  /* virtual */    ~CursorView        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& guide);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  QSize            sizeHint            () const;

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
