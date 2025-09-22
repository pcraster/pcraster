#ifndef INCLUDED_AG_CURSORVIEW
#define INCLUDED_AG_CURSORVIEW


// PCRaster library headers.
#include "dal_DataSpaceAddress.h"

// Module headers.
#include "ag_Visualisation.h"

#include <tuple>
#include <vector>


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

  typedef std::tuple<QLabel*, DimensionCoordinateEdit*, QLabel*> DimensionTuple;

  Q_OBJECT

  std::vector<DimensionTuple> d_dimensionTuples;

  DataSourceTable* d_datasetTable{nullptr};

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

  void             rescan              () override;

  void             process             () override;

  void             visualise           () override;

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

  /* virtual */    ~CursorView        () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& guide);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  QSize            sizeHint            () const override;

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
