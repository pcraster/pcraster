#ifndef INCLUDED_AG_TABLEVISUALISATION
#define INCLUDED_AG_TABLEVISUALISATION



// External headers.
#include <QTableWidget>

// Project headers.

// Module headers.
#include "ag_Visualisation.h"



namespace ag {
  // TableVisualisation declarations.
}



namespace ag {

//! Base class for visualisations that order their contents in a table.
/*!
  This class reserves one row for each piece of data to be visualized.

  Specializations of this class must configure the row index to data guide
  look up table (see guideMap()).
*/
class TableVisualisation: public Visualisation<QTableWidget>
{

  friend class TableVisualisationTest;

private:

  Q_OBJECT

  //! Translates row indices to data guides.
  std::map<int, DataGuide> d_guideMap;

  void             selectRow           (int row);

  void             deselectRow         (int row);

  bool             rowIsSelected       (int row) const;

private Q_SLOTS:

  void             handleChangedSelection(
                                        QItemSelection const& selectedItems,
                                        QItemSelection const& deselectedItems);

protected:

                   TableVisualisation  (DataObject* object,
                                        std::string const& visualisationName,
                                        QWidget* parent);

  std::map<int, DataGuide>& guideMap   ();

  void             clearTable          ();

  void             updateSelection     ();

  void             focusOutEvent       (QFocusEvent* event);

protected Q_SLOTS:

  virtual void     handleDoubleClickedCell(
                                        int row,
                                        int col);

  virtual void     handleRequestedCustomContextMenu(
                                        QPoint const& pos);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~TableVisualisation ();

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
