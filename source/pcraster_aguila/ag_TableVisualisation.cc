#include "ag_TableVisualisation.h"

// External headers.
#include <boost/foreach.hpp>
#include <QFocusEvent>

// Project headers.

// Module headers.
#include "ag_DataObject.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the TableVisualisation class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TABLEVISUALISATION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TABLEVISUALISATION MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     object
  \param     visualisationName
  \param     parent
  \warning   .
  \sa        .

  Some default behaviour is configured here:
  - It is assumed selection is row based and that more than one row can be
    selected.
  - Cell contents are not editable.
*/
TableVisualisation::TableVisualisation(
         DataObject* object,
         std::string const& visualisationName,
         QWidget* parent)

  : Visualisation<QTableWidget>(object, visualisationName, parent)

{
  setSelectionBehavior(SelectRows);
  setSelectionMode(ExtendedSelection);
  setEditTriggers(NoEditTriggers);
  setContextMenuPolicy(Qt::CustomContextMenu);

  connect(selectionModel(), SIGNAL(selectionChanged(QItemSelection const&,
         QItemSelection const&)), this, SLOT(handleChangedSelection(
         QItemSelection const&, QItemSelection const&)));
  connect(this, SIGNAL(cellDoubleClicked(int, int)),
         this, SLOT(handleDoubleClickedCell(int, int)));
  connect(this, SIGNAL(customContextMenuRequested(QPoint const&)),
         this, SLOT(handleRequestedCustomContextMenu(QPoint const&)));
}



//! Destructor.
/*!
*/
TableVisualisation::~TableVisualisation()
{
}



//! Map for keeping track of which data guide is represented by which row.
/*!
  \return    Lookup table for looking up data guide by row index.

  Specializations must make sure that this look up table reflects the state
  of the table.
*/
std::map<int, DataGuide>& TableVisualisation::guideMap()
{
  return d_guideMap;
}



//! Queries the data object for selection status of data guides and adjusts the table accordingly.
/*!
  This function should be called in case the selection state of the data object
  has changed and the table should know about it.
*/
void TableVisualisation::updateSelection()
{
  for(std::map<std::string, DataGuide>::size_type i = 0; i < d_guideMap.size();
         ++i) {
    if(dataObject().isSelected(d_guideMap[i])) {
      selectRow(i);
    }
    else {
      deselectRow(i);
    }
  }
}



bool TableVisualisation::rowIsSelected(
         int row) const
{
  bool result = false;

  BOOST_FOREACH(QModelIndex const& index, selectedIndexes()) {
    if(index.row() == row) {
      result = true;
      break;
    }
  }

  return result;
}



//! Selects \a row.
/*!
  \param     row Index of row to select.
*/
void TableVisualisation::selectRow(
         int row)
{
  // Selecting an already selected row results in recursion because Qt
  // treats it as a selection change.
  if(!rowIsSelected(row)) {
    QModelIndex topLeft(model()->index(row, 0, QModelIndex()));
    QModelIndex bottomRight(model()->index(row, columnCount() - 1,
           QModelIndex()));
    QItemSelection selection(topLeft, bottomRight);

    selectionModel()->select(selection, QItemSelectionModel::Select);
  }
}



//! Deselects \a row.
/*!
  \param     row Index of row to deselect.
*/
void TableVisualisation::deselectRow(
         int row)
{
  // Deselecting an already deselected row results in recursion because Qt
  // treats it as a selection change.
  if(rowIsSelected(row)) {
    QModelIndex topLeft(model()->index(row, 0, QModelIndex()));
    QModelIndex bottomRight(model()->index(row, columnCount() - 1,
           QModelIndex()));
    QItemSelection selection(topLeft, bottomRight);

    selectionModel()->select(selection, QItemSelectionModel::Deselect);
  }
}



//! Adjusts the data object according to the selection changes in the table.
/*!
  \param     selectedItems Items newly selected.
  \param     deselectedItems Items newly deselected.

  This function should be called when the selection state of the table has
  changed and the data object should know about it.

  The data object is notified of the changed made.
*/
void TableVisualisation::handleChangedSelection(
         QItemSelection const& selectedItems,
         QItemSelection const& deselectedItems)
{
  QModelIndex index;

  BOOST_FOREACH(index, selectedItems.indexes()) {
    dataObject().setSelected(d_guideMap[index.row()], true, false);
  }

  BOOST_FOREACH(index, deselectedItems.indexes()) {
    dataObject().setSelected(d_guideMap[index.row()], false, false);
  }

  dataObject().notify();
}



//! Clears the contents of the table.
/*!
  The number of columns in the table will remain the same. All rows will be
  removed.
*/
void TableVisualisation::clearTable()
{
  while(rowCount()) {
    removeRow(rowCount() - 1);
  }

  guideMap().clear();

  assert(rowCount() == 0);
}



//! This function is called when a cell is double clicked.
/*!
  \param     row Row index of the cell.
  \param     col Column index of the cell.

  The default does nothing.
*/
void TableVisualisation::handleDoubleClickedCell(
         int /* row */,
         int /* col */)
{
}



//! This function is called when a custom context menu is requested.
/*!
  \param     pos Position of the mouse.

  The default does nothing.
*/
void TableVisualisation::handleRequestedCustomContextMenu(
         QPoint const& /* pos */)
{
}



void TableVisualisation::focusOutEvent(
         QFocusEvent* event)
{
  if(event->reason() != Qt::PopupFocusReason) {
    clearSelection();
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

