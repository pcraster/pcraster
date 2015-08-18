#include "ag_DataSourceTable.h"

// Library headers.
#include <boost/foreach.hpp>
#include <QHeaderView>

// PCRaster library headers.

// Module headers.
#include "ag_DataObject.h"
#include "ag_Util.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the DataSourceTable class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASOURCETABLE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATASOURCETABLE MEMBERS
//------------------------------------------------------------------------------

DataSourceTable::DataSourceTable(
         DataObject *object,
         QWidget* parent)

  : TableVisualisation(object, "Data source table", parent)

{
  BOOST_FOREACH(DataGuide const& guide, dataObject().dataGuides()) {
    addAttribute(guide);
  }

  createInterface();
  updateValues();
}



DataSourceTable::~DataSourceTable()
{
}



void DataSourceTable::createInterface()
{
  setColumnCount(3);

  QStringList labels;
  labels << "dataset" << "value" << "properties";
  setHorizontalHeaderLabels(labels);

  fillTable();
}



void DataSourceTable::updateValues()
{
  // Update values at current cursor position.
  std::vector<DataGuide> const& guides(visualisationEngine().dataGuides());

  for(size_t i = 0; i < guides.size(); ++i) {
    item(i, 1)->setText(QString(dataObject().label(guides[i]).c_str()));
  }
}



void DataSourceTable::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void DataSourceTable::process()
{
  if(    visualisationEngine().change() & VisEngine::OTHERATTRIB) {
    clearTable();
    fillTable();
  }
}



void DataSourceTable::visualise()
{
  if(    visualisationEngine().change() & VisEngine::SELECTION) {
    updateSelection();
  }
  else if(visualisationEngine().change() & VisEngine::CURSOR ||
         visualisationEngine().change() & VisEngine::VALUE_SELECTION ||
         visualisationEngine().change() & VisEngine::DRAWPROPS) {
    updateValues();
  }

  visualisationEngine().finishedScanning(dataObject());
}



// not called by Qt4 anymore
// void DataSourceTable::contentsMousePressEvent(
//          QMouseEvent *event)
// {
//   std::cout << "contentsMousePressEvent" << std::endl;
// 
//   int row = rowAt(event->y());
//   int col = columnAt(event->x());
// 
//   if(row < 0 || col < 0) {
//     dataObject().setSelected(false);
//   }
//   else if(row >= 0) {
//     std::vector<DataGuide> const& guides(visualisationEngine().dataGuides());
// 
//     for(int i = 0; i < int(guides.size()); ++i) {
//       if(i != row) {
//         dataObject().setSelected(guides[i], false, false);
//       }
//       else {
//         dataObject().setSelected(guides[i], true, false);
//       }
//     }
// 
//     dataObject().notify();
//   }
// }



void DataSourceTable::addAttribute(
         DataGuide const& guide)
{
  visualisationEngine().addAttribute(dataObject(), guide);
}



void DataSourceTable::fillTable()
{
  assert(rowCount() == 0);

  std::vector<DataGuide> const& guides(visualisationEngine().dataGuides());

  setRowCount(guides.size());

  for(size_t i = 0; i < guides.size(); ++i) {
    QTableWidgetItem* item;

    item = new QTableWidgetItem(
         QString(dataObject().description(guides[i]).c_str()));
    item->setIcon(ag::pixmap(guides[i]));
    setItem(i, 0, item);

    item = new QTableWidgetItem(
         QString(dataObject().label(guides[i]).c_str()));
    setItem(i, 1, item);

    // Make sure the table knows which guide is in which row.
    guideMap()[i] = guides[i];
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

