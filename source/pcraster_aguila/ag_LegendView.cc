#include "ag_LegendView.h"

// Library headers.
#include <boost/scoped_ptr.hpp>
#include <QHeaderView>
#include <QGridLayout>
#include <QMenu>
#include <QMessageBox>

// PCRaster library headers.

// Module headers.
#include "dal_Client.h"
#include "dal_Dal.h"
#include "dal_Exception.h"
#include "dal_TableDriver.h"
#include "ag_ClassDrawPropertiesWidget.h"
#include "ag_DataObject.h"
#include "ag_DataProperties.h"
#include "ag_GeneralDataPropertiesWidget.h"
#include "ag_Legend.h"
#include "ag_PropertiesDialog.h"
#include "ag_RangeDrawPropertiesWidget.h"
#include "ag_SaveDataAsDialog.h"
#include "ag_Viewer.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the LegendView class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LEGENDVIEW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LEGENDVIEW MEMBERS
//------------------------------------------------------------------------------

LegendView::LegendView(
         DataObject* object,
         ViewerType type,
         QWidget* parent)

  : TableVisualisation(object, "Legend View", parent),
    _viewerType(type)

{
  // Supported data types.
  std::vector<geo::DataType> dataTypes;
  dataTypes.push_back(geo::STACK);
  dataTypes.push_back(geo::FEATURE);
  dataTypes.push_back(geo::VECTOR);
  dataTypes.push_back(geo::TIMESERIES);
  setSupportedDataTypes(dataTypes);

  // Supported value scales.
  std::vector<CSF_VS> valueScales;
  valueScales.push_back(VS_BOOLEAN);
  valueScales.push_back(VS_NOMINAL);
  valueScales.push_back(VS_ORDINAL);
  valueScales.push_back(VS_SCALAR);
  valueScales.push_back(VS_DIRECTION);
  valueScales.push_back(VS_LDD);

  // Feature data without attribute, geometry only.
  valueScales.push_back(VS_UNDEFINED);

  setSupportedValueScales(valueScales);

  viewport()->setBackgroundRole(QPalette::Window);
  setFrameStyle(QFrame::NoFrame);
  setShowGrid(false);
  horizontalHeader()->hide();
  verticalHeader()->hide();
  setColumnCount(1);

  createActions();
}



LegendView::~LegendView()
{
}



void LegendView::createActions()
{
  _generalPropertiesAction = new QAction("Edit general properties...", this);
  connect(_generalPropertiesAction, SIGNAL(triggered()), this,
         SLOT(editGeneralProperties()));

  _drawPropertiesAction = new QAction("Edit draw properties...", this);
  connect(_drawPropertiesAction, SIGNAL(triggered()), this,
         SLOT(editDrawProperties()));

  _saveGraphAction = new QAction("Save graph data as...", this);
  connect(_saveGraphAction, SIGNAL(triggered()), this,
         SLOT(saveGraphData()));

  _mapAction = new QAction("Show map...", this);
  connect(_mapAction, SIGNAL(triggered()), this,
         SLOT(showMap()));

  _timeSeriesAction = new QAction("Show time series...", this);
  connect(_timeSeriesAction, SIGNAL(triggered()), this,
         SLOT(showTimeSeries()));

  _cumulativePropabilityPlotAction = new QAction(
         "Show probability plot...", this);
  connect(_cumulativePropabilityPlotAction, SIGNAL(triggered()), this,
         SLOT(showCumulativeProbabilityPlot()));

  // newAct = new QAction(tr("&New"), this);
  // newAct->setShortcut(tr("Ctrl+N"));
  // newAct->setStatusTip(tr("Create a new file"));
  // connect(newAct, SIGNAL(triggered()), this, SLOT(newFile()));
}



Legend* LegendView::contextMenuLegend() const
{
  assert(_contextMenuIndex.isValid());

  Legend* legend = dynamic_cast<Legend*>(indexWidget(_contextMenuIndex));
  assert(legend);

  return legend;
}



void LegendView::editGeneralProperties()
{
  assert(!_contextMenuGuides.empty());

  PropertiesWidget* widget = new GeneralDataPropertiesWidget(
         dataObject(), _contextMenuGuides.front(), this);
  PropertiesDialog* dialog = new PropertiesDialog(widget, this);
  dialog->setFixedSize(dialog->sizeHint());
  dialog->exec();
}



void LegendView::editDrawProperties()
{
  assert(!_contextMenuGuides.empty());

  if(_contextMenuGuides.front().type() == geo::VECTOR) {
    // Properties of vector drawer cannot be edited at the moment.
    return;
  }

  PropertiesWidget* widget = 0;

  if(_contextMenuGuides.front().valueScale() == VS_BOOLEAN ||
     _contextMenuGuides.front().valueScale() == VS_NOMINAL ||
     _contextMenuGuides.front().valueScale() == VS_ORDINAL ||
     _contextMenuGuides.front().valueScale() == VS_LDD) {
    widget = new ClassDrawPropertiesWidget(dataObject(), _contextMenuGuides.front(), this);
  }
  else if(_contextMenuGuides.front().valueScale() == VS_SCALAR ||
          _contextMenuGuides.front().valueScale() == VS_DIRECTION) {
    widget = new RangeDrawPropertiesWidget(dataObject(), _contextMenuGuides.front(), this);
  }

  if(widget) {
    PropertiesDialog* dialog = new PropertiesDialog(widget, this);
    dialog->setFixedSize(dialog->sizeHint());
    dialog->exec();
  }
}



void LegendView::saveGraphData()
{
  assert(!_contextMenuGuides.empty());

  SaveDataAsDialog dialog(dal::Client::dal().writerFormats(dal::TABLE), this);

  if(dialog.exec() == QDialog::Accepted) {
    DataGuide const& guide = _contextMenuGuides.front();
    dal::Table const* table = 0;
    boost::scoped_ptr<dal::Table> scopedTable;

    // We only handle time graph data at the moment.
    assert(dataObject().hasTimeSeries(guide));
    assert(!dataObject().hasCumProbabilities(guide));

    if(guide.type() == geo::STACK) {
      // This table is generated so we keep it in a scoped pointer.
      scopedTable.reset(new dal::Table());
      dataObject().rasterDataSources().data(guide).readTimeSeries(
        dataObject().dataSpace(), dataObject().dataSpaceAddress(),
        *scopedTable.get());
      table = scopedTable.get();
    }
    else if(guide.type() == geo::FEATURE) {
      /// FEATURE
    }
    else if(guide.type() == geo::TIMESERIES) {
      // This table is for use only.
      table = &dataObject().tableDataSources().data(guide).table();
    }

    dal::Format const& format(dialog.selectedFormat());

    try {
      assert(dal::Client::dal().driverByName(format.name()));
      dal::DataSpace space(dataObject().dataSpace());
      dal::DataSpaceAddress address(dataObject().dataSpaceAddress());
      size_t index = space.indexOf(dal::Time);
      space.eraseDimension(index);
      address.eraseCoordinate(index);
      dal::Driver* driver(dal::Client::dal().driverByName(format.name()));
      assert(driver);
      dynamic_cast<dal::TableDriver const*>(driver)->write(
         *table, space, address, dialog.name());
    }
    catch(dal::Exception const& exception) {
      QMessageBox::critical(this, "Save graph data...",
          QString(exception.message().c_str()), QMessageBox::Ok);
    }
  }
}



void LegendView::showMap()
{
  assert(!_contextMenuGuides.empty());

  Viewer* viewer = Viewer::instance();
  viewer->createMapView(_contextMenuGuides,
         viewer->group(this));
}



void LegendView::showTimeSeries()
{
  assert(!_contextMenuGuides.empty());

  Viewer* viewer = Viewer::instance();
  viewer->createTimeGraphView(_contextMenuGuides,
         viewer->group(this));
}



void LegendView::showCumulativeProbabilityPlot()
{
  assert(!_contextMenuGuides.empty());

  Viewer* viewer = Viewer::instance();
  viewer->createProbabilityGraphView(_contextMenuGuides,
         viewer->group(this));
}



LegendView::LegendTuples LegendView::legendTuples(
         DataGuide const& guide)
{
  LegendTuples result;

  for(size_t i = 0; i < _legendTuples.size(); ++i) {
    std::vector<DataGuide> const& guides(boost::get<0>(_legendTuples[i]));

    if(std::find(guides.begin(), guides.end(), guide) != guides.end()) {
      result.push_back(_legendTuples[i]);
    }
  }

  assert(!result.empty());

  return result;
}



std::vector<Legend*> LegendView::legends(
         DataGuide const& guide)
{
  std::vector<Legend*> result;
  LegendTuples tuples(legendTuples(guide));

  for(size_t i = 0; i < tuples.size(); ++i) {
    Legend* legend(boost::get<1>(tuples[i]));
    result.push_back(legend);
  }

  assert(!result.empty());

  return result;
}



std::vector<DataGuide> const& LegendView::dataGuides(
       Legend const* legend) const
{
  std::vector<DataGuide> const* result = 0;

  for(size_t i = 0; i < _legendTuples.size(); ++i) {
    std::vector<DataGuide> const& guides(boost::get<0>(_legendTuples[i]));

    if(legend == boost::get<1>(_legendTuples[i])) {
      result = &guides;
    }
  }

  assert(result);

  return *result;
}



std::vector<DataGuide> const& LegendView::dataGuides(
         QModelIndex const& index) const
{
  assert(index.isValid());

  std::vector<DataGuide> const* result = 0;

  // Determine data guides connected to legend.
  Legend* legend = dynamic_cast<Legend*>(indexWidget(index));
  assert(legend);
  result = &dataGuides(legend);

  return *result;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  It is assumed here that when two data guides point to the same draw
  properties, they are merged and only a legend for the first guide is created.
*/
void LegendView::recreateLegend(
         std::vector<DataGuide> const& guides)
{
  // Clean up.
  _legends.clear();
  clearTable();

  // Create legend.

  // (Draw property, index) tuple type.
  typedef boost::tuple<DrawProps*, size_t> DrawPropertiesLegendIndexTuple;

  // Collection of tuples.
  std::vector<DrawPropertiesLegendIndexTuple> drawPropertiesLegendIndexTuples;

  DrawProps* properties;
  Legend* legend;
  int colWidth = 0;

  for(int i = guides.size() - 1; i >= 0; --i) {
    DataGuide const& guide(guides[i]);

    // Properties of current data item.
    properties = &dataObject().properties().drawProperties(guide);

    // Do not create a legend when an equal legend is already created.
    // Bit simplistic, needs refinement.
    // TODO keep track of all dataguides for which a legend is created.
    // TODO We need to be able to connect a legend to every dataguide later.
    // TODO See also legends(...).
    // TODO LegendPair needs to support a vector of guides.

    std::vector<size_t> legendIndices;

    // Loop over all (property, index) tuples.
    for(size_t i = 0; i < drawPropertiesLegendIndexTuples.size(); ++i) {
      // Find tuple who's properties corresponds with properties of current
      // data item, if present.
      if(boost::get<0>(drawPropertiesLegendIndexTuples[i]) == properties) {
        legendIndices.push_back(boost::get<1>(
              drawPropertiesLegendIndexTuples[i]));
      }
    }

    if(!legendIndices.empty()) {
      // A legend for this guide has already been created using another guide.
      for(size_t i = 0; i < legendIndices.size(); ++i) {
        std::vector<DataGuide>& guides(boost::get<0>(
              _legendTuples[legendIndices[i]]));

        if(std::find(guides.begin(), guides.end(), guide) == guides.end()) {
          guides.push_back(guide);
        }
      }
    }
    else {
      // Assume the first guide can represent all guides with the same
      // properties.
      legend = new Legend(dataObject(), guide, _viewerType);
      setRowCount(rowCount() + 1);
      setRowHeight(rowCount() - 1, legend->height());
      colWidth = std::max(legend->width(), colWidth);
      setCellWidget(rowCount() - 1, 0, legend);
      _legends.push_back(legend);

      std::vector<DataGuide> guides;
      guides.push_back(guide);
      LegendTuple tuple(guides, legend);
      _legendTuples.push_back(tuple);

      drawPropertiesLegendIndexTuples.push_back(
         DrawPropertiesLegendIndexTuple(properties, _legendTuples.size() -1));
    }

    // Make sure the table knows which guide is in which row.
    guideMap()[rowCount() - 1] = guide;
  }

  setColumnWidth(0, colWidth);
}



void LegendView::addAttribute(
         DataGuide const& guide)
{
  testDataGuide(guide);
  visualisationEngine().addAttribute(dataObject(), guide);
}



void LegendView::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void LegendView::process()
{
  if(visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
    if(!dataObject().backgroundColour().isValid()) {
      setPalette(QPalette());
    }
    else {
      QPalette palette;
      palette.setColor(backgroundRole(), dataObject().backgroundColour());
      setPalette(palette);
    }
  }
}



void LegendView::visualise()
{
  // Done scanning, update stuff if needed.
  if(    visualisationEngine().change() & VisEngine::OTHERATTRIB ||
         visualisationEngine().change() & VisEngine::DRAWPROPS  ||
         visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR // ||
         // Results in too many recreation of legend when it is not needed.
         // I guess we're only interested in flips of no value selected to
         // value selected or the other way around. Not interested in selected
         // value changes.
         /* visualisationEngine().change() & VisEngine::VALUE_SELECTION */) {
    recreateLegend(visualisationEngine().dataGuides());
  }

  if(visualisationEngine().change() & VisEngine::SELECTION) {
    updateSelection();
  }

  visualisationEngine().finishedScanning(dataObject());
}



void LegendView::handleRequestedCustomContextMenu(
         QPoint const& pos)
{
  // Determine the cell pos is pointing to.
  _contextMenuIndex = indexAt(pos);

  // The index is not valid when the user clicks outside of a legend.
  if(!_contextMenuIndex.isValid()) {
    _contextMenuGuides.clear();
  }
  else {
    _contextMenuGuides = dataGuides(_contextMenuIndex);
    DataGuide guide = _contextMenuGuides.front();

    QMenu menu(this);
    menu.addAction(_generalPropertiesAction);

    // Filter out attributes for which there's no draw properties editor.
    if(!(guide.type() == geo::FEATURE && guide.valueScale() == VS_UNDEFINED) &&
       !(guide.type() == geo::VECTOR)) {
      menu.addAction(_drawPropertiesAction);
    }

    // if(isRowSelected(_contextMenuIndex.row())) {
    //   // Menu in case of selected legends.
    //   // _selectedMenuGuides = ...;
    // }

    if(dataObject().hasSpace(guide)) {
      menu.addAction(_mapAction);
    }

    if(dataObject().hasTimeSeries(guide)) {
      menu.addAction(_timeSeriesAction);
    }

    if(dataObject().hasCumProbabilities(guide)) {
      menu.addAction(_cumulativePropabilityPlotAction);
    }

    if(_viewerType == VT_Graph) {
      if(_contextMenuGuides.size() == 1 &&
         !dataObject().hasCumProbabilities(guide)) {
        menu.addAction(_saveGraphAction);
      }
    }

    menu.exec(mapToGlobal(pos));
  }
}



void LegendView::handleDoubleClickedCell(
         int row,
         int col)
{
  _contextMenuIndex = model()->index(row, col);
  assert(_contextMenuIndex.isValid());

  _contextMenuGuides = dataGuides(_contextMenuIndex);

  editDrawProperties();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

