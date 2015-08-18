#include "ag_Map2DView.h"

// Library headers.
#include <QMessageBox>
#include <QMouseEvent>
#include <QPainter>
#include <QPixmap>
#include <QPoint>

// PCRaster library headers.
#include "dal_MathUtils.h"
#include "dal_RasterDimensions.h"
#include "com_classifier.h"
#include "com_exception.h"
#include "com_rawpalette.h"
#include "qt_ColourLib.h"

// Module headers.
#include "ag_BooleanRasterDrawer.h"
#include "ag_DataObject.h"
#include "ag_DataProperties.h"
#include "ag_DirectionalRasterDrawer.h"
#include "ExceedanceProbabilityFeatureLayerDrawer.h"
#include "ag_ExceedanceProbabilityRasterDrawer.h"
#include "ag_FeatureLayerDrawer.h"
#include "ag_LddRasterDrawer.h"
#include "ag_NominalRasterDrawer.h"
#include "ag_OrdinalRasterDrawer.h"
#include "ag_RangeDrawProps.h"
#include "ag_RangeFeatureLayerDrawer.h"
#include "ag_RasterDataSources.h"
#include "ag_ScalarRasterDrawer.h"
#include "ag_VectorDrawer.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the Map2DView class.
*/



namespace {

double worldUnitsToPixels(
         double scale,
         double zoom,
         qreal amount)
{
  assert(scale != 0.0);

  return (zoom * amount) / scale;
}



double pixelsToWorldUnits(
         double scale,
         double zoom,
         double amount)
{
  assert(zoom != 0.0);

  return (scale * amount) / zoom;
}

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MAP2DVIEW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MAP2DVIEW MEMBERS
//------------------------------------------------------------------------------

//! ctor
Map2DView::Map2DView(
         DataObject* object,
         QWidget* parent)

  : BufferedVisualisation(object, "Map View", BufferedWidget::Center, parent),
    _action(NoAction)

{
  // Supported data types.
  std::vector<geo::DataType> dataTypes;
  dataTypes.push_back(geo::STACK);
  dataTypes.push_back(geo::FEATURE);
  dataTypes.push_back(geo::VECTOR);
  setSupportedDataTypes(dataTypes);

  // Supported value scales.
  std::vector<CSF_VS> valueScales;
  valueScales.push_back(VS_BOOLEAN);
  valueScales.push_back(VS_NOMINAL);
  valueScales.push_back(VS_ORDINAL);
  valueScales.push_back(VS_SCALAR);
  valueScales.push_back(VS_DIRECTION);
  valueScales.push_back(VS_LDD);

  // Feature data without attribute, only geometry.
  valueScales.push_back(VS_UNDEFINED);

  setSupportedValueScales(valueScales);

  // Supported file formats. -> can be changed if map2d supports eps.
  std::vector<com::FileFormatInfo> fileFormats;
  fileFormats.push_back(com::FileFormatInfo::png());
  setSaveAsFileFormats(fileFormats);

  setFocusPolicy(Qt::WheelFocus);

  startQueryMode();
}



//! dtor
Map2DView::~Map2DView()
{
}



void Map2DView::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void Map2DView::process()
{
  if(visualisationEngine().change() & VisEngine::MAP2DZOOM) {
    setDirty();
  }

  if(visualisationEngine().change() & VisEngine::MAP2DMOVE) {
    QPointF offset(worldUnitsToPixels(
         dataObject().map2DOffset() - visualisationEngine().map2DOffset()));
    moveBy(offset);
  }

  if(visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
    if(!dataObject().backgroundColour().isValid()) {
      setPalette(QPalette());
    }
    else {
      QPalette palette;
      palette.setColor(backgroundRole(), dataObject().backgroundColour());
      setPalette(palette);
    }

    deleteScene(QRectF(0.0, 0.0, size().width(), size().height()));
  }

  if(visualisationEngine().change() & VisEngine::OTHERATTRIB ||
         visualisationEngine().change() & VisEngine::VISIBILITY ||
         visualisationEngine().change() & VisEngine::DRAWPROPS ||
         visualisationEngine().change() & VisEngine::MAP2DZOOM ||
         visualisationEngine().change() & VisEngine::MAP2DSCALE ||
         (visualisationEngine().change() & VisEngine::CURSOR &&
          (visualisationEngine().change() & VisEngine::TIME ||
           visualisationEngine().change() & VisEngine::QUANTILE)) ||
         visualisationEngine().change() & VisEngine::VALUE_SELECTION ||
         visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
    createScene(visualisationEngine().dataGuides(),
         QRectF(0.0, 0.0, size().width(), size().height()));
    setClean();
  }
}



void Map2DView::visualise()
{
  if(visualisationEngine().change() & VisEngine::OTHERATTRIB ||
         visualisationEngine().change() & VisEngine::VISIBILITY ||
         visualisationEngine().change() & VisEngine::DRAWPROPS ||
         visualisationEngine().change() & VisEngine::MAP2DZOOM ||
         visualisationEngine().change() & VisEngine::MAP2DSCALE ||
         (visualisationEngine().change() & VisEngine::CURSOR &&
          (visualisationEngine().change() & VisEngine::TIME ||
           visualisationEngine().change() & VisEngine::QUANTILE)) ||
         visualisationEngine().change() & VisEngine::RASTER_CELL ||
         visualisationEngine().change() & VisEngine::VALUE_SELECTION ||
         visualisationEngine().change() & VisEngine::MAP2DMOVE ||
         visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
    repaint();
  }

  visualisationEngine().finishedScanning(dataObject());
}



void Map2DView::paintEvent(
         QPaintEvent* event)
{
  // Draw map and crosshair.
  BufferedWidget::paintEvent(event);
  drawCrossHair();
  drawZoomRectangle(zoomRectangle());
}



void Map2DView::startQueryMode()
{
  setCursor(Qt::PointingHandCursor);
  _action = Query;
}



void Map2DView::startPanMode()
{
  setCursor(Qt::SizeAllCursor);
  _action = Pan;
}



void Map2DView::startZoomAreaMode()
{
  setCursor(Qt::CrossCursor);
  _action = ZoomByRectangle;
}



void Map2DView::startSelectMode()
{
  setCursor(Qt::ArrowCursor);
}



double Map2DView::pixelsToWorldUnits(
         double amount) const
{
  return ::pixelsToWorldUnits(dataObject().map2DScale(),
         dataObject().map2DZoom(), amount);
}



QPointF Map2DView::pixelsToWorldUnits(
         QPointF const& amount) const
{
  return QPointF(pixelsToWorldUnits(amount.x()),
         pixelsToWorldUnits(amount.y()));
}



double Map2DView::worldUnitsToPixels(
         double amount) const
{
  return ::worldUnitsToPixels(dataObject().map2DScale(),
         dataObject().map2DZoom(), amount);
}



QPointF Map2DView::worldUnitsToPixels(
         QPointF const& amount) const
{
  return QPointF(worldUnitsToPixels(amount.x()),
         worldUnitsToPixels(amount.y()));
}



//! Draws a crosshair on the map.
/*!
  \warning   This function can only be called while in a paint event.
  \warning   It is assumed here that the data space contains spatial dimensions,
             which need not be the case.

  A crosshair is only drawn if the data space of the data object is not empty.
*/
void Map2DView::drawCrossHair()
{
  dal::DataSpace const& space(dataObject().dataSpace());
  dal::DataSpaceAddress const& address(dataObject().dataSpaceAddress());

  if(space.hasSpace()) {
    size_t index = space.indexOf(dal::Space);

    if(address.isValid(index)) {
      dal::SpatialCoordinate const& spatialCoordinates(
         address.coordinate<dal::SpatialCoordinate>(index));

      QPointF pos;

      if(map(spatialCoordinates.x(), spatialCoordinates.y(), pos)) {
        QPainter painter(this);
        painter.setPen(palette().color(QPalette::WindowText));
        painter.drawLine(pos.x(), 1.0, pos.x(), height());
        painter.drawLine(1.0, pos.y(), width(), pos.y());
      }
    }
  }
}



//! Draws a zoom rectangle on the map.
/*!
  \warning   This function can only be called while in a paint event.

  A zoom rectangle is only drawn when the mouse has been dragged on the map,
  while in 'zoom area' mode.
*/
void Map2DView::drawZoomRectangle(
         QRect const& rectangle)
{
  if(_action == ZoomByRectangle && _mapViewMouseTarget.moved()) {
    QPainter painter(this);
    painter.setPen(palette().color(QPalette::WindowText));
    painter.drawRect(rectangle);
  }
}



//! Query the map at location \a pos.
/*!
  \param     pos Pixel location to query.

  This triggers an update of the data object.
*/
void Map2DView::queryMap(
         QPoint const& pos)
{
  double x, y;

  if(map(pos, &x, &y)) {
    dataObject().setXY(x, y, true);
  }
  else {
    dataObject().unsetCoordinates(dal::Space, true);
  }
}



void Map2DView::mousePressEvent(
         QMouseEvent* event)
{
  _mapViewMouseTarget.press(event->pos());

  if(event->modifiers() & Qt::ShiftModifier) {
    if(event->button() == Qt::LeftButton) {
      startZoomAreaMode();
    }
  }
  else if(event->modifiers() & Qt::ControlModifier) {
    if(event->button() == Qt::LeftButton) {
      startQueryMode();
      queryMap(event->pos());
    }
  }
  else if(event->modifiers() & Qt::AltModifier) {
  }
  else {
    // No, it may be that we are going to Pan or ZoomByRectangle. In that case
    // we don't want the spatial cursor to change.
    // if(event->button() == Qt::LeftButton) {
    //   queryMap(event->pos());
    // }
  }

  event->accept();
}



QRect Map2DView::zoomRectangle() const
{
  return QRect(
    // Upper left point.
    std::min(_mapViewMouseTarget.pressPosition().x(),
         _mapViewMouseTarget.movePosition().x()),
    std::min(_mapViewMouseTarget.pressPosition().y(),
         _mapViewMouseTarget.movePosition().y()),
    // Width and height.
    std::abs(_mapViewMouseTarget.movement().x()),
          std::abs(_mapViewMouseTarget.movement().y()));
}



void Map2DView::mouseReleaseEvent(
         QMouseEvent* event)
{
  if(event->modifiers() & Qt::ShiftModifier) {
    if((event->button() == Qt::LeftButton)) {
      // Reset the mouse target, otherwise the zoom rectangle will be shown.
      QRect rectangle(zoomRectangle());
      _mapViewMouseTarget.initialize();
      zoomByRectangle(rectangle);
    }
  }
  else if(event->modifiers() & Qt::ControlModifier) {
  }
  else if(event->modifiers() & Qt::AltModifier) {
  }
  else {
    if((event->button() == Qt::LeftButton) && _action == Query) {
      queryMap(event->pos());
    }
  }

  startQueryMode();
  event->accept();
}



void Map2DView::mouseDoubleClickEvent(
         QMouseEvent* event)
{
  assert(!_mapViewMouseTarget.moved());

  if(event->modifiers() & Qt::ShiftModifier) {
  }
  else if(event->modifiers() & Qt::ControlModifier) {
  }
  else if(event->modifiers() & Qt::AltModifier) {
  }
  else {
    if(event->button() == Qt::LeftButton) {
      // This makes the crosshair disappear.
      dataObject().unsetCoordinates(dal::Space, false);

      // Zoom in to current mouse position.
      double dx = (width() / 2.0) - _mapViewMouseTarget.pressPosition().x();
      double dy = (height() / 2.0) - _mapViewMouseTarget.pressPosition().y();

      dataObject().map2DMoveBy(pixelsToWorldUnits(dx), pixelsToWorldUnits(dy),
         false);
      dataObject().map2DZoomBy(1.50, true);
    }
  }

  event->accept();
}



void Map2DView::mouseMoveEvent(
         QMouseEvent* event)
{
  _mapViewMouseTarget.move(event->pos());

  // Moving against the side of the screen generates move events while the
  // mouse coordinates don't change.
  if(!_mapViewMouseTarget.moved()) {
    return;
  }

  if(event->modifiers() & Qt::ShiftModifier) {
    if(event->buttons() & Qt::LeftButton) {
      startZoomAreaMode();
      repaint();
    }
  }
  else if(event->modifiers() & Qt::ControlModifier) {
    if(event->buttons() & Qt::LeftButton) {
      startQueryMode();
      queryMap(event->pos());
    }
  }
  else if(event->modifiers() & Qt::AltModifier) {
  }
  else {
    if(event->buttons() & Qt::LeftButton) {
      startPanMode();
      dataObject().map2DMoveBy(
            pixelsToWorldUnits(_mapViewMouseTarget.movement().x()),
            pixelsToWorldUnits(_mapViewMouseTarget.movement().y()),
            true);

      // Done using mouse target object. Reset as if mouse was pressed at
      // current location.
      _mapViewMouseTarget.press(event->pos());
      assert(!_mapViewMouseTarget.moved());
    }
  }

  event->accept();
}



void Map2DView::wheelEvent(
         QWheelEvent* event)
{
  if(event->modifiers() & Qt::ShiftModifier) {
  }
  else if(event->modifiers() & Qt::ControlModifier) {
  }
  else if(event->modifiers() & Qt::AltModifier) {
  }
  else {
    int nrDegrees = event->delta() / 8;
    double fraction = nrDegrees / 360.0;

    if(event->orientation() == Qt::Vertical) {
      // Zoom in to current mouse position.
      dataObject().map2DZoomBy(1.0 + fraction, true);
    }
  }

  event->accept();
}



void Map2DView::keyPressEvent(
         QKeyEvent* event)
{
  if(event->modifiers() & Qt::ShiftModifier) {
    switch(event->key()) {
      case Qt::Key_Plus: {
        // Zoom map in.
        dataObject().map2DZoomBy(1.10, true);
        break;
      }
      default: {
        event->ignore();
        break;
      }
    }
  }
  else if(event->modifiers() & Qt::ControlModifier) {

    switch(event->key()) {

/*
#ifdef WIN32
      // yepyep
      // this sucks: one control sets state and key to control
      case Qt::Key_Control: {
        saveMode();
        startZoomAreaMode();
        break;
      }
#endif
*/

      case Qt::Key_J: {
        // Zoom map out.
        // zoomBy(0.90);
        dataObject().map2DZoomBy(0.90, true);
        break;
      }

      case Qt::Key_K: {
        // Zoom map in.
        // zoomBy(1.10);
        dataObject().map2DZoomBy(1.10, true);
        break;
      }

      default: {
        event->ignore();
        break;
      }
    }
  }
  else if(event->modifiers() & Qt::AltModifier) {
    event->ignore();
  }
  else {

    switch(event->key()) {

      case Qt::Key_Plus: {
        // Zoom map in.
        // zoomBy(1.10);
        dataObject().map2DZoomBy(1.10, true);
        break;
      }

      case Qt::Key_Minus: {
        // Zoom map out.
        // zoomBy(0.90);
        dataObject().map2DZoomBy(0.90, true);
        break;
      }

      case Qt::Key_Left:
      case Qt::Key_H: {
        // Move map right.
        dataObject().map2DMoveBy(pixelsToWorldUnits(10.0), 0.0, true);
        break;
      }

      case Qt::Key_Down:
      case Qt::Key_J: {
        // Move map up.
        dataObject().map2DMoveBy(0.0, pixelsToWorldUnits(-10.0), true);
        break;
      }

      case Qt::Key_Up:
      case Qt::Key_K: {
        // Move map down.
        dataObject().map2DMoveBy(0.0, pixelsToWorldUnits(10.0), true);
        break;
      }

      case Qt::Key_Right:
      case Qt::Key_L: {
        // Move map left.
        dataObject().map2DMoveBy(pixelsToWorldUnits(-10.0), 0.0, true);
        break;
      }

      case Qt::Key_R: {
        resetMapView();
        break;
      }

/*
      case Qt::Key_Shift: {
        saveMode();
        startPanMode();
        break;
      }

      case Qt::Key_Control: {
        saveMode();
        startZoomAreaMode();
        break;
      }
*/

      default: {
        event->ignore();
        break;
      }
    }
  }
}



void Map2DView::keyReleaseEvent(
         QKeyEvent* event)
{
  // Handle Map2DView specific key releases here.
  // if(event->modifiers() & Qt::ShiftModifier) {
  //   _mapViewMouseTarget.press(_mapViewMouseTarget.movePosition());
  //   startQueryMode();
  // }
  // else if(event->modifiers() & Qt::AltModifier) {
  // }
  // else if(event->modifiers() & Qt::ControlModifier) {
  //   _mapViewMouseTarget.press(_mapViewMouseTarget.movePosition());
  //   startQueryMode();
  // }
  // else {
  // }

  event->ignore();
}



void Map2DView::resetMapView()
{
  // Reset all transformations. First zoom, than translate!
  zoomAll();
  dataObject().setMap2DZoom(1.0, false);
  dataObject().map2DMoveBy(-dataObject().map2DOffset(), false);
  dataObject().notify();
}



//! Determines and sets the scale which results in the whole map to be shown.
/*!
  \sa        setScale(double)

  If one of the sides of the widget or the clone are 1 (widget, see Qt's QRect
  docs) or 0.0 (clone), then the scale will be set to 0.0.
*/
void Map2DView::zoomAll()
{
  double scale = 0.0;

  dal::SpaceDimensions const& dimensions(dataObject().envelope());
  double longitudinalExtent = dimensions.longitudinalExtent();
  double latitudinalExtent = dimensions.latitudinalExtent();

  if(width() > 1 && height() > 1 &&
         longitudinalExtent > 0.0 && latitudinalExtent > 0.0) {

    // Determine the relation between the width and height of the view.
    double relView = static_cast<double>(width()) / height();

    // Determine the relation between the width and height of the clone.
    double relClone = longitudinalExtent / latitudinalExtent;

    // Compare both relations and determine the scale.
    scale = relView <= relClone ? longitudinalExtent / width()
                                : latitudinalExtent / height();
  }

  dataObject().setMap2DScale(scale, false);
}



void Map2DView::zoomByRectangle(
         QRect const& rectangle) const
{
  if(rectangle.isEmpty()) {
    return;
  }

  // Center map around center zoom rectangle.
  QPointF movement(pixelsToWorldUnits(
         QPointF(this->rect().center()) - rectangle.center()));
  dataObject().map2DMoveBy(movement, false);

  // Zoom into the zoom rectangle.
  double scale = MIN(
       ABS(static_cast<double>(width()) / rectangle.width()),
       ABS(static_cast<double>(height()) / rectangle.height()));
  dataObject().map2DZoomBy(scale, false);

  dataObject().notify();
}



void Map2DView::deleteScene(
         QRectF const& area)
{
  // if(buffer().size().isValid()) {
  if(!buffer().isNull()) {
    QPainter p(&buffer());
    p.setPen(palette().color(QPalette::Window));
    p.setBrush(palette().color(QPalette::Window));
    p.drawRect(area);
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void Map2DView::createScene(
         std::vector<DataGuide> const& dataGuides,
         QRectF const& area)
{
  if(dal::comparable<double>(dataObject().map2DScale(), 0.0)) {
    zoomAll();
  }

  std::vector<MapDrawer*> drawers;

  for(std::vector<DataGuide>::const_iterator it = dataGuides.begin();
         it != dataGuides.end(); ++it) {
    if(dataObject().isEnabled(*it)) {
      DataGuide guide = *it;
      assert(dataObject().isValid(guide));

      switch(guide.type()) {
        case geo::STACK: {
          Raster const* raster = &dataObject().rasterDataSources().data(guide);

          switch(guide.valueScale()) {
            case VS_BOOLEAN: {
              drawers.push_back(new BooleanRasterDrawer(raster,
                   dataObject().envelope(),
                   dataObject().properties().booleanDrawProperties(guide)));

              break;
            }
            case VS_NOMINAL: {
              drawers.push_back(new NominalRasterDrawer(raster,
                   dataObject().envelope(),
                   dataObject().properties().nominalDrawProperties(guide)));

              break;
            }
            case VS_ORDINAL: {
              drawers.push_back(new OrdinalRasterDrawer(raster,
                   dataObject().envelope(),
                   dataObject().properties().ordinalDrawProperties(guide)));

              break;
            }
            case VS_SCALAR: {
              RangeDrawProps const& props =
                   dataObject().properties().rangeDrawProperties(guide);

              if(dataObject().hasSelectedValue() &&
                   dataObject().dataSpace(guide).hasCumProbabilities() &&
                   props.probabilityScale() ==
                        RangeDrawProps::ExceedanceProbabilities) {
                drawers.push_back(new ExceedanceProbabilityRasterDrawer(
                   raster, dataObject().envelope(), props));
              }
              else {
                drawers.push_back(new ScalarRasterDrawer(raster,
                    dataObject().envelope(), props));
              }

              break;
            }
            case VS_DIRECTION: {
              drawers.push_back(new DirectionalRasterDrawer(raster,
                   dataObject().envelope(),
                   dataObject().properties().rangeDrawProperties(guide)));

              break;
            }
            case VS_LDD: {
              drawers.push_back(new LddRasterDrawer(raster,
                   dataObject().envelope(),
                   dataObject().properties().lddDrawProperties(guide),
                   palette().color(QPalette::WindowText)));

              break;
            }
            default: {
              assert(false);
              break;
            }
          }

          break;
        }
        case geo::FEATURE: {
          FeatureLayer const* layer =
              &dataObject().featureDataSources().data(guide);

          switch(guide.valueScale()) {
            /// FEATURE handle other vs's.
            case VS_SCALAR: {
              RangeDrawProps const& props =
                   dataObject().properties().rangeDrawProperties(guide);

              if(dataObject().hasSelectedValue() &&
                    dataObject().dataSpace(guide).hasCumProbabilities() &&
                    props.probabilityScale() ==
                         RangeDrawProps::ExceedanceProbabilities) {
                drawers.push_back(new ExceedanceProbabilityFeatureLayerDrawer(
                   layer, dataObject().envelope(), props));
              }
              else {
                drawers.push_back(new RangeFeatureLayerDrawer(layer,
                     dataObject().envelope(), props));
              }

              break;
            }
            default: {
              drawers.push_back(new FeatureLayerDrawer(layer,
                   dataObject().envelope()));
              break;
            }
          }

          break;
        }
        case geo::VECTOR: {
          assert(guide.valueScale() == VS_SCALAR);
          drawers.push_back(new VectorDrawer(
            &dataObject().vectorDataSources().data(guide),
            dataObject().envelope(),
            dataObject().properties().rangeDrawProperties(guide)));
          break;
        }
        default: {
          assert(false);
          break;
        }
      }
    }
  }

  deleteScene(area);

  if(!buffer().isNull()) {
    QPainter painter(&buffer());

    typedef std::vector<MapDrawer*>::iterator iterator;

    // try {
      for(iterator it = drawers.begin(); it != drawers.end(); ++it) {
        (*it)->draw(painter, area, anchor(), dataObject().map2DZoom(),
              dataObject().map2DOffset(), dataObject().map2DScale());
      }
    // }
    // catch(com::OutOfRangeException& exception) {
    //   // KDJ: Since Qt4 this code seems not to be executed anymore. Because of
    //   //      the port I updated the implementation of the drawers and maybe
    //   //      that is the reason.
    //   QMessageBox::critical(this, visualisationName().c_str(),
    //        exception.messages().c_str(), QMessageBox::Ok, Qt::NoButton);
    // }

    painter.end();

    for(iterator it = drawers.begin(); it != drawers.end(); ++it) {
      delete *it;
    }
  }
}



void Map2DView::updateBuffer(
         QRectF const& area)
{
  dal::DataSpaceAddress addressWithoutSpace(
         dataObject().dataSpace().eraseCoordinates(
              dataObject().dataSpaceAddress(), dal::Space));

  if(addressWithoutSpace.isValid()) {
    createScene(visualisationEngine().dataGuides(), area);
  }
  else {
    deleteScene(area);
  }
}



//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  This one is used by the code drawing the cross-hair. If the cross-hair is
  drawn OK, than there is no need to change this code.
*/
bool Map2DView::map(
         double x,
         double y,
         QPointF& pos) const
{
  double scale = dataObject().map2DScale();
  double zoom = dataObject().map2DZoom();

  dal::SpaceDimensions const& dimensions(dataObject().envelope());

  if(scale != 0.0 && zoom != 0.0) {
    pos.rx() =
         // Amount of pixels the map is offset from the center.
         worldUnitsToPixels(dataObject().map2DOffset().x()) +
         anchor().x() - // Pixel coordinates of the center.
         // Distance in pixels from center of map to x.
         worldUnitsToPixels(0.5 * dimensions.longitudinalExtent() -
              (x - dimensions.west()));
    pos.ry() =
         // Amount of pixels the map is offset from the center.
         worldUnitsToPixels(dataObject().map2DOffset().y()) +
         anchor().y() - // Pixel coordinate of the center.
         // Distance in pixels from center of map to y.
         worldUnitsToPixels(0.5 * dimensions.latitudinalExtent() -
              (dimensions.north() - y));

    return true;
  }

  return false;
}



//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  This one is used by the code for querying the map. If the values of the
  queried location are correct, than there is no reason to change this code.
*/
bool Map2DView::map(
         QPointF const& pos,
         double* x,
         double* y) const
{
  double scale = dataObject().map2DScale();
  double zoom = dataObject().map2DZoom();

  dal::SpaceDimensions const& dimensions(dataObject().envelope());

  if(scale != 0.0 && zoom != 0.0) {
    *x = pixelsToWorldUnits(pos.x() - anchor().x()) -
         dataObject().map2DOffset().x() +
         (0.5 * dimensions.longitudinalExtent());

    *y = pixelsToWorldUnits(pos.y() - anchor().y()) -
         dataObject().map2DOffset().y() +
         (0.5 * dimensions.latitudinalExtent());

    // x is relative to the left of the raster. Add left of raster.
    // Similar for y.
    *x += dimensions.west();
    *y = dimensions.north() - *y;

    return true;
  }

  return false;
}



void Map2DView::addAttribute(const DataGuide& dataGuide)
{
  testDataGuide(dataGuide);
  visualisationEngine().addAttribute(dataObject(), dataGuide);
}



void Map2DView::clear()
{
  visualisationEngine().clear();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


} // namespace
