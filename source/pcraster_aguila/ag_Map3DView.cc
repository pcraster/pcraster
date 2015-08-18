#include "ag_Map3DView.h"
#include <QMouseEvent>
#include "com_const.h"
#include "geo_DataType.h"
#include "geo_mathlib.h"
#include "ag_DataObject.h"
#include "ag_Map3DObject.h"
#include "ag_VisEngine.h"



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------

namespace ag {

class Map3DViewPrivate
{
public:

  Map3DObject*     d_map3D;

  Map3DViewPrivate()
  {
    d_map3D = new Map3DObject();
    d_map3D->setInitRotation(20.0 * com::DEG2RAD, 30.0 * com::DEG2RAD, 0.0);
  }

  ~Map3DViewPrivate()
  {
    delete d_map3D;
  }

};

}



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

ag::Map3DView::Map3DView(DataObject* object, QWidget* parent)

  : GLVisualisation(object, "Drape View", parent)

{
  d_data = new Map3DViewPrivate();
  addSceneObject(d_data->d_map3D);

  // Supported data types.
  std::vector<geo::DataType> dataTypes;
  dataTypes.push_back(geo::STACK);
  setSupportedDataTypes(dataTypes);

  // Supported value scales.
  std::vector<CSF_VS> valueScales;
  valueScales.push_back(VS_BOOLEAN);
  valueScales.push_back(VS_NOMINAL);
  valueScales.push_back(VS_ORDINAL);
  valueScales.push_back(VS_SCALAR);
  valueScales.push_back(VS_DIRECTION);
  valueScales.push_back(VS_LDD);
  setSupportedValueScales(valueScales);

  // Supported file formats.
  std::vector<com::FileFormatInfo> fileFormats;
  // Has a bug.
  // fileFormats.push_back(com::FileFormatInfo::png());
  fileFormats.push_back(com::FileFormatInfo::eps());
  setSaveAsFileFormats(fileFormats);

  setFocusPolicy(Qt::WheelFocus);
}



ag::Map3DView::~Map3DView()
{
  deleteScene();
  delete d_data;
}



void ag::Map3DView::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void ag::Map3DView::process()
{
  if(visualisationEngine().change() & VisEngine::QUADLENGTH) {
    d_data->d_map3D->setQuadLength(dataObject().quadLength());
  }

  if(visualisationEngine().change() & VisEngine::MAP3DSCALE) {
    d_data->d_map3D->setScale(dataObject().map3DScale());
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

    qglClearColor(dataObject().backgroundColour());
  }

  if(visualisationEngine().change() & VisEngine::OTHERHEIGHT ||
         visualisationEngine().change() & VisEngine::OTHERATTRIB ||
         (visualisationEngine().change() & VisEngine::CURSOR &&
          visualisationEngine().change() & VisEngine::TIME) ||
         visualisationEngine().change() & VisEngine::VISIBILITY ||
         visualisationEngine().change() & VisEngine::QUADLENGTH ||
         visualisationEngine().change() & VisEngine::MAP3DSCALE ||
         visualisationEngine().change() & VisEngine::DRAWPROPS ||
         visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {

    deleteScene();

    if(visualisationEngine().heightDataGuide()) {
      createScene(*visualisationEngine().heightDataGuide(),
         visualisationEngine().dataGuides());

      // If this is the first data set we need to position the camera and
      // set up the projection.
      if(visualisationEngine().change() & VisEngine::OTHERHEIGHT) {
        reset();
        resetViewport(width(), height());
      }
    }
  }
}



void ag::Map3DView::visualise()
{
  if(visualisationEngine().change() & VisEngine::OTHERHEIGHT ||
         visualisationEngine().change() & VisEngine::OTHERATTRIB ||
         (visualisationEngine().change() & VisEngine::CURSOR &&
          visualisationEngine().change() & VisEngine::TIME) ||
         visualisationEngine().change() & VisEngine::VISIBILITY ||
         visualisationEngine().change() & VisEngine::QUADLENGTH ||
         visualisationEngine().change() & VisEngine::MAP3DSCALE ||
         visualisationEngine().change() & VisEngine::DRAWPROPS ||
         visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
    updateGL();
  }

  visualisationEngine().finishedScanning(dataObject());
}



double ag::Map3DView::leftScene() const
{
  return d_data->d_map3D->left();
}



double ag::Map3DView::rightScene() const
{
  return d_data->d_map3D->right();
}



double ag::Map3DView::bottomScene() const
{
  return d_data->d_map3D->bottom();
}



double ag::Map3DView::topScene() const
{
  return d_data->d_map3D->top();
}



double ag::Map3DView::backScene() const
{
  return d_data->d_map3D->back();
}



double ag::Map3DView::frontScene() const
{
  return d_data->d_map3D->front();
}



void ag::Map3DView::createScene(
         ag::DataGuide const& height,
         std::vector<DataGuide> const& dataGuides)
{
  makeCurrent();
  d_data->d_map3D->createScene(dataObject(), height, dataGuides);
  setInitHead(0.0, 0.0, 1.5 * (widthScene() + depthScene()) / 2.0);
  checkForGLErrors();
  setValid(true);
}



void ag::Map3DView::deleteScene()
{
  makeCurrent();
  d_data->d_map3D->deleteScene();

/*
  if(d_data->d_sceneList > 0)
  {
    makeCurrent();
    glDeleteLists(d_data->d_sceneList, 1);
    d_data->d_sceneList = 0;
    d_data->d_left = 0.0;
    d_data->d_right = 0.0;
    d_data->d_top = 0.0;
    d_data->d_bottom = 0.0;
    d_data->d_front = 0.0;
    d_data->d_back = 0.0;
    setDirty();
  }
*/
}



/*
void ag::Map3DView::deleteFishnet()
{
  if(d_data->d_fishnetList > 0)
  {
    makeCurrent();
    glDeleteLists(d_data->d_fishnetList, 1);
    d_data->d_fishnetList = 0;
    setDirty();
  }
}
*/



/*
void ag::Map3DView::setShowFishnet(bool s)
{
  d_data->d_map3D->setShowFishnet(s);
}



bool ag::Map3DView::showFishnet() const
{
  return d_data->d_map3D->showFishnet();
}
*/



/*
void ag::Map3DView::setQuadLength(size_t s)
{
  d_data->d_map3D->setQuadLength(s);
}
*/



/*
size_t ag::Map3DView::quadLength() const
{
  return d_data->d_map3D->quadLength();
}
*/



/*
void ag::Map3DView::setScale(double s)
{
  d_data->d_map3D->setScale(s);
}
*/



/*
double ag::Map3DView::scale() const
{
  return d_data->d_map3D->scale();
}
*/



/*
void ag::Map3DView::rotateScene(GLfloat x, GLfloat y, GLfloat z)
{
  d_data->d_map3D->rotateBy(y, x, z);
}
*/



/*
void ag::Map3DView::setValid(bool s)
{
  d_data->d_valid = s;
}



bool ag::Map3DView::valid() const
{
  return d_data->d_valid && d_data->d_map3D->valid();
}
*/



ag::SceneObject& ag::Map3DView::sceneObject() const
{
  assert(d_data->d_map3D);

  return *d_data->d_map3D;
}



void ag::Map3DView::addAttribute(const DataGuide& dataGuide)
{
  testDataGuide(dataGuide);
  visualisationEngine().addAttribute(dataObject(), dataGuide);
}



void ag::Map3DView::setHeight(const DataGuide& dataGuide)
{
  testDataGuide(dataGuide);
  visualisationEngine().setHeight(dataGuide);
}



int ag::Map3DView::depthOfRenderingContext() const
{
  int depth = 0;
  int bits;
  glGetIntegerv(GL_RED_BITS, &bits);
  depth += bits;
  glGetIntegerv(GL_GREEN_BITS, &bits);
  depth += bits;
  glGetIntegerv(GL_BLUE_BITS, &bits);
  depth += bits;
  glGetIntegerv(GL_ALPHA_BITS, &bits);
  depth += bits;
  return depth;
}



bool ag::Map3DView::doubleBuffer() const
{
  return format().doubleBuffer();
}



void ag::Map3DView::mousePressEvent(QMouseEvent* event)
{
  d_mapViewTarget.press(event->pos());
}



void ag::Map3DView::mouseReleaseEvent(QMouseEvent* event)
{
        event->ignore();
/*
  if(event->state() == Qt::RightButton) {
    if(!d_mapViewTarget.moved()) {
      showMapViewPopup(mapToGlobal(event->pos()));
    }
  }
  */
}



void ag::Map3DView::mouseMoveEvent(QMouseEvent* event)
{
  d_mapViewTarget.move(event->pos());
  QPoint move = d_mapViewTarget.movement();

  if(event->buttons() == Qt::LeftButton) {

    // Changes in x-direction: move aim in x-direction.
    rotateHead(0.0, static_cast<double>(move.x()) / 25 * com::DEG2RAD, 0.0);

    // Changes in y-direction: move head in z-direction.
    moveHead(0.0, 0.0, step() * static_cast<double>(move.y()) / 15);
    updateGL();
  }
  else if(event->buttons() == Qt::RightButton) {

    // Changes in x direction: move aim in x-direction.
    rotateHead(0.0, static_cast<double>(move.x()) / 25 * com::DEG2RAD, 0.0);
    // Changes in y direction: move aim in z-direction.
    rotateHead(static_cast<double>(move.y()) / 25 * com::DEG2RAD, 0.0, 0.0);
    updateGL();
  }
  else if(event->buttons() & Qt::LeftButton &&
       event->buttons() & Qt::RightButton) {

    moveHead(step() * static_cast<double>(move.x()) / 10, 0.0, 0.0);
    moveHead(0.0, -step() * static_cast<double>(move.y()) / 10, 0.0);
    updateGL();
  }

  // Reset press position.
  d_mapViewTarget.press(event->pos());
}



void ag::Map3DView::keyPressEvent(QKeyEvent* event)
{
  SceneView::keyPressEvent(event);

  if(event->modifiers() & Qt::ShiftModifier) {
    switch(event->key()) {
      case Qt::Key_Q: {
        // Make the quadlength larger.
        dataObject().setQuadLength(dataObject().quadLength() + 2, true);
        break;
      }

      default: {
        event->ignore();
        break;
      }
    }
  }
  else if(event->modifiers() & Qt::ControlModifier) {
    event->ignore();
  }
  else if(event->modifiers() & Qt::AltModifier) {
    event->ignore();
  }
  else {
    switch(event->key()) {
      case Qt::Key_Plus: {
        dataObject().setMap3DScale(dataObject().map3DScale() + 0.1, true);
        break;
      }

      case Qt::Key_Minus: {
        dataObject().setMap3DScale(dataObject().map3DScale() - 0.1, true);
        break;
      }

      case Qt::Key_Q: {
        // Make the quadlength smaller.
        if(dataObject().quadLength() > 1) {
          dataObject().setQuadLength(dataObject().quadLength() - 2, true);
        }
        break;
      }

      case Qt::Key_R: {
        // Reset rotations, aim and positions.
        reset();
        /*
        if(d_data->d_mapViewDlg) {
          d_data->d_mapViewDlg->updateWidgets();
        }
        */
        updateGL();
        break;
      }

      default: {
        event->ignore();
        break;
      }
    }
  }
}



/*
void ag::Map3DView::showMapViewPopup(const QPoint& point)
{
  if(!d_mapViewPopup) {
    d_mapViewPopup = new QPopupMenu();
    d_mapViewPopup->insertItem("Properties...", this, SLOT(showMapViewDlg()));
  }

  d_mapViewPopup->popup(point);
}



void ag::Map3DView::showMapViewDlg()
{
  if(d_mapViewDlg) {
    if(!d_mapViewDlg->isVisible()) {
      d_mapViewDlg->show();
    }
    else {
      d_mapViewDlg->raise();
    }
  }
  else {
    d_mapViewDlg = new Map3DViewOptionsDlg(this, "map 3d view options dialog");
    d_mapViewDlg->setPalette(qApp->palette());
    d_mapViewDlg->setCaption(caption());
    if(icon()) {
      d_mapViewDlg->setIcon(*icon());
    }
    d_mapViewDlg->setIconText(iconText());
    connect(d_mapViewDlg, SIGNAL(recreateScene()), this, SLOT(recreateScene()));
    d_mapViewDlg->show();
    d_mapViewDlg->setMaximumSize(d_mapViewDlg->sizeHint());
  }
}
*/


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------

