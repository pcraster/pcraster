#include "ag_SceneView.h"
#include <cmath>
#include <vector>
#include <GL/glu.h>
#include <boost/format.hpp>
#include <QKeyEvent>
#include <QPoint>
#include "com_const.h"
#include "com_exception.h"
#include "com_matrix.h"
#include "com_vector.h"
#include "ag_Camera.h"
#include "ag_Feedback.h"



//------------------------------------------------------------------------------

namespace ag {

typedef std::vector<ag::SceneObject*>::iterator so_it;

class SceneViewPrivate
{
public:
  bool             d_dirty;            // If d_dirty is true, redraw is req.
  bool             d_valid;            // If d_valid is false, recreate is req.

  ag::Camera*     d_userCamera;       // Controllable camera.
  ag::Camera*     d_staticCamera;     // Static camera.
  std::vector<ag::SceneObject*> d_sceneObjects;
  GLfloat          d_step;
  GLfloat          d_angle;

  SceneViewPrivate()
    : d_dirty(false), d_valid(true), d_staticCamera(0),
      d_step(0.0), d_angle(5.0)
  {
    d_userCamera = new ag::Camera();
  }

  ~SceneViewPrivate()
  {
    delete d_userCamera;
    if(d_staticCamera) {
      delete d_staticCamera;
    }
  }
};

}



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

GLfloat ag::SceneView::calcFOV(double s, double d)
{
  double rad = 2.0 * std::atan2(s / 2.0, d);
  double deg = (180.0 * rad) / M_PI;

  return static_cast<GLfloat>(deg);
}



// Scene is positioned around the origin.
GLfloat ag::SceneView::calcFOV(double w, double d, double h, double x, double y,
                   double z)
{
  //  1. The radius of bounding sphere is the distance from center of the
  //     bounding box to any corner.
  //  2. Calculate the distance between the center of the bounding sphere to
  //     the viewpoint.

  double r    = std::sqrt(std::pow(0.5 * w, 2.0) + std::pow(0.5 * d, 2.0) +
                   std::pow(0.5 * h, 2.0));
  double dist = std::sqrt(std::pow(x, 2.0) + std::pow(y, 2.0) +
                   std::pow(z, 2.0));
  double rad  = 2.0 * std::atan2(r, dist);
  double deg  = (180.0 * rad) / M_PI;

  return static_cast<GLfloat>(deg);
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructs a SceneView object.
/*!
*/
ag::SceneView::SceneView(QWidget *p)

  : QGLWidget(p),
    d_data(new SceneViewPrivate())

{
  // Make sure that this rendering context is valid.
  assert(isValid());
}



//! Destructs a SceneView object.
/*!
*/
ag::SceneView::~SceneView()
{
  delete d_data;
}



void ag::SceneView::initializeGL()
{
  // 10. Set the clear colour state variable to the background colour.
  // 20. Since we enable non-uniform scaling we have to enable GL_NORMALIZE
  //     even though it's expensive. If we only did uniform scaling we could
  //     have enabled GL_RESCALE_NORMAL which is faster. See also p. 66 of the
  //     OpenGL Programming Guide, 3rd.
  // 30. Enable the depth test for hidden surface removal.
  // 40. Configure and install a light source.
  // 99. Check for any errors detected by the opengl lib.

  // PORT: Updated code but not tested yet.
  // qglClearColor(colorGroup().background());                               // 10.
  qglClearColor(palette().window().color());                              // 10.
  glShadeModel(GL_FLAT);
  glEnable(GL_DEPTH_TEST);                                                // 30.
  glEnable(GL_TEXTURE_2D);   // Ldd drawer.
  glEnable(GL_BLEND);        // Ldd drawer.

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glPixelStorei(GL_PACK_ALIGNMENT, 1);

  GLfloat pos[] = { 0.0, 0.0, 0.0, 1.0 };                                 // 40.
  GLfloat light_diffuse[] = { 1.0, 1.0, 1.0, 1.0 };
  GLfloat light_specular[] = { 1.0, 1.0, 1.0, 1.0 };

  glLightfv(GL_LIGHT0, GL_POSITION, pos);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, light_specular);
  setShowLight(true);        // Ride the lightning!

  checkForGLErrors();                                                     // 99.
}



void ag::SceneView::resizeGL(int w, int h)
{
  resetViewport(w, h);
}



//! Resets the projection matrix.
/*!
  \param     w Width of the viewport in pixels.
  \param     h Height of the viewport in pixels.
  \sa        resizeGL(int, int)

  Call this function if the size of the viewport changed or if data was
  previously unavailable but now is.

  First this function calls makeCurrent().
*/
void ag::SceneView::resetViewport(int w, int h)
{
  // 10. Resize the opengl viewport to the new size of the widget.
  // 30. Clear the matrix.
  // 35. Projection transformation: choosing the lens.
  // 99. Check for any errors detected by the opengl lib.
  makeCurrent();
  glViewport(0, 0, static_cast<GLsizei>(w), static_cast<GLsizei>(h));     // 10.
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();                                                       // 30.

  // Yes! We have data.
  if(widthScene() > 0.0 && depthScene() > 0.0 && heightScene() > 0.0)
  {
    GLfloat fovy = calcFOV(widthScene(), depthScene(), heightScene(),     // 35.
                   d_data->d_userCamera->x(), d_data->d_userCamera->y(),
                   d_data->d_userCamera->z());
    GLfloat aspect = static_cast<GLfloat>(w) / h;
    GLfloat d = std::sqrt(widthScene() * widthScene() +
                   depthScene() + depthScene());
    gluPerspective(fovy, aspect, 0.05 * d, 5.0 * d);
    d_data->d_userCamera->setSize(0.1 * d, 0.3 * d, 0.1 * d);
  }

  d_data->d_dirty = true;
  glMatrixMode(GL_MODELVIEW);
  checkForGLErrors();                                                     // 99.
}



void ag::SceneView::paintGL()
{
  if(dirty()) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();

    if(d_data->d_staticCamera) {
      d_data->d_staticCamera->apply();
      d_data->d_userCamera->render();
    }
    else {
      d_data->d_userCamera->apply();
    }

    std::vector<ag::SceneObject*>::iterator it;
    for(it = d_data->d_sceneObjects.begin();
                   it != d_data->d_sceneObjects.end(); ++it) {
      (*it)->render();
    }

    glFlush();                                                            // 98.
    d_data->d_dirty = false;
    checkForGLErrors();                                                   // 99.
  }
}

/*
    if(!d_data->d_showBBox) {
      glPushMatrix();
        glMultMatrixd(d_data->d_boundingBox->rotation().data());
        glTranslatef(-d_data->d_boundingBox->x(), -d_data->d_boundingBox->y(),
                   -d_data->d_boundingBox->z());
        callScene();
      glPopMatrix();
    }
    else {
      d_data->d_boundingBox->setSize(widthScene(), depthScene(), heightScene());
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      bool light = showLight();
      if(!light) {
        glEnable(GL_LIGHTING);
        glEnable(GL_LIGHT0);
      }
      d_data->d_boundingBox->render();
      if(!light) {
        glDisable(GL_LIGHTING);
        glDisable(GL_LIGHT0);
      }
    }
*/



//! Checks if the OpenGL library has detected an error.
/*!
  \exception com::Exception The OpenGL library detected an error.
  \warning   makeCurrent() must have been called already.
*/
void ag::SceneView::checkForGLErrors()
{
  static GLenum glErrCode;
  static const GLubyte *glErrString;

  if((glErrCode = glGetError()) != GL_NO_ERROR) {
    glErrString = gluErrorString(glErrCode);

    std::string m = (boost::format("error reported by OpenGL library: %1%")
         % glErrString).str();
    throw com::Exception(m);
  }
}



//! Resets all.
/*!
  This function resets the camera and all scene objects.
*/
void ag::SceneView::reset()
{
  d_data->d_userCamera->reset();

  std::vector<ag::SceneObject*>::iterator it;
  for(it = d_data->d_sceneObjects.begin();
                   it != d_data->d_sceneObjects.end(); ++it) {
    (*it)->reset();
  }
}



/*!
  \param     x X-coordinate of new initial head position.
  \param     y Y-coordinate of new initial head position.
  \param     z Z-coordinate of new initial head position.

  The initial head position is used as the head position after the scene is
  reset().
*/
void ag::SceneView::setInitHead(double x, double y, double z)
{
  d_data->d_userCamera->setInitPosition(x, y, z);
  d_data->d_step = std::sqrt(x * x + y * y + z * z) / 20;
}



void ag::SceneView::setHead(double x, double y, double z)
{
  d_data->d_userCamera->setPosition(x, y, z);
}



void ag::SceneView::setInitHeadRotation(double rx, double ry, double rz)
{
  d_data->d_userCamera->setInitRotation(ry, rx, rz);
}



void ag::SceneView::setHeadRotation(double rx, double ry, double rz)
{
  d_data->d_userCamera->setRotation(ry, rx, rz);
}



void ag::SceneView::rotateHead(double rx, double ry, double rz)
{
  d_data->d_userCamera->rotateBy(ry, rx, rz);
}



// Moves head in the direction of the head rotation.
void ag::SceneView::moveHead(double mx, double my, double mz)
{
  GLfloat m[16];
  d_data->d_userCamera->matrix(m);
  com::Matrix<GLfloat> rm(4, 4, m);
  com::Matrix<GLfloat> p(4, 1);

  p.setElement(1, 1, mx);
  p.setElement(2, 1, my);
  p.setElement(3, 1, mz);
  p = rm * p;
  mx = p.element(1, 1);
  my = p.element(2, 1);
  mz = p.element(3, 1);

  d_data->d_userCamera->moveBy(mx, my, mz);
}



double ag::SceneView::widthScene() const
{
  return std::abs(rightScene() - leftScene());
}



double ag::SceneView::depthScene() const
{
  return std::abs(backScene() - frontScene());
}



double ag::SceneView::heightScene() const
{
  return std::abs(topScene() - bottomScene());
}



void ag::SceneView::setShadeModel(GLenum m)
{
  makeCurrent();

#ifdef DEBUG_DEVELOP
  assert(m != GL_INVALID_ENUM);
#endif

  if(shadeModel() != m)
  {
    glShadeModel(m);
    d_data->d_dirty = true;
  }
}



GLenum ag::SceneView::shadeModel() const
{
  // Since makeCurrent is a non-const member we have to trick the compiler.
  SceneView * const localThis = const_cast<SceneView * const>(this);
  localThis->makeCurrent();

  int m;
  glGetIntegerv(GL_SHADE_MODEL, &m);

#ifdef DEBUG_DEVELOP
  assert(static_cast<GLenum>(m) != GL_INVALID_ENUM);
#endif

  return static_cast<GLenum>(m);
}



void ag::SceneView::setShowLight(bool s)
{
  makeCurrent();

  if(s != showLight()) {
    if(s) {
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
    }
    else {
      glDisable(GL_LIGHTING);
      glDisable(GL_LIGHT0);
    }

    d_data->d_dirty = true;
  }
}



bool ag::SceneView::showLight() const
{
  // Since makeCurrent is a non-const member we have to trick the compiler.
  SceneView * const localThis = const_cast<SceneView * const>(this);
  localThis->makeCurrent();

  GLboolean s;
  glGetBooleanv(GL_LIGHT0, &s);
  return s == GL_TRUE;
}



void ag::SceneView::setDirty()
{
  d_data->d_dirty = true;
}



void ag::SceneView::setValid(bool s)
{
  d_data->d_valid = s;
}



GLfloat ag::SceneView::step() const
{
  return d_data->d_step;
}



GLfloat ag::SceneView::angle() const
{
  return d_data->d_angle;
}



bool ag::SceneView::dirty() const
{
  bool dirty = false;
  dirty = dirty || d_data->d_dirty || d_data->d_userCamera->dirty();

  // if(dirty) {
  //   cout << "d_data->d_dirty || d_data->d_userCamera->dirty()" << endl;
  // }
  // else {

  std::vector<ag::SceneObject*>::iterator it;
  for(it = d_data->d_sceneObjects.begin();
                   it != d_data->d_sceneObjects.end(); ++it) {
    dirty = dirty || (*it)->dirty();
  }
    // if(dirty) {
    //   cout << "(*it)->dirty()" << endl;
    // }
  // }

  return dirty;
}



void ag::SceneView::keyPressEvent(QKeyEvent* event)
{
  if(event->modifiers() & Qt::ShiftModifier) {
    switch(event->key()) {
      case Qt::Key_Up: {
        // Move camera to the front.
        moveHead(0.0, 0.0, -step());
        updateGL();
        break;
      }

      case Qt::Key_Down: {
        // Move camera to the back.
        moveHead(0.0, 0.0, step());
        updateGL();
        break;
      }

      case Qt::Key_Left: {
        // Move camera to the left.
        moveHead(-step(), 0.0, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_Right: {
        // Move camera to the right.
        moveHead(step(), 0.0, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_H: {
        // Move camera to the left.
        moveHead(-step(), 0.0, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_L: {
        // Move camera to the right.
        moveHead(step(), 0.0, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_K: {
        // Move camera to the top.
        moveHead(0.0, step(), 0.0);
        updateGL();
        break;
      }

      case Qt::Key_J: {
        // Move camera to the bottom.
        moveHead(0.0, -step(), 0.0);
        updateGL();
        break;
      }
    }
  }
  else if(event->modifiers() & Qt::ControlModifier) {
    switch(event->key()) {
      case Qt::Key_K: {
        // Move camera to the front.
        moveHead(0.0, 0.0, -step());
        updateGL();
        break;
      }

      case Qt::Key_J: {
        // Move camera to the back.
        moveHead(0.0, 0.0, step());
        updateGL();
        break;
      }
    }
  }
  else {
    switch(event->key()) {
      case Qt::Key_0: {
        // Install user camera.
        installCamera(SceneView::USER);
        updateGL();
        break;
      }

      case Qt::Key_2: {
        // Install static front camera.
        installCamera(SceneView::FRONT);
        updateGL();
        break;
      }

      case Qt::Key_4: {
        // Install static left camera.
        installCamera(SceneView::LEFT);
        updateGL();
        break;
      }

      case Qt::Key_5: {
        // Install static top camera.
        installCamera(SceneView::TOP);
        updateGL();
        break;
      }

      case Qt::Key_6: {
        // Install static right camera.
        installCamera(SceneView::RIGHT);
        updateGL();
        break;
      }

      case Qt::Key_8: {
        // Install static back camera.
        installCamera(SceneView::BACK);
        updateGL();
        break;
      }

      case Qt::Key_Left: {
        // Rotate scene around the z-axes.
        rotateScene(0.0, -2.0 * com::DEG2RAD, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_Right: {
        // Rotate scene around the z-axes.
        rotateScene(0.0, 2.0 * com::DEG2RAD, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_Up: {
        // Rotate scene around the x-axes.
        rotateScene(-2.0 * com::DEG2RAD, 0.0, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_Down: {
        // Rotate scene around the x-axes.
        rotateScene(2.0 * com::DEG2RAD, 0.0, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_H: {
        // Aim camera left.
        rotateHead(0.0, -2.0 * com::DEG2RAD, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_J: {
        // Aim camera down. 
        rotateHead(2.0 * com::DEG2RAD, 0.0, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_K: {
        // Aim camera up.
        rotateHead(-2.0 * com::DEG2RAD, 0.0, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_L: {
        // Aim camera right.
        rotateHead(0.0, 2.0 * com::DEG2RAD, 0.0);
        updateGL();
        break;
      }

      case Qt::Key_M: {
        // Roll camera
        rotateHead(0.0, 0.0, -2.0 * com::DEG2RAD);
        updateGL();
        break;
      }

      case Qt::Key_N: {
        // Roll camera
        rotateHead(0.0, 0.0, 2.0 * com::DEG2RAD);
        updateGL();
        break;
      }
    }
  }
}



bool ag::SceneView::valid() const
{
  return d_data->d_valid && sceneObject().valid();
}



void ag::SceneView::addSceneObject(ag::SceneObject* s)
{
  assert(s);
  d_data->d_sceneObjects.push_back(s);
  d_data->d_dirty = true;
}



void ag::SceneView::removeSceneObject(ag::SceneObject* s)
{
  assert(s);
  so_it it = std::find(d_data->d_sceneObjects.begin(),
                   d_data->d_sceneObjects.end(), s);
  assert(it != d_data->d_sceneObjects.end());
  d_data->d_sceneObjects.erase(it);
  d_data->d_dirty = true;
}



void ag::SceneView::installCamera(Camera c)
{
  //          yaw | pitch | roll
  // TOP  :   0.0    90.0    0.0
  // FRONT:   0.0     0.0    0.0
  // LEFT :  90.0     0.0    0.0
  // BACK : 190.0     0.0    0.0
  // RIGHT: 270.0     0.0    0.0

  if(c == USER) {
    if(d_data->d_staticCamera) {
      delete d_data->d_staticCamera;
      d_data->d_staticCamera = 0;
      d_data->d_dirty = true;
    }
  }
  else {
    GLfloat x, y, z, yaw, pitch, roll;
    GLfloat d = 4.0 * std::sqrt(widthScene() * widthScene() +
                   depthScene() + depthScene());

    if(c == TOP) {
      x     = 0.0;
      y     = d;
      z     = 0.0;
      yaw   = 0.0;
      pitch = 90.0 * com::DEG2RAD;
      roll  = 0.0;
    }
    else if(c == FRONT) {
      x     = 0.0;
      y     = 0.0;
      z     = d;
      yaw   = 0.0;
      pitch = 0.0;
      roll  = 0.0;
    }
    else if(c == LEFT) {
      x     = -d;
      y     = 0.0;
      z     = 0.0;
      yaw   = 90.0 * com::DEG2RAD;
      pitch = 0.0;
      roll  = 0.0;
    }
    else if(c == BACK) {
      x     = 0.0;
      y     = 0.0;
      z     = -d;
      yaw   = 180.0 * com::DEG2RAD;
      pitch = 0.0;
      roll  = 0.0;
    }
    else if(c == RIGHT) {
      x     = d;
      y     = 0.0;
      z     = 0.0;
      yaw   = 270.0 * com::DEG2RAD;
      pitch = 0.0;
      roll  = 0.0;
    }
    else {
      assert(false);
      x = y = z = yaw = pitch = roll = -99999.99999f;       // Never reached.
    }

    if(!d_data->d_staticCamera) {
      d_data->d_staticCamera = new ag::Camera();
    }

    if(d_data->d_staticCamera->x() != x || d_data->d_staticCamera->y() != y ||
                   d_data->d_staticCamera->z() != z) {
      d_data->d_staticCamera->setPosition(x, y, z);
      d_data->d_dirty = true;
    }

    if(d_data->d_staticCamera->yaw() != yaw ||
                   d_data->d_staticCamera->pitch() != pitch ||
                   d_data->d_staticCamera->roll() != roll) {
      d_data->d_staticCamera->setRotation(yaw, pitch, roll);
      d_data->d_dirty = true;
    }
  }
}



void ag::SceneView::retrieveFeedback(Feedback* feedback)
{
  //  1. Start feedback mode.
  //  2. Make scene dirty.
  //  3. Render scene.
  //  4. Stop feedback mode and enter render mode.

  assert(feedback);

  feedback->start();                                                       // 1.
  setDirty();                                                              // 2.
  updateGL();                                                              // 3.
  feedback->stop();                                                        // 4.
}



int ag::SceneView::depthOfRenderingContext() const
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



void ag::SceneView::rotateScene(double x, double y, double z)
{
  sceneObject().rotateBy(y, x, z);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------

/*!
  \enum ag::SceneView::Camera
  Camera positions. There's one camera which can be move and oriented en
  there're 5 static camera's.

*/

/*!
  \var ag::SceneView::Camera ag::SceneView::USER
  Camera the user looks through and which can be moved and oriented.
*/

/*!
  \var ag::SceneView::Camera ag::SceneView::TOP
  Static camera at the roof of the scene.
*/

/*!
  \var ag::SceneView::Camera ag::SceneView::FRONT
  Static camera in front of the scene.
*/

/*!
  \var ag::SceneView::Camera ag::SceneView::LEFT
  Static camera to the left of the scene.
*/

/*!
  \var ag::SceneView::Camera ag::SceneView::BACK
  Static camera at the back of the scene.
*/

/*!
  \var ag::SceneView::Camera ag::SceneView::RIGHT
  Static camera to the right of the scene.
*/



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


