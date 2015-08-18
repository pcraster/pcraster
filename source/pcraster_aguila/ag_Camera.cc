#include "ag_Camera.h"
#include <cassert>
#include <GL/glu.h>



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructs a Camera object.
/*!
  \sa        SceneObject(GLfloat, GLfloat, GLfloat, GLfloat, GLfloat, GLfloat)
*/
ag::Camera::Camera(GLfloat x, GLfloat y, GLfloat z,
                    GLfloat yaw, GLfloat pitch, GLfloat roll)

  : SceneObject(x, y, z, yaw, pitch, roll),
    d_list(0), d_quadric(0)

{
}



//! Destructs a Camera object.
/*!
*/
ag::Camera::~Camera()
{
}



//! Renders the camera.
/*!
  \sa        apply()
*/
void ag::Camera::renderObject()
{
  if(!valid()) {
    deleteList();
    createList();
    setValid(true);
  }

  assert(d_list > 0);

/*
  // Gimbal lock!
  glRotatef(-pitch() / com::DEG2RAD, 1.0, 0.0, 0.0);
  glRotatef(-yaw() / com::DEG2RAD, 0.0, 1.0, 0.0);
  glRotatef(-roll() / com::DEG2RAD, 0.0, 0.0, 1.0);
*/

  Quaternion q(-pitch(), -yaw(), -roll());
  GLfloat m[16];
  q.matrix(m);

  glTranslatef(x(), y(), z());
  glMultMatrixf(m);

  glCallList(d_list);
  glPushMatrix();
    glTranslatef(0.0, 0.0, 2.0 / 3.0 * depth());
    glCallList(d_list + 1);
  glPopMatrix();
  glPushMatrix();
    glRotatef(180.0, 1.0, 0.0, 0.0);
    glCallList(d_list + 2);
  glPopMatrix();
  setDirty(false);
}



//! Applies the camera so you end up looking through it.
/*!
  \sa        renderObject()

  This function installs the properties (aim and position) of the camera so
  you will look through it.
*/
void ag::Camera::apply()
{
  GLfloat m[16];
  matrix(m);

  glMultMatrixf(m);
  glTranslatef(-x(), -y(), -z());

  setDirty(false);
}



void ag::Camera::createList()
{
#ifdef DEBUG_DEVELOP
  assert(!(d_list > 0));
  assert(!d_quadric);
#endif

  d_list = glGenLists(3);
  assert(d_list > 0);
  d_quadric = gluNewQuadric();
  assert(d_quadric);

  GLfloat tailWidth = 0.5 * width();
  GLfloat tailLength = 2.0 / 3.0 * depth();
  GLfloat headWidth = width();
  GLfloat headLength = 1.0 / 3.0 * depth();

  static GLfloat mat_diffuse[]   = { 0.75f, 0.0f, 0.0f, 1.0f };
  static GLfloat mat_specular[]  = { 0.5f, 0.0f, 0.0f, 1.0f };
  static GLfloat mat_shininess[] = { 25.0f };
  static GLfloat mat_emission[]  = { 0.1f, 0.0f, 0.0f, 1.0f };

  gluQuadricDrawStyle(d_quadric, (GLenum)GLU_FILL);
  gluQuadricNormals(d_quadric, (GLenum)GLU_SMOOTH);

  // Build the arrow tail.
  glNewList(d_list + 0, GL_COMPILE);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_diffuse);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_shininess);
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_emission);
    gluQuadricOrientation(d_quadric, (GLenum)GLU_OUTSIDE);
    gluCylinder(d_quadric, tailWidth, tailWidth, tailLength, 25, 25);
    gluQuadricOrientation(d_quadric, (GLenum)GLU_INSIDE);
    gluDisk(d_quadric, 0, tailWidth, 25, 25);
  glEndList();

  glNewList(d_list + 1, GL_COMPILE);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_diffuse);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_shininess);
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_emission);
    gluQuadricOrientation(d_quadric, (GLenum)GLU_OUTSIDE);
    gluDisk(d_quadric, 0, tailWidth, 25, 25);
  glEndList();

  // Build the arrow head.
  glNewList(d_list + 2, GL_COMPILE);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_diffuse);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_shininess);
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_emission);
    gluQuadricOrientation(d_quadric, (GLenum)GLU_OUTSIDE);
    gluCylinder(d_quadric, headWidth, 0.0, headLength, 25, 25);
    gluQuadricOrientation(d_quadric, (GLenum)GLU_INSIDE);
    gluDisk(d_quadric, tailWidth, headWidth, 25, 25);
  glEndList();
}



void ag::Camera::deleteList()
{
  if(d_list > 0) {
    glDeleteLists(d_list, 3);
    d_list = 0;
  }
  if(d_quadric) {
    gluDeleteQuadric(d_quadric);
    d_quadric = 0;
  }
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



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


