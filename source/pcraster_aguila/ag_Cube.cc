#include "ag_Cube.h"
#include <cassert>



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructs a Cube object.
/*!
  \sa        SceneObject(GLfloat, GLfloat, GLfloat, GLfloat, GLfloat, GLfloat)

  The default sizes of a cube are 1.0, 1.0, 1.0. Call
  setSize(GLfloat, GLfloat, GLfloat) to change this.
*/
ag::Cube::Cube(GLfloat x, GLfloat y, GLfloat z,
                   GLfloat yaw, GLfloat pitch, GLfloat roll)

  : SceneObject(x, y, z, yaw, pitch, roll),
    d_list(0)

{
  setSize(1.0, 1.0, 1.0);
}



//! Destructs a Cube.
/*!
*/
ag::Cube::~Cube()
{
  if(d_list > 0) {
    deleteList();
  }
}



void ag::Cube::renderObject()
{
  if(!valid()) {
    deleteList();
    createList();
    setValid(true);
  }

  assert(d_list > 0);

  GLfloat m[16];
  matrix(m);
  glMultMatrixf(m);
  glTranslatef(x(), y(), z());
  glCallList(d_list);
  setDirty(false);
}



//! Creates the display list for the cube.
/*!
  \sa        deleteList()

  The cube will have red, green and blue sides.
*/
void ag::Cube::createList()
{
  GLfloat red[4]      = { 1.0, 0.0, 0.0, 1.0 };
  GLfloat green[4]    = { 0.0, 1.0, 0.0, 1.0 };
  GLfloat blue[4]     = { 0.0, 0.0, 1.0, 1.0 };
  GLfloat emission[4] = { 0.25, 0.25, 0.25, 1.0 };

  GLfloat x  = SceneObject::x() - 0.5 * width();
  GLfloat y  = SceneObject::y() - 0.5 * height();
  GLfloat z  = SceneObject::z() - 0.5 * depth();

  size_t n   = 10;
  GLfloat dx = width() / n;
  GLfloat dy = height() / n;
  GLfloat dz = depth() / n;


#ifdef DEBUG_DEVELOP
  assert(!(d_list > 0));
#endif
  d_list = glGenLists(1);
  glNewList(d_list, GL_COMPILE);

  glMaterialfv(GL_FRONT, GL_EMISSION, emission);

  glBegin(GL_QUADS);

    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, blue);

    // Front panel.
    glNormal3f(0, 0, 1);
    for(size_t i = 0; i < n; ++i) {
      for(size_t j = 0; j < n; ++j) {
        glVertex3f(x      + i * dx, y + dy + j * dy, z + depth());
        glVertex3f(x      + i * dx, y      + j * dy, z + depth());
        glVertex3f(x + dx + i * dx, y      + j * dy, z + depth());
        glVertex3f(x + dx + i * dx, y + dy + j * dy, z + depth());
      }
    }

    // Back panel.
    glNormal3f(0, 0, -1);
    for(size_t i = 0; i < n; ++i) {
      for(size_t j = 0; j < n; ++j) {
        glVertex3f(x      + i * dx, y + dy + j * dy, z);
        glVertex3f(x + dx + i * dx, y + dy + j * dy, z);
        glVertex3f(x + dx + i * dx, y      + j * dy, z);
        glVertex3f(x      + i * dx, y      + j * dy, z);
      }
    }

    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, green);

    // Left panel.
    glNormal3f(-1, 0, 0);
    for(size_t i = 0; i < n; ++i) {
      for(size_t j = 0; j < n; ++j) {
        glVertex3f(x, y      + i * dy, z      + j * dz);
        glVertex3f(x, y      + i * dy, z + dz + j * dz);
        glVertex3f(x, y + dy + i * dy, z + dz + j * dz);
        glVertex3f(x, y + dy + i * dy, z      + j * dz);
      }
    }

    // Right panel.
    glNormal3f(1, 0, 0);
    for(size_t i = 0; i < n; ++i) {
      for(size_t j = 0; j < n; ++j) {
        glVertex3f(x + width(), y      + i * dy, z      + j * dz);
        glVertex3f(x + width(), y + dy + i * dy, z      + j * dz);
        glVertex3f(x + width(), y + dy + i * dy, z + dz + j * dz);
        glVertex3f(x + width(), y      + i * dy, z + dz + j * dz);
      }
    }

    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, red);

    // Top panel.
    glNormal3f(0, 1, 0);
    for(size_t i = 0; i < n; ++i) {
      for(size_t j = 0; j < n; ++j) {
        glVertex3f(x      + i * dx, y + height(), z      + j * dz);
        glVertex3f(x      + i * dx, y + height(), z + dz + j * dz);
        glVertex3f(x + dx + i * dx, y + height(), z + dz + j * dz);
        glVertex3f(x + dx + i * dx, y + height(), z      + j * dz);
      }
    }

    // Bottom panel.
    glNormal3f(0, -1, 0);
    for(size_t i = 0; i < n; ++i) {
      for(size_t j = 0; j < n; ++j) {
        glVertex3f(x      + i * dx, y, z      + j * dz);
        glVertex3f(x + dx + i * dx, y, z      + j * dz);
        glVertex3f(x + dx + i * dx, y, z + dz + j * dz);
        glVertex3f(x      + i * dx, y, z + dz + j * dz);
      }
    }

  glEnd();

  glEndList();
}



//! Deletes the display list of the cube.
/*!
  \sa        createList()
*/
void ag::Cube::deleteList()
{
  if(d_list > 0) {
    glDeleteLists(d_list, 1);
    d_list = 0;
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


