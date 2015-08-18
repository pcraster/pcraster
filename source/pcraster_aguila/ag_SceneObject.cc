#include "ag_SceneObject.h"



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructs a SceneObject object.
/*!
  \param     x X-coordinate of position.
  \param     y Y-coordinate of position.
  \param     z Z-coordinate of position.
  \param     yaw Yaw (radians).
  \param     pitch Pitch (radians).
  \param     roll Roll (radians).

  The initial position and the initial rotations will be set to (0.0, 0.0,
  0.0). Use setInitPosition(GLfloat, GLfloat, GLfloat) and
  setInitRotation(GLfloat, GLfloat, GLfloat) to change these defaults.
*/
ag::SceneObject::SceneObject(GLfloat x, GLfloat y, GLfloat z,
                   GLfloat yaw, GLfloat pitch, GLfloat roll)

  : d_x(x), d_y(y), d_z(z),
    d_xInit(0.0), d_yInit(0.0), d_zInit(0.0),
    d_yaw(yaw), d_roll(roll), d_pitch(pitch),
    d_yawInit(0.0), d_rollInit(0.0), d_pitchInit(0.0),
    d_width(0.0), d_depth(0.0), d_height(0.0),
    d_dirty(false), d_valid(false)

{
}



//! Destructs a SceneObject object.
/*!
*/
ag::SceneObject::~SceneObject()
{
}



//! Renders the SceneObject object.
/*!
  \sa        renderObject()

  This function pushes a new matrix on the stack, calls renderObject() and
  pops the matrix from the stack.

  This function calls functions from the OpenGL API.
*/
void ag::SceneObject::render()
{
  glPushMatrix();
    renderObject();
  glPopMatrix();
}



//! Notifies the object that the object should be rendered.
/*!
  \param     s Setting.
  \sa        dirty()
  \warning   Only call setDirty(bool) with argument true if the object really
             is dirty. Rendering can be expensive!

  A dirty object is an object whose properties have changed. For example, if
  the position of an object is changed, the object becomes dirty. The object
  owning this object should decide to render the scene again.

  Normally, the specialised SceneObject will just call setDirty() every time
  properties are changed. SceneObject will set dirty to true if the object is
  rendered.
*/
void ag::SceneObject::setDirty(bool s)
{
  d_dirty = s;
}



void ag::SceneObject::setValid(bool s)
{
  d_valid = s;
}



//! Returns true if the object should be rendered.
/*!
  \return    true if the object is dirty.
  \sa        setDirty()
*/
bool ag::SceneObject::dirty() const
{
  return d_dirty;
}



bool ag::SceneObject::valid() const
{
  return d_valid;
}



//! Sets the initial position to \a x, \a y, \a z.
/*!
  \param     x X-coordinate.
  \param     y Y-coordinate.
  \param     z Z-coordinate.
  \sa        resetPosition(), setInitRotation(GLfloat, GLfloat, GLfloat)

  The initial position is used when resetPosition() is called. The current
  position is unchanged.
*/
void ag::SceneObject::setInitPosition(GLfloat x, GLfloat y, GLfloat z)
{
  d_xInit = x;
  d_yInit = y;
  d_zInit = z;
}



//! Sets the initial rotation to \a yaw, \a pitch, \a roll.
/*!
  \param     yaw Yaw.
  \param     pitch Pitch.
  \param     roll Roll.
  \sa        resetRotation(), setInitPosition(GLfloat, GLfloat, GLfloat)

  The initial rotation is used when resetRotation() is called. The current
  rotation is unchanged.
*/
void ag::SceneObject::setInitRotation(GLfloat yaw, GLfloat pitch, GLfloat roll)
{
  d_yawInit = yaw;
  d_pitchInit = pitch;
  d_rollInit = roll;
}



//! Resets the position and rotation.
/*!
  \sa        resetPosition(), resetRotation()
*/
void ag::SceneObject::reset()
{
  resetPosition();
  resetRotation();
}



//! Resets the rotation.
/*!
  \sa        setInitRotation(GLfloat, GLfloat, GLfloat), resetPosition()

  The function sets the current rotation to the initial rotation.
*/
void ag::SceneObject::resetRotation()
{
  d_quaternion.reset();
  setRotation(d_yawInit, d_pitchInit, d_rollInit);
}



//! Resets the position.
/*!
  \sa        setInitPosition(GLfloat, GLfloat, GLfloat), resetRotation()

  This function sets the current position to the initial position.
*/
void ag::SceneObject::resetPosition()
{
  setPosition(d_xInit, d_yInit, d_zInit);
}



//! Moves the object to the new location \a x, \a y, \a z.
/*!
  \param     x X-coordinate.
  \param     y Y-coordinate.
  \param     z Z-coordinate.
  \sa        moveBy(GLfloat, GLfloat, GLfloat)
*/
void ag::SceneObject::setPosition(GLfloat x, GLfloat y, GLfloat z)
{
  if(d_x != x || d_y != y || d_z != z) {
    d_x = x;
    d_y = y;
    d_z = z;
    setDirty(true);
  }
}



//! Moves the object by \a x, \a y, \a z.
/*!
  \param     x Change in x-coordinate.
  \param     y Change in y-coordinate.
  \param     z Change in z-coordinate.
  \sa        setPosition(GLfloat, GLfloat, GLfloat)
*/
void ag::SceneObject::moveBy(GLfloat x, GLfloat y, GLfloat z)
{
  if(x != 0.0 || y != 0.0 || z != 0.0) {
    d_x += x;
    d_y += y;
    d_z += z;
    setDirty(true);
  }
}



//! Sets the object rotation to \a yaw, \a pitch, \a roll.
/*!
  \param     yaw Yaw (radians).
  \param     pitch Pitch (radians).
  \param     roll Roll (radians).
  \sa        rotateBy(GLfloat, GLfloat, GLfloat)
*/
void ag::SceneObject::setRotation(GLfloat yaw, GLfloat pitch, GLfloat roll)
{
  if(d_yaw != yaw || d_pitch != pitch || d_roll != roll) {
    d_yaw = yaw;
    d_pitch = pitch;
    d_roll = roll;
    d_quaternion.reset();
    Quaternion q(d_pitch, d_yaw, d_roll);
    d_quaternion.postMult(q);
    setDirty(true);
  }
}



//! Rotates the object by \a yaw, \a pitch, \a roll.
/*!
  \param     yaw Change in yaw (radians).
  \param     pitch Change in pitch (radians).
  \param     roll Change in roll (radians).
  \sa        setRotation(GLfloat, GLfloat, GLfloat)
*/
void ag::SceneObject::rotateBy(GLfloat yaw, GLfloat pitch, GLfloat roll)
{
  if(yaw != 0.0 || pitch != 0.0 || roll != 0.0) {
    d_yaw += yaw;
    d_pitch += pitch;
    d_roll += roll;
    Quaternion q(pitch, yaw, roll);
    d_quaternion.postMult(q);
    setDirty(true);
  }
}



//! Rotates the yaw by \a a.
/*!
  \param     a Yaw (radians).
  \sa        pitchBy(GLfloat), rollBy(GLfloat)
*/
void ag::SceneObject::yawBy(GLfloat a)
{
  if(a != 0.0) {
    d_yaw += a;
    Quaternion q(0.0, a, 0.0);
    d_quaternion.postMult(q);
    setDirty(true);
  }
}



//! Rotates the pitch by \a a.
/*!
  \param     a Pitch (radians).
  \sa        yawBy(GLfloat), rollBy(GLfloat)
*/
void ag::SceneObject::pitchBy(GLfloat a)
{
  if(a != 0.0) {
    d_pitch += a;
    Quaternion q(a, 0.0, 0.0);
    d_quaternion.postMult(q);
    setDirty(true);
  }
}



//! Rotates the roll by \a.
/*!
  \param     a Roll (radians).
  \sa        yawBy(GLfloat), pitchBy(GLfloat)
*/
void ag::SceneObject::rollBy(GLfloat a)
{
  if(a != 0.0) {
    d_roll += a;
    Quaternion q(0.0, 0.0, a);
    d_quaternion.postMult(q);
    setDirty(true);
  }
}



//! Returns the x-coordinate of the position.
/*!
  \return    X-coordinate.
  \sa        y(), z()
*/
GLfloat ag::SceneObject::x() const
{
  return d_x;
}



//! Returns the y-coordinate of the position.
/*!
  \return    Y-coordinate.
  \sa        x(), z()
*/
GLfloat ag::SceneObject::y() const
{
  return d_y;
}



//! Returns the z-coordinate of the position.
/*!
  \return    Z-coordinate.
  \sa        x(), y()
*/
GLfloat ag::SceneObject::z() const
{
  return d_z;
}



/*
GLfloat ag::SceneObject::xInit() const
{
  return d_xInit;
}



GLfloat ag::SceneObject::yInit() const
{
  return d_yInit;
}



GLfloat ag::SceneObject::zInit() const
{
  return d_zInit;
}
*/



//! Returns the yaw.
/*!
  \return    Yaw.
  \sa        pitch(), roll()
*/
GLfloat ag::SceneObject::yaw() const
{
  return d_yaw;
}



//! Returns the pitch.
/*!
  \return    Pitch.
  \sa        yaw(), roll()
*/
GLfloat ag::SceneObject::pitch() const
{
  return d_pitch;
}



//! Returns the roll.
/*!
  \return    Roll.
  \sa        yaw(), pitch()
*/
GLfloat ag::SceneObject::roll() const
{
  return d_roll;
}



/*
GLfloat ag::SceneObject::yawInit() const
{
  return d_yawInit;
}



GLfloat ag::SceneObject::pitchInit() const
{
  return d_pitchInit;
}



GLfloat ag::SceneObject::rollInit() const
{
  return d_rollInit;
}
*/



//! Writes the rotation matrix in \a m.
/*!
  \param     m Array of 16 GLfloat's.
  \warning   The argument \a m will be overwritten.

  You can use the array as a rotation matrix in OpenGL.
*/
void ag::SceneObject::matrix(GLfloat m[16])
{
  d_quaternion.matrix(m);
}



void ag::SceneObject::setSize(GLfloat w, GLfloat d, GLfloat h)
{
  if(d_width != w || d_depth != d || d_height != h) {
    d_width = w;
    d_depth = d;
    d_height = h;
    setDirty(true);
    setValid(false);
  }
}



GLfloat ag::SceneObject::width() const
{
  return d_width;
}



GLfloat ag::SceneObject::depth() const
{
  return d_depth;
}



GLfloat ag::SceneObject::height() const
{
  return d_height;
}



const ag::Quaternion& ag::SceneObject::quaternion() const
{
  return d_quaternion;
}



//! Returns the left coordinate.
/*!
  \return    Left.
  \warning   Call setSize(GLfloat, GLfloat, GLfloat) first!
  \sa        right(), back(), front(), bottom(), top()
*/
GLfloat ag::SceneObject::left() const
{
  return x() - 0.5 * width();
}



//! Returns the right coordinate.
/*!
  \return    Right.
  \warning   Call setSize(GLfloat, GLfloat, GLfloat) first!
  \sa        left(), back(), front(), bottom(), top()
*/
GLfloat ag::SceneObject::right() const
{
  return x() + 0.5 * width();
}



//! Returns the front coordinate.
/*!
  \return    Front.
  \warning   Call setSize(GLfloat, GLfloat, GLfloat) first!
  \sa        left(), right(), back(), bottom(), top()
*/
GLfloat ag::SceneObject::front() const
{
  return z() + 0.5 * depth();
}



//! Returns the back coordinate.
/*!
  \return    Back.
  \warning   Call setSize(GLfloat, GLfloat, GLfloat) first!
  \sa        left(), right(), front(), bottom(), top()
*/
GLfloat ag::SceneObject::back() const
{
  return z() - 0.5 * depth();
}



//! Returns the top coordinate.
/*!
  \return    Top.
  \warning   Call setSize(GLfloat, GLfloat, GLfloat) first!
  \sa        left(), right(), back(), front(), bottom()
*/
GLfloat ag::SceneObject::top() const
{
  return y() + 0.5 * height();
}



//! Returns the bottom coordinate.
/*!
  \return    Bottom.
  \warning   Call setSize(GLfloat, GLfloat, GLfloat) first!
  \sa        left(), right(), back(), front(), top()
*/
GLfloat ag::SceneObject::bottom() const
{
  return y() - 0.5 * height();
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



//! Renders the object.
/*!
  \fn        void ag::SceneObject::renderObject()
  \sa        render()

  Implement this function for your specialised SceneObject.

  This function is called by render(). You don't have to call the pushMatrix()
  and popMatrix() openGL functions: a clean matrix is already pushed for you
  and will be popped when you're done.
*/
