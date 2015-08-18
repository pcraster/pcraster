#ifndef INCLUDED_AG_SCENEOBJECT
#define INCLUDED_AG_SCENEOBJECT



#include <QtOpenGL>

#ifndef INCLUDED_AG_QUATERNION
#include "ag_Quaternion.h"
#endif



namespace ag {



//! OpenGL object.
/*!
  Abstract base class for OpenGL objects.

  The position of a scene object is determined by the x, y and z coordinates.

  The orientation of a scene object is determined by the yaw, pitch and roll
  rotations. Imagine an aeroplane: yaw change is turning left or right, pitch 
  change is moving nose down and tail up (or vice-versa) and roll change is
  moving one wingtip up and the other down.
*/
class SceneObject
{

private:

  GLfloat          d_x;
  GLfloat          d_y;
  GLfloat          d_z;

  GLfloat          d_xInit;
  GLfloat          d_yInit;
  GLfloat          d_zInit;

  GLfloat          d_yaw;
  GLfloat          d_roll;
  GLfloat          d_pitch;

  GLfloat          d_yawInit;
  GLfloat          d_rollInit;
  GLfloat          d_pitchInit;

  GLfloat          d_width;
  GLfloat          d_depth;
  GLfloat          d_height;

  bool             d_dirty;
  bool             d_valid;

  Quaternion       d_quaternion;

  //! Assignment operator. NOT IMPLEMENTED.
  SceneObject &    operator=           (const SceneObject &);

  //! Copy constructor. NOT IMPLEMENTED.
                   SceneObject         (const SceneObject &);

protected:

  void             setDirty            (bool s);

  void             setValid            (bool s);

  virtual void     renderObject        () = 0;

  const Quaternion& quaternion         () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SceneObject         (GLfloat x = 0.0,
                                        GLfloat y = 0.0,
                                        GLfloat z = 0.0,
                                        GLfloat yaw = 0.0,
                                        GLfloat pitch = 0.0,
                                        GLfloat roll = 0.0);

  virtual          ~SceneObject        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             render              ();

  virtual void     reset               ();

  void             setInitPosition     (GLfloat x,
                                        GLfloat y,
                                        GLfloat z);

  void             setInitRotation     (GLfloat yaw,
                                        GLfloat pitch,
                                        GLfloat roll);

  void             resetRotation       ();

  void             resetPosition       ();

  void             setPosition         (GLfloat x,
                                        GLfloat y,
                                        GLfloat z);

  void             moveBy              (GLfloat x,
                                        GLfloat y,
                                        GLfloat z);

  void             setRotation         (GLfloat yaw,
                                        GLfloat pitch,
                                        GLfloat roll);

  void             rotateBy            (GLfloat yaw,
                                        GLfloat pitch,
                                        GLfloat roll);

  void             yawBy               (GLfloat a);

  void             pitchBy             (GLfloat a);

  void             rollBy              (GLfloat a);

  void             setSize             (GLfloat w,
                                        GLfloat d,
                                        GLfloat h);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  GLfloat          x                   () const;

  GLfloat          y                   () const;

  GLfloat          z                   () const;

/*
  GLfloat          xInit               () const;

  GLfloat          yInit               () const;

  GLfloat          zInit               () const;
*/

  GLfloat          yaw                 () const;

  GLfloat          pitch               () const;

  GLfloat          roll                () const;

/*
  GLfloat          yawInit             () const;

  GLfloat          pitchInit           () const;

  GLfloat          rollInit            () const;
*/

  GLfloat          width               () const;

  GLfloat          depth               () const;

  GLfloat          left                () const;

  GLfloat          right               () const;

  GLfloat          front               () const;

  GLfloat          back                () const;

  GLfloat          top                 () const;

  GLfloat          bottom              () const;

  GLfloat          height              () const;

  void             matrix              (GLfloat m[16]);

  bool             dirty               () const;

  bool             valid               () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
