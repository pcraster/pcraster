#ifndef INCLUDED_AG_CAMERA
#define INCLUDED_AG_CAMERA

#include "ag_SceneObject.h"



// class GLUquadric;

namespace ag {



/*!
  \class Camera
  \brief Concrete scene object, mainly for looking through at a scene.

  Most of the time the camera will be used to look through (see apply()) but it
  can be looked at to (see renderObject())!
*/
//       1         2         3         4         5         6         7         8
class Camera: public SceneObject
{

private:

  GLuint           d_list{0};
  // GLUquadric*      d_quadric{nullptr};

  void             createList          ();

  void             deleteList          ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Camera              (GLfloat x = 0.0,
                                        GLfloat y = 0.0,
                                        GLfloat z = 0.0,
                                        GLfloat yaw = 0.0,
                                        GLfloat pitch = 0.0,
                                        GLfloat roll = 0.0);

                   Camera              (const Camera &) = delete;

  /* virtual */    ~Camera             () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Camera &         operator=           (const Camera &) = delete;

  void             renderObject        () override;

  void             apply               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
