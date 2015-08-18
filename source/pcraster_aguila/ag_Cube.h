#ifndef INCLUDED_AG_CUBE
#define INCLUDED_AG_CUBE



#ifndef INCLUDED_AG_SCENEOBJECT
#include "ag_SceneObject.h"
#endif



namespace ag {



/*!
  \class Cube
  \brief Concrete scene object.

  This is a simple example class to show how a concrete scene object can be
  created for use in a OpenGL scene.
*/
//       1         2         3         4         5         6         7         8
class Cube: public SceneObject
{

private:

  GLuint           d_list;

  //! Assignment operator. NOT IMPLEMENTED.
  Cube &           operator=           (const Cube &);

  //! Copy constructor. NOT IMPLEMENTED.
                   Cube                (const Cube &);

  void             createList          ();

  void             deleteList          ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Cube                (GLfloat x = 0.0,
                                        GLfloat y = 0.0,
                                        GLfloat z = 0.0,
                                        GLfloat yaw = 0.0,
                                        GLfloat pitch = 0.0,
                                        GLfloat roll = 0.0);

  /* virtual */    ~Cube               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             renderObject        ();

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
