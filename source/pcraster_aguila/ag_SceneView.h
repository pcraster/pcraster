#ifndef INCLUDED_AG_SCENEVIEW
#define INCLUDED_AG_SCENEVIEW



#include <QtOpenGL>
#include "csf.h"



namespace ag {
  class Feedback;
  class SceneObject;
  class SceneViewPrivate;
}



namespace ag {



//! A SceneView is a 3D world with ag::SceneObject's, viewed by ag::Camera's.
/*!
*/
class SceneView: public QGLWidget
{

private:

  //! Calcs angle of the FOV based on size \a s and distance \a d.
  static GLfloat   calcFOV             (double s,
                                        double d);

  //! Calcs angle of FOV based on size of scene and position of viewpoint.
  static GLfloat   calcFOV             (double w,
                                        double d,
                                        double h,
                                        double x,
                                        double y,
                                        double z);

  SceneViewPrivate* d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  SceneView &      operator=           (const SceneView &);

  //! Copy constructor. NOT IMPLEMENTED.
                   SceneView           (const SceneView &);

  //! Frees dynamically allocated memory.
  void             clean               ();

  virtual double   leftScene           () const = 0;

  virtual double   rightScene          () const = 0;

  virtual double   bottomScene         () const = 0;

  virtual double   topScene            () const = 0;

  virtual double   backScene           () const = 0;

  virtual double   frontScene          () const = 0;

  //! Notifies the widget that the scene is dirty and should be redrawn.
  void             setDirty            ();

protected:

  //! Constructor.
                   SceneView           (QWidget *p = 0);

  virtual ag::SceneObject& sceneObject() const = 0;

  virtual void     initializeGL        ();

  virtual void     resizeGL            (int w,
                                        int h);

  virtual void     paintGL             ();

  void             checkForGLErrors    ();

  void             setValid            (bool s);

  //! Sets the initial head position.
  void             setInitHead         (double x,
                                        double y,
                                        double z);

  //! Sets a new head position.
  void             setHead             (double x,
                                        double y,
                                        double z);

  //! Sets the initial head rotation.
  void             setInitHeadRotation (double rx,
                                        double ry,
                                        double rz);

  double           widthScene          () const;

  double           depthScene          () const;

  double           heightScene         () const;

  bool             dirty               () const;

  void             keyPressEvent       (QKeyEvent* event);

public:

  enum Camera { USER, TOP, FRONT, LEFT, BACK, RIGHT };

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Destructor.
  virtual          ~SceneView          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Resets the rotations, aim and positions.
  void             reset               ();

  //! Sets the rotation of the head to \a rx, \a ry, \a rz.
  void             setHeadRotation     (double rx,
                                        double ry,
                                        double rz);

  //! Rotates the head some more.
  void             rotateHead          (double rx,
                                        double ry,
                                        double rz);

  //! Moves the head.
  void             moveHead            (double mx,
                                        double my,
                                        double mz);

  void             rotateScene         (double x,
                                        double y,
                                        double z);

  //! Sets the shade model to \a m.
  void             setShadeModel       (GLenum m);

  //! Turns the light on or off.
  void             setShowLight        (bool s);

  //! Resets the projection stuff for this scene.
  void             resetViewport       (int w,
                                        int h);

  void             installCamera       (Camera c);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             addSceneObject      (ag::SceneObject* s);

  void             removeSceneObject   (ag::SceneObject* s);

  //! Returns the currently set shade model.
  GLenum           shadeModel          () const;

  //! Returns true if the light is on.
  bool             showLight           () const;

  GLfloat          step                () const;

  GLfloat          angle               () const;

  void             retrieveFeedback    (Feedback* feedback);

  int              depthOfRenderingContext() const;

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
