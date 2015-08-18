#ifndef INCLUDED_AG_SCENE
#define INCLUDED_AG_SCENE





class QKeyEvent;
namespace ag {
  class SceneView;
}



namespace ag {



//! The Scene class provides the common functionality for scene visualisations.
/*!
*/
class Scene
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Scene&           operator=           (const Scene&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Scene               (const Scene&);

protected:

                   Scene               ();

  bool             keyPressHandled     (QKeyEvent* e);

  virtual SceneView& sceneView         () const = 0;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~Scene              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

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
