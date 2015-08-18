#ifndef INCLUDED_AG_MAP3DVIEW
#define INCLUDED_AG_MAP3DVIEW



#include "ag_DataGuide.h"
#include "ag_GLVisualisation.h"
#include "ag_MouseTarget.h"



namespace ag {
  class Map3DViewPrivate;
  class DataObject;
  class SceneObject;
}



namespace ag {



/*!
  \class Map3DView
  \brief short_description

  longer_description
*/
class Map3DView: public ag::GLVisualisation
{

private:

  Map3DViewPrivate *d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  Map3DView &      operator=           (const Map3DView &);

  //! Copy constructor. NOT IMPLEMENTED.
                   Map3DView           (const Map3DView &);

  //! Frees dynamically allocated memory.
  void             clean               ();

  double           leftScene           () const;

  double           rightScene          () const;

  double           bottomScene         () const;

  double           topScene            () const;

  double           backScene           () const;

  double           frontScene          () const;

/*
  void             setValid            (bool s);
*/

  MouseTarget      d_mapViewTarget;

  /*
  //! Sets the length of the quads to \a s.
  void             setQuadLength       (size_t s);

  void             setScale            (double s);
  */

protected:

  void             rescan              ();

  void             process             ();

  void             visualise           ();

  ag::SceneObject& sceneObject         () const;

  void             mousePressEvent     (QMouseEvent* event);

  void             mouseReleaseEvent   (QMouseEvent* event);

  void             mouseMoveEvent      (QMouseEvent* event);

  void             keyPressEvent       (QKeyEvent* event);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   Map3DView           (DataObject* object,
                                        QWidget* parent = 0);

  //! Destructor.
  /* virtual */    ~Map3DView          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (const DataGuide& dataGuide);

  void             setHeight           (const DataGuide& dataGuide);

  //! Creates display lists based on height \a height and attributes \a dataGuides from \a dataObject.
  void             createScene    (const ag::DataGuide &height,
                                   const std::vector<DataGuide>& dataGuides);

  //! Deletes the scene.
  void             deleteScene         ();

/*
  void             deleteFishnet       ();
*/

  /*
  void             setShowFishnet      (bool s);
  */

/*
  void             rotateScene         (GLfloat x,
                                        GLfloat y,
                                        GLfloat z);
*/

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  /*
  bool             showFishnet         () const;
  */

  /*
  //! Returns the quad length in number of cells.
  size_t           quadLength          () const;

  double           scale               () const;
  */

/*
  bool             valid               () const;
*/

  int              depthOfRenderingContext() const;

  bool             doubleBuffer        () const;

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
