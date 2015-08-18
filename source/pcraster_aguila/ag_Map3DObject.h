#ifndef INCLUDED_AG_MAP3DOBJECT
#define INCLUDED_AG_MAP3DOBJECT



#include "ag_SceneObject.h"
#include "ag_DataObject.h"



namespace ag {
}



namespace ag {



//! Map3D Scene object.
/*!
  A Map3D object visualizes stacks as 3D floating sheets. One of the stacks
  represents height and the others are attributes which are draped on the sheet
  (see createScene(const ag::DataObject&, const ag::DataGuide&,
  const std::vector<DataGuide>&).
*/
class Map3DObject: public ag::SceneObject
{

private:

  size_t           d_quadLength;

  GLfloat          d_scale;

  GLuint           d_list;

  bool             d_showFishnet;

  //! Assignment operator. NOT IMPLEMENTED.
  Map3DObject&     operator=           (const Map3DObject&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Map3DObject         (const Map3DObject&);

  void             createHeightScene   (const ag::DataObject& o,
                                        const ag::DataGuide& h);

  void             createDrapeScene
                                  (const ag::DataObject& o,
                                   const ag::DataGuide& h,
                                   const std::vector<DataGuide>& dataGuides);

protected:

  void             renderObject        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Map3DObject         (GLfloat x = 0.0,
                                        GLfloat y = 0.0,
                                        GLfloat z = 0.0,
                                        GLfloat yaw = 0.0,
                                        GLfloat pitch = 0.0,
                                        GLfloat roll = 0.0);

  /* virtual */    ~Map3DObject        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             createScene    (const ag::DataObject& o,
                                   const ag::DataGuide& h,
                                   const std::vector<DataGuide>& dataGuides);

  void             deleteScene         ();

  void             setQuadLength       (size_t l);

  void             setScale            (GLfloat s);

  void             setShowFishnet      (bool s);

  void             reset               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

/*
  GLfloat          left                () const;

  GLfloat          right               () const;

  GLfloat          front               () const;

  GLfloat          back                () const;

  GLfloat          top                 () const;

  GLfloat          bottom              () const;
*/

  size_t           quadLength          () const;

  GLfloat          scale               () const;

  bool             showFishnet         () const;

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
