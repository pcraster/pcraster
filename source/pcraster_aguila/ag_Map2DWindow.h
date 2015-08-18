#ifndef INCLUDED_AG_MAP2DWINDOW
#define INCLUDED_AG_MAP2DWINDOW



#include <boost/filesystem/path.hpp>
#include "ag_MapWindow.h"



namespace ag {
  class DataGuide;
  class Map2D;
}



namespace ag {



//! The Map2DWindow class if for map2d application windows.
/*!
*/
class Map2DWindow: public ag::MapWindow
{

private:

  Q_OBJECT

  Map2D*           d_map;

  //! Assignment operator. NOT IMPLEMENTED.
  Map2DWindow&     operator=           (const Map2DWindow&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Map2DWindow         (const Map2DWindow&);

  void             createInterface     ();

  void             saveAsPNG           (boost::filesystem::path const& path);

protected:

  std::string      windowName          () const;

  bool             dataVisualised      () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   Map2DWindow         (const qt::AppWindowProperties& props,
                                        DataObject* object);

  //! Destructor.
  /* virtual */    ~Map2DWindow              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             rescan              ();

  void             addAttribute        (ag::DataGuide const& guide);

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
