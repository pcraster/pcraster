#ifndef INCLUDED_AG_MAP3DWINDOW
#define INCLUDED_AG_MAP3DWINDOW



#include <boost/filesystem/path.hpp>
#include "ag_Configure.h"
#include "ag_MapWindow.h"



namespace ag {
  class DataGuide;
  class Map3D;
}



namespace ag {



//! The Map3DWindow class is for map3d application windows.
class PCR_AG_DECL Map3DWindow: public ag::MapWindow
{

private:

  Q_OBJECT

  Map3D*           d_map;

  //! Assignment operator. NOT IMPLEMENTED.
  Map3DWindow &    operator=           (const Map3DWindow &);

  //! Copy constructor. NOT IMPLEMENTED.
                   Map3DWindow         (const Map3DWindow &);

  void             createInterface     ();

  void             saveAsPNG           (boost::filesystem::path const& path);

private Q_SLOTS:

  void             showOpenGLInfo      ();

protected:

  std::string      windowName          () const;

  bool             dataVisualised      () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   Map3DWindow         (const qt::AppWindowProperties& props,
                                        ag::DataObject* object);

  //! Destructor.
  /* virtual */    ~Map3DWindow        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             rescan              ();

  void             setHeight           (const DataGuide& guide);

  void             addAttribute        (const DataGuide& guide);

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
