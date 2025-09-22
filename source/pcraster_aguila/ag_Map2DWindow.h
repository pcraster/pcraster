#ifndef INCLUDED_AG_MAP2DWINDOW
#define INCLUDED_AG_MAP2DWINDOW



#include "ag_MapWindow.h"

#include <filesystem>


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

  Map2D*           d_map{nullptr};

  //! Assignment operator. NOT IMPLEMENTED.
  Map2DWindow&     operator=           (const Map2DWindow&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Map2DWindow         (const Map2DWindow&);

  void             createInterface     ();

  void             saveAsPNG           (std::filesystem::path const& path) override;

protected:

  std::string      windowName          () const override;

  bool             dataVisualised      () const override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   Map2DWindow         (const qt::AppWindowProperties& props,
                                        DataObject* object);

  //! Destructor.
  /* virtual */    ~Map2DWindow              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             rescan              () override;

  void             addAttribute        (ag::DataGuide const& guide) override;

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
