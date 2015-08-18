#ifndef INCLUDED_AG_MAP2D
#define INCLUDED_AG_MAP2D



#include <boost/filesystem/path.hpp>
#include "ag_Configure.h"
#include "ag_Map.h"



class QSplitter;
namespace ag {
  class DataGuide;
  class DataObject;
  class LegendView;
  class Map2DView;
}



namespace ag {



//! The Map2DWindow class if for map2d application windows.
/*!
*/
class PCR_AG_DECL Map2D: public Map
{

private:

  Q_OBJECT

  QSplitter*       d_splitter;

  Map2DView*       d_mapView;

  LegendView*      d_legendView;

  //! Assignment operator. NOT IMPLEMENTED.
  Map2D&           operator=           (const Map2D&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Map2D               (const Map2D&);

  void             createInterface     ();

  void             rescan              ();

  void             process             ();

  void             visualise           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   Map2D               (DataObject* dataObject,
                                        QWidget* parent = 0);

  //! Destructor.
  /* virtual */    ~Map2D              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (const DataGuide& dataGuide);

  void             clear               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const LegendView* legendView         () const;

  void             saveAsPNG           (boost::filesystem::path const& path) const;

public Q_SLOTS:

  void             startQueryMode      ();

  void             startPanMode        ();

  void             startZoomAreaMode   ();

  void             startSelectMode     ();

  void             resetMapView        ();

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
