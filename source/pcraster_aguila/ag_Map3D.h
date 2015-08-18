#ifndef INCLUDED_AG_MAP3D
#define INCLUDED_AG_MAP3D



// Library headers.
#include <boost/filesystem/path.hpp>

// PCRaster library headers.

// Module headers.
#include "ag_Map.h"



class QSplitter;
namespace ag {
  // Map3D declarations.
  class DataGuide;
  class DataObject;
  class LegendView;
  class Map3DView;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class Map3D: public Map
{

private:

  Q_OBJECT

  QSplitter*       d_splitter;

  Map3DView*       d_mapView;

  LegendView*      d_legendView;

  //! Assignment operator. NOT IMPLEMENTED.
  Map3D&           operator=           (const Map3D& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Map3D               (const Map3D& rhs);

  void             createInterface     (DataObject* object);

  void             rescan              ();

  void             process             ();

  void             visualise           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Map3D               (DataObject* dataObject,
                                        QWidget* parent = 0);

  /* virtual */    ~Map3D              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (const DataGuide& dataGuide);

  void             setHeight           (const DataGuide& dataGuide);

  void             saveAsPNG           (boost::filesystem::path const& path) const;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  int              depthOfRenderingContext() const;

  bool             doubleBuffer        () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
