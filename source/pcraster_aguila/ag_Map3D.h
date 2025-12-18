#ifndef INCLUDED_AG_MAP3D
#define INCLUDED_AG_MAP3D

#include "ag_Map.h"

#include <filesystem>


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

  QSplitter*       d_splitter{};

  Map3DView*       d_mapView{nullptr};

  LegendView*      d_legendView{nullptr};

  //! Assignment operator. NOT IMPLEMENTED.
  Map3D&           operator=           (const Map3D& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Map3D               (const Map3D& rhs);

  void             createInterface     (DataObject* object);

  void             rescan              () override;

  void             process             () override;

  void             visualise           () override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Map3D               (DataObject* dataObject,
                                        QWidget* parent = nullptr);

  /* virtual */    ~Map3D              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (const DataGuide& dataGuide);

  void             setHeight           (const DataGuide& dataGuide);

  void             saveAsPNG           (std::filesystem::path const& path) const;

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
