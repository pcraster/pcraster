#ifndef INCLUDED_AG_VISENGINE
#define INCLUDED_AG_VISENGINE



#include <vector>
#include "ag_DataGuide.h"



class QColor;
class QPointF;
namespace dal {
  class DataSpace;
  class DataSpaceAddress;
}
namespace ag {
  class DataObject;
  class DataProperties;
  // class CursorPos;
  class VisEnginePrivate;
}



namespace ag {



/*!
  \class VisEngine
  \brief The VisEngine class is the base class for all visualisation engines.

  A visualisation engine is the glue between a visualisation object and its
  views. The engine keeps track of the current state of the views and, if
  needed, it changes that state. It has all the rules typical for a certain
  visualisation and, together with a dataobject, it can decide what should be
  done. The engine is the brain of the visualisation.

  \todo All test code in 
        ag::IVisualisation::testDataGuide(const geo::DataGuide& dataGuide)
        might be better moved to this class. See also the setHeight function.
*/
class VisEngine
{

private:

  VisEnginePrivate* _data;

  //! Copy constructor. NOT IMPLEMENTED.
                   VisEngine           (const VisEngine &);

protected:

  typedef std::vector<ag::DataGuide>::iterator dataguide_iterator;

  typedef std::vector<ag::DataGuide>::const_reverse_iterator
                   const_reverse_dataguide_iterator;

  void             addDataGuide        (const ag::DataGuide& aDataGuide);

  void             addDataProperties   (const ag::DataObject& dataObject,
                                        const ag::DataGuide& dataGuide);

  ag::DataProperties& properties       ();

public:

  enum TypeOfChange {
    //! Cursor changed.
    CURSOR         = 0x00000001,
    //! Time changed.
    TIME           = 0x00000002,
    //! Quantile changed.
    QUANTILE       = 0x00000004,
    //! Raster cell changed. FEATURE Change RASTER_CELL to SPATIAL_COORDINATES.
    RASTER_CELL    = 0x00000008,
    //! Data is selected or unselected.
    SELECTION      = 0x00000010,
    //! Data is visible or not.
    VISIBILITY     = 0x00000020,
    //! Attribute(s) changed, possibly removed.
    OTHERATTRIB    = 0x00000040,
    //! Draw properties changed.
    DRAWPROPS      = 0x00000080,
    //! Map 2D Zoom changed.
    MAP2DZOOM      = 0x00000100,
    //! Map 2D changed.
    MAP2DSCALE     = 0x00000200,
    MAP2DMOVE      = 0x00000400,
    QUADLENGTH     = 0x00000800,
    MAP3DSCALE     = 0x00001000,
    OTHERHEIGHT    = 0x00002000,
    /// CLASSIFICATION_ALGORITHM = 0x00004000,
    //! Selected value instead of quantiles, or selected value changed value.
    VALUE_SELECTION = 0x00008000,
    BACKGROUND_COLOUR = 0x00010000
  };

  typedef std::vector<ag::DataGuide>::const_iterator
                   const_dataguide_iterator;

/*
0x00020000
0x00040000
0x00080000
0x00100000
0x00200000
0x00400000
0x00800000
*/

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VisEngine           ();

  //! Destructor.
  virtual          ~VisEngine          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  VisEngine&       operator=           (const VisEngine& aEngine);

  void             rescan              (const ag::DataObject& o);

  //! If \a s is true, the visualisation will respond to cursor changes.
  void             setFolowCursor      (bool s);

  void             finishedScanning    (DataObject const& object);

  void             setHeight           (const DataGuide& dataGuide);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           size                () const;

  bool             isEmpty             () const;

  DataGuide const& guide               (size_t index) const;

  const std::vector<DataGuide>& dataGuides() const;

  const_dataguide_iterator begin       () const;

  const_reverse_dataguide_iterator rbegin() const;

  const_dataguide_iterator end         () const;

  const_reverse_dataguide_iterator rend() const;

  //! Returns true if the visualisation should respond to cursor changes.
  bool             folowCursor         ();

  // const CursorPos& cursorPos           () const;

  bool             cursorChanged       () const;

  bool             timeChanged         () const;

  void             addAttribute        (const ag::DataObject& object,
                                        const ag::DataGuide& guide);

  void             clear               ();

  unsigned int     change              () const;

  DataGuide const* heightDataGuide     () const;

  dal::DataSpace const& dataSpace      () const;

  dal::DataSpaceAddress const& dataSpaceAddress() const;

  double           map2DZoom           () const;

  double           map2DScale          () const;

  QPointF          map2DOffset         () const;

// #ifdef DEBUG_DEVELOP
  std::string      changeToString      () const;
// #endif

  QColor const&    backgroundColour    () const;

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
