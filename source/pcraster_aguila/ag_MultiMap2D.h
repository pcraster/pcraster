#ifndef INCLUDED_AG_MULTIMAP2D
#define INCLUDED_AG_MULTIMAP2D



// Library headers.
#include <boost/filesystem/path.hpp>

// PCRaster library headers.

// Module headers.
#include "ag_Map.h"



class QSplitter;
namespace ag {
  // MultiMap2D declarations.
  class DataGuide;
  class DataObject;
  class MultiMap2DView;
  class LegendView;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class MultiMap2D: public Map
{

  friend class MultiMap2DTest;

private:

  Q_OBJECT

  QSplitter*       d_splitter;

  MultiMap2DView*  d_multiMap2DView;

  LegendView*      d_legendView;

  //! Assignment operator. NOT IMPLEMENTED.
  MultiMap2D&      operator=           (MultiMap2D const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   MultiMap2D          (MultiMap2D const& rhs);

  void             createInterface     (size_t nrRows,
                                        size_t nrCols);

  void             rescan              ();

  void             process             ();

  void             visualise           ();

protected Q_SLOTS:

  void             resetMapView        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MultiMap2D          (DataObject* dataObject,
                                        size_t nrRows = 1,
                                        size_t nrCols = 1,
                                        QWidget* parent = 0);

  /* virtual */    ~MultiMap2D         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& guide);

  void             addAttribute        (size_t row,
                                        size_t col,
                                        DataGuide const& guide);

  // void             zoomAll             ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           nrCols              () const;

  void             saveAsPNG           (boost::filesystem::path const& path) const;

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
