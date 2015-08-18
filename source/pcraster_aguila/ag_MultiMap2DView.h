#ifndef INCLUDED_AG_MULTIMAP2DVIEW
#define INCLUDED_AG_MULTIMAP2DVIEW



// Library headers.
#include <vector>
#include <boost/tuple/tuple.hpp>

// PCRaster library headers.

// Module headers.
#include "ag_Visualisation.h"



class QLineEdit;
namespace ag {
  // MultiMap2DView declarations.
  class DataGuide;
  class DataObject;
  class Map2DView;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \todo      Make this class a Visualisation.
*/
class MultiMap2DView: public Visualisation<>
{

  friend class MultiMap2DViewTest;

private:

  size_t           d_nrRows;

  size_t           d_nrCols;

  std::vector<boost::tuple<QLineEdit*, Map2DView*> >d_mapViews;

  //! Assignment operator. NOT IMPLEMENTED.
  MultiMap2DView&  operator=           (MultiMap2DView const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   MultiMap2DView      (MultiMap2DView const& rhs);

  void             createInterface     ();

  void             setLabel            (size_t row,
                                        size_t col);

  void             rescan              ();

  void             process             ();

  void             visualise           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MultiMap2DView      (DataObject* dataObject,
                                        size_t nrRows = 1,
                                        size_t nrCols = 1,
                                        QWidget* parent = 0);

  /* virtual */    ~MultiMap2DView     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& guide);

  void             addAttribute        (size_t row,
                                        size_t col,
                                        DataGuide const& guide);

  void             zoomAll             ();

  void             resetMapView        ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           nrCols              () const;

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
