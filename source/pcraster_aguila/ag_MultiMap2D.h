#ifndef INCLUDED_AG_MULTIMAP2D
#define INCLUDED_AG_MULTIMAP2D

#include "ag_Map.h"

#include <filesystem>


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

  QSplitter*       d_splitter{};

  MultiMap2DView*  d_multiMap2DView{nullptr};

  LegendView*      d_legendView{nullptr};

  //! Assignment operator. NOT IMPLEMENTED.
  MultiMap2D&      operator=           (MultiMap2D const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   MultiMap2D          (MultiMap2D const& rhs);

  void             createInterface     (size_t nrRows,
                                        size_t nrCols);

  void             rescan              () override;

  void             process             () override;

  void             visualise           () override;

protected Q_SLOTS:

  void             resetMapView        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MultiMap2D          (DataObject* dataObject,
                                        size_t nrRows = 1,
                                        size_t nrCols = 1,
                                        QWidget* parent = nullptr);

  /* virtual */    ~MultiMap2D         () override;

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

  void             saveAsPNG           (std::filesystem::path const& path) const;

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
