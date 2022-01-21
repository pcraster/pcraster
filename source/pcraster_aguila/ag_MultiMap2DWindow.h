#ifndef INCLUDED_AG_MULTIMAP2DWINDOW
#define INCLUDED_AG_MULTIMAP2DWINDOW


#include "ag_MapWindow.h"

#include <filesystem>


namespace ag {
  // MultiMap2DWindow declarations.
  class MultiMap2D;
  class VisEngine;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class MultiMap2DWindow: public MapWindow
{

  friend class MultiMap2DWindowTest;

private:

  std::vector<VisEngine*> d_engines;

  MultiMap2D*      d_map;

  //! Assignment operator. NOT IMPLEMENTED.
  MultiMap2DWindow& operator=          (MultiMap2DWindow const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   MultiMap2DWindow    (MultiMap2DWindow const& rhs);

  void             createInterface     (size_t nrRows,
                                        size_t nrCols);

  void             saveAsPNG           (std::filesystem::path const& path) override;

  // void             process             ();

  // void             visualise           ();

protected:

  std::string      windowName          () const override;

  bool             dataVisualised      () const override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MultiMap2DWindow    (qt::AppWindowProperties const& props,
                                        DataObject* object,
                                        size_t nrRows = 1,
                                        size_t nrCols = 1);

  /* virtual */    ~MultiMap2DWindow   () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             rescan              () override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& guide) override;

  void             addAttribute        (size_t row,
                                        size_t col,
                                        DataGuide const& guide);

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
