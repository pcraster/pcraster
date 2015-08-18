#ifndef INCLUDED_AG_TIMEPLOT
#define INCLUDED_AG_TIMEPLOT



#include "ag_Visualisation.h"

// Library headers.
#include <boost/filesystem/path.hpp>

// PCRaster library headers.

// Module headers.



class QSplitter;
namespace ag {
  // TimePlot declarations.
  class DataGuide;
  class DataObject;
  class LegendView;
  class PlotView;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class TimePlot: public Visualisation<>
{

private:

  QSplitter*       _splitter;

  PlotView*        _plotView;

  LegendView*      _legendView;

  void             createInterface     (DataObject* object);

  void             rescan              ();

  void             process             ();

  void             visualise           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TimePlot            (DataObject* object,
                                        QWidget* parent = 0);

  /* virtual */    ~TimePlot           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (const DataGuide& dataGuide);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  QSize            sizeHint            () const;

  void             saveAsPNG           (boost::filesystem::path const& path) const;

  // void             saveAsEPS           (com::PathName const& filename) const;

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
