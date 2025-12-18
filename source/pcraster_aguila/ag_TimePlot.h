#ifndef INCLUDED_AG_TIMEPLOT
#define INCLUDED_AG_TIMEPLOT

#include "ag_Visualisation.h"

#include <filesystem>


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

  QSplitter*       _splitter{};

  PlotView*        _plotView{nullptr};

  LegendView*      _legendView{nullptr};

  void             createInterface     (DataObject* object);

  void             rescan              () override;

  void             process             () override;

  void             visualise           () override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TimePlot            (DataObject* object,
                                        QWidget* parent = nullptr);

  /* virtual */    ~TimePlot           () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (const DataGuide& dataGuide);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  QSize            sizeHint            () const override;

  void             saveAsPNG           (std::filesystem::path const& path) const;

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
