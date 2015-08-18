#ifndef INCLUDED_AG_VALUETIMESERIESPLOT
#define INCLUDED_AG_VALUETIMESERIESPLOT



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_AG_PLOTVISUALISATION
#include "ag_PlotVisualisation.h"
#define INCLUDED_AG_PLOTVISUALISATION
#endif

#ifndef INCLUDED_AG_TIMESERIESPLOT
#include "ag_TimeSeriesPlot.h"
#define INCLUDED_AG_TIMESERIESPLOT
#endif



namespace ag {
  // ValueTimeSeriesPlot declarations.
}



namespace ag {

//! Class for plots showing value over time.
/*!
  These plots show the temporal variation of a value. This is the common
  time series plot.

  \sa        .
  \todo      Replace all references of PlotView in non-prob cases by
             ValueTimeSeriesPlot.
*/
class ValueTimeSeriesPlot: public PlotVisualisation,
                           public TimeSeriesPlot
{

  friend class ValueTimeSeriesPlotTest;

private:

  void             appended            (QwtDoublePoint const& point);

  void             moved               (QwtDoublePoint const& point);

  bool             recreatePlotRequired() const;

  bool             replotRequired      () const;

protected:

  void             rescan              ();

  void             process             ();

  void             visualise           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ValueTimeSeriesPlot (DataObject* object,
                                        QWidget* parent=0);

  /* virtual */    ~ValueTimeSeriesPlot();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             createPlot          (
                                  std::vector<DataGuide> const& dataGuides);

  void             addAttribute        (DataGuide const& dataGuide);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
