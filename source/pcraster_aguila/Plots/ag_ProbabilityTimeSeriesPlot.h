#ifndef INCLUDED_AG_PROBABILITYTIMESERIESPLOT
#define INCLUDED_AG_PROBABILITYTIMESERIESPLOT



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_AG_PLOTVISUALISATION
#include "ag_PlotVisualisation.h"
#define INCLUDED_AG_PLOTVISUALISATION
#endif

#ifndef INCLUDED_AG_PROBABILITYPLOT
#include "ag_ProbabilityPlot.h"
#define INCLUDED_AG_PROBABILITYPLOT
#endif

#ifndef INCLUDED_AG_TIMESERIESPLOT
#include "ag_TimeSeriesPlot.h"
#define INCLUDED_AG_TIMESERIESPLOT
#endif



namespace ag {
  // ProbabilityTimeSeriesPlot declarations.
}



namespace ag {

//! Class for plots showing probability over time.
/*!
  These plots show the temporal variation of probability.Probability can be
  cumulative probability or exceedence probability.

  \sa        .
  \todo      Copy probability stuff from PlotView to here.
  \todo      Replace all references of PlotView in prob cases by
             ProbabililityTimeSeriesPlot.
  \todo      Remove PlotView.
*/
class ProbabilityTimeSeriesPlot: public PlotVisualisation,
                                 public ProbabilityPlot,
                                 public TimeSeriesPlot
{

  friend class ProbabilityTimeSeriesPlotTest;

private:

protected:

  void             rescan              ();

  void             process             ();

  void             visualise           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ProbabilityTimeSeriesPlot(DataObject* object,
                                        QWidget* parent=0);

  /* virtual */    ~ProbabilityTimeSeriesPlot();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

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
