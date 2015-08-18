#ifndef INCLUDED_AG_PROBABILITYVALUEPLOT
#define INCLUDED_AG_PROBABILITYVALUEPLOT



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



namespace ag {
  // ProbabilityValuePlot declarations.
}



namespace ag {

//! Class for plots showing probability over value.
/*!
  Examples of such plots are the cumulative probability plot and the exceedence
  probability plot.

  \sa        .
  \todo      Copy stuff from CumDistributionFunctionView to here.
  \todo      Replace all references of CumDistributionFunctionView by
             ProbabilityValuePlot.
  \todo      Remove CumDistributionFunctionView.
*/
class ProbabilityValuePlot: public PlotVisualisation,
                            public ProbabilityPlot
{

  friend class ProbabilityValuePlotTest;

private:

  void             appended            (QwtDoublePoint const& point);

  void             moved               (QwtDoublePoint const& point);

  QSize            minimumSizeHint     () const;

  void             createPlot          ();

  void             extremes            (REAL4* const min,
                                        REAL4* const max);

  bool             onlyCumulativeProbabilitiesShown() const;

  bool             onlyExceedanceProbabilitiesShown() const;

protected:

  void             rescan              ();

  void             process             ();

  void             visualise           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ProbabilityValuePlot(DataObject* object,
                                        QWidget* parent=0);

  /* virtual */    ~ProbabilityValuePlot();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addAttribute        (DataGuide const& dataGuide);

  void             toggleMarker        ();

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
