#ifndef INCLUDED_AG_PROBABILITYTIMESERIESPLOT
#include "ag_ProbabilityTimeSeriesPlot.h"
#define INCLUDED_AG_PROBABILITYTIMESERIESPLOT
#endif

// External headers.

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ProbabilityTimeSeriesPlot class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROBABILITYTIMESERIESPLOT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PROBABILITYTIMESERIESPLOT MEMBERS
//------------------------------------------------------------------------------

ProbabilityTimeSeriesPlot::ProbabilityTimeSeriesPlot(
                   DataObject* object,
                   QWidget* parent)

  : PlotVisualisation(object, "Probability Time Series Plot", parent),
    ProbabilityPlot(),
    TimeSeriesPlot()

{
}



ProbabilityTimeSeriesPlot::~ProbabilityTimeSeriesPlot()
{
}



void ProbabilityTimeSeriesPlot::rescan()
{
}



void ProbabilityTimeSeriesPlot::process()
{
}



void ProbabilityTimeSeriesPlot::visualise()
{
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

