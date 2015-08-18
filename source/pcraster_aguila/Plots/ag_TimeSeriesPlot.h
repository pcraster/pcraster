#ifndef INCLUDED_AG_TIMESERIESPLOT
#define INCLUDED_AG_TIMESERIESPLOT



// External headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// Project headers.

// Module headers.



namespace ag {
  // TimeSeriesPlot declarations.
}



namespace ag {

//! Base class for time series plots.
/*!
  Specializations can show variation of a value or probability over time, for
  example. What these plots share is the fact that the independent variable,
  shown on the x-axis, is time.

  \sa        .
*/
class TimeSeriesPlot: private boost::noncopyable
{

  friend class TimeSeriesPlotTest;

private:

protected:

                   TimeSeriesPlot      ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~TimeSeriesPlot     ();

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
