#ifndef INCLUDED_AG_PROBABILITYPLOT
#define INCLUDED_AG_PROBABILITYPLOT



// External headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// Project headers.

// Module headers.



namespace ag {
  // ProbabilityPlot declarations.
}



namespace ag {

//! Base class for probability plots.
/*!
  Specializations can show probability over time or probability over value,
  for example. What these plots share is the fact that the dependent variable,
  shown on the y-axis, is the probability.

  \sa        .
*/
class ProbabilityPlot: private boost::noncopyable
{

  friend class ProbabilityPlotTest;

private:

protected:

                   ProbabilityPlot     ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~ProbabilityPlot    ();

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
