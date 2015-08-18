#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_PROGRESSBAR
#include "com_progressbar.h"
#define INCLUDED_COM_PROGRESSBAR
#endif

// Library headers.
#ifndef INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#include <boost/math/special_functions/round.hpp>
#define INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_DIMAP
#include "com_dimap.h"
#define INCLUDED_COM_DIMAP
#endif

/*!
  \file
  This file contains the implementation of the ProgressBar class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class ProgressBarPrivate
{
public:

  ProgressBarPrivate()
  {
  }

  ~ProgressBarPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROGRESSBAR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PROGRESSBAR MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     stream Stream to print progress bar on.
  \param     width Width of progress bar in number of characters.
*/
com::ProgressBar::ProgressBar(size_t width, std::ostream& stream)

  : IProgressBar(0, width),
    StreamWriter(stream),
    d_nrHashesWritten(0)

{
  POSTCOND(width > 2);
}



//! Constructor.
/*!
  \param     nrActions Number of actions which will be performed.
  \param     stream Stream to print progress bar on.
  \param     width Width of progress bar in characters.
*/
com::ProgressBar::ProgressBar(size_t nrSteps, size_t width,
         std::ostream& stream)

  : IProgressBar(nrSteps, width),
    StreamWriter(stream),
    d_nrHashesWritten(0)

{
  POSTCOND(width > 2);
}



//! Destructor.
/*!
*/
com::ProgressBar::~ProgressBar()
{
}



void com::ProgressBar::clear()
{
  StreamWriter::clear();
  d_nrHashesWritten = 0;

  DEVELOP_POSTCOND(nrCharactersWritten() == 0);
}



void com::ProgressBar::init()
{
  ProgressTracker::init();

  *this << '[';
  for(size_t i = 0; i < width() - 2; ++i) {
    *this << ' ';
  }
  *this << ']';
  flush();

  d_nrHashesWritten = 0;
}



void com::ProgressBar::update()
{
  DEVELOP_PRECOND(nrCharactersWritten() == width());

  // Determine nr of hashes to print.
  com::DiMap map(0, nrSteps(), 0.0, static_cast<double>(width() - 2));
  size_t nrHashes = static_cast<size_t>(
                   boost::math::round(map.invTransform(nrFinishedSteps())));

  if(nrHashes != d_nrHashesWritten) {
    clear();
    *this << '[';

    // Print hashes.
    for(size_t i = 0; i < nrHashes; ++i) {
      *this << '#';
    }

    // Pad with spaces.
    for(size_t i = nrHashes; i < width() - 2; ++i) {
      *this << ' ';
    }

    *this << ']';
    flush();
    d_nrHashesWritten = nrHashes;
  }

  DEVELOP_POSTCOND(nrCharactersWritten() == width());
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

