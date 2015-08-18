#ifndef INCLUDED_DAL_TIMESTEPMAPPER
#define INCLUDED_DAL_TIMESTEPMAPPER



// Library headers.
#ifndef INCLUDED_BOOST_DATE_TIME_POSIX_TIME_POSIX_TIME
#include <boost/date_time/posix_time/posix_time.hpp>
#define INCLUDED_BOOST_DATE_TIME_POSIX_TIME_POSIX_TIME
#endif

// PCRaster library headers.
#ifndef INCLUDED_DEV_COMPILER
#include "dev_Compiler.h"
#define INCLUDED_DEV_COMPILER
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif



namespace dal {
  // TimeStepMapper declarations.
}



namespace dal {



//! This mappers maps time steps to real world dates.
/*!
  The mapper is initialized using a time step, a date and a time step duration.
  Using this information the mapper can map source indices to destination dates
  and destination dates to source indices.
*/
class PCR_DAL_DECL TimeStepMapper
{

  friend class TimeStepMapperTest;

private:

  //! Source dimension index.
  double           d_index;

  //! Point in time corresponding with source dimension index.
  boost::posix_time::ptime d_time;

  //! Duration of one step in time.
  boost::posix_time::time_duration d_duration;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TimeStepMapper      ();

                   TimeStepMapper      (double index,
                                        boost::posix_time::ptime const& time,
                                        boost::posix_time::time_duration const& duration);

  virtual          ~TimeStepMapper     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  TimeStepMapper&  operator|=          (TimeStepMapper const& rhs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  double           index               () const;

  boost::posix_time::ptime time        () const;

  boost::posix_time::time_duration const& duration() const;

  bool             isValid             () const;

  double           source              (boost::posix_time::ptime const& time) const;

  boost::posix_time::ptime destination (double index) const;

  bool             equals              (TimeStepMapper const& mapper) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

#ifdef DEBUG_DEVELOP
std::ostream&      operator<<          (std::ostream& stream,
                                        TimeStepMapper const& mapper);
#endif



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
