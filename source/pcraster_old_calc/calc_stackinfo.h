#ifndef INCLUDED_CALC_STACKINFO
#define INCLUDED_CALC_STACKINFO

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif


namespace calc {
  // StackInfo declarations.
  class FileWriter;
}



namespace calc {



//! data collection for IoFieldStrategy::setStackInfo()
struct StackInfo
{
  //! do d_min and d_max have a valid value
  bool   d_minMaxSet;
  //! min and max value of whole stack
  double d_min, d_max;
  //! number of time steps of the model (not the stack!)
  size_t d_nrTimeSteps;
  //! value scale of stack
  VS     d_vs;
  //! name of stack
  std::string d_stackName;
  //! the filewriter, knows which timesteps are written!
  const FileWriter *d_fileWriter;
/*
private:

  //! Assignment operator. NOT IMPLEMENTED.
  StackInfo&           operator=           (const StackInfo&);

  //! Copy constructor. NOT IMPLEMENTED.
                   StackInfo               (const StackInfo&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   StackInfo               ();

     virtual     ~StackInfo              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
*/
};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
