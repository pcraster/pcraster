#ifndef INCLUDED_CALC_STACKINFO
#define INCLUDED_CALC_STACKINFO

#include "stddefx.h"
#include "vsenum.h"

#include <string>



namespace calc {
  // StackInfo declarations.
  class FileWriter;
}



namespace calc {



//! data collection for IoFieldStrategy::setStackInfo()
struct StackInfo
{
  //! do d_min and d_max have a valid value
  bool   d_minMaxSet{};
  //! min and max value of whole stack
  double d_min{}, d_max{};
  //! number of time steps of the model (not the stack!)
  size_t d_nrTimeSteps{};
  //! value scale of stack
  VS     d_vs;
  //! name of stack
  std::string d_stackName;
  //! the filewriter, knows which timesteps are written!
  const FileWriter *d_fileWriter{};
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
