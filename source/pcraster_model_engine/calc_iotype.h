#ifndef INCLUDED_CALC_IOTYPE
#define INCLUDED_CALC_IOTYPE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif


// Module headers.


namespace calc {
  // IOType declarations.
}



namespace calc {



//! The Input and Output type of Variable in a script
class IOType
{
public:
  typedef pcrxml::ModelInputType  Input;
  typedef pcrxml::ModelOutputType Output;

private:

  Input            d_input;
  Output           d_output;


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   IOType               ();

                   IOType               (const Input&  input, const Output& output);

                   IOType               (IOType const& rhs) = default;

  /* virtual */    ~IOType              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  IOType&          operator=           (IOType const& rhs) = default;
  void             setInput            (const Input&  input);
  void             setOutput           (const Output& output);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  Input            input               () const;
  Output           output              () const;

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

} // namespace calc

#endif
