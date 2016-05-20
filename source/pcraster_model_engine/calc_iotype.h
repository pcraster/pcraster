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

/* DEFAULT

  //  Assignment operator.
  IOType&           operator=           (IOType const& rhs);

  //  Copy constructor.
                   IOType               (IOType const& rhs);
*/
  Input            d_input;
  Output           d_output;


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   IOType               ();

                   IOType               (Input  input, Output output);

  /* virtual */    ~IOType              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setInput            (Input  input);
  void             setOutput           (Output output);

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
