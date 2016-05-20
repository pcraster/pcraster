#ifndef INCLUDED_CALC_LEXINPUTCREATOR
#define INCLUDED_CALC_LEXINPUTCREATOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // LexInputCreator declarations.
}



namespace calc {

class LexInput;

//! Interface for creation of LexInput object
class LexInputCreator
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  LexInputCreator&           operator=           (LexInputCreator const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   LexInputCreator               (LexInputCreator const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LexInputCreator               () {};

  virtual         ~LexInputCreator              ()  {};

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual LexInput*        createLexInput        () const=0;

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
