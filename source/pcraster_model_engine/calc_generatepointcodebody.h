#ifndef INCLUDED_CALC_GENERATEPOINTCODEBODY
#define INCLUDED_CALC_GENERATEPOINTCODEBODY



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_PARSET
#include "calc_parset.h"
#define INCLUDED_CALC_PARSET
#endif



namespace calc {
  // GeneratePointCodeBody declarations.
  class ASTNode;
}



namespace calc {

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

// defined in calc_pointcodebodygenerator.cc
void generatePointCodeBody(
      std::ostream& s,
      ASTNode*      code,
      const ParSet& vContents);

} // namespace calc

#endif
