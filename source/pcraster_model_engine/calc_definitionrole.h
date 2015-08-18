#ifndef INCLUDED_CALC_DEFINITIONROLE
#define INCLUDED_CALC_DEFINITIONROLE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // DefinitionRole declarations.
}



namespace calc {

//! Role a symbol has specified in its ASTDefinition
enum DefinitionRole {
    NotSpecified= 0,
    Input       = 'i',
    Output      = 'o',
    Constant    = 'c'
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
