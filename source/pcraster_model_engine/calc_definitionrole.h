#ifndef INCLUDED_CALC_DEFINITIONROLE
#define INCLUDED_CALC_DEFINITIONROLE

#include "stddefx.h"



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
