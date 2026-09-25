#ifndef INCLUDED_CALC_DEFINITIONROLE
#define INCLUDED_CALC_DEFINITIONROLE

#include "stddefx.h"

#include <cstdint>


namespace calc {
  // DefinitionRole declarations.
}



namespace calc {

//! Role a symbol has specified in its ASTDefinition
enum DefinitionRole : std::uint8_t {
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
