#ifndef INCLUDED_OLDCALC_ARCVIEWEXTCHECKDATA
#define INCLUDED_OLDCALC_ARCVIEWEXTCHECKDATA

#include "stddefx.h"

#include <string>



namespace calc {

//! hold a name of stack or map that is output in an esrigrid enabled script
typedef struct ArcViewExtCheckData {
  /*!
     \arg isStack boolean value stack map
     \arg name of stack or map
   */
  ArcViewExtCheckData(int isStack, const std::string& name):
    d_isStack(isStack),d_name(name) {}
  //! is it a stack? map otherwise
  int          d_isStack; // isMap otherwise
  //! name of stack prefix if isStack true, name of map otherwise
  std::string  d_name;
} ArcViewExtCheckData;

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
