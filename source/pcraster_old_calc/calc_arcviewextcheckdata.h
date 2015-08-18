#ifndef INCLUDED_CALC_ARCVIEWEXTCHECKDATA
#define INCLUDED_CALC_ARCVIEWEXTCHECKDATA

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
