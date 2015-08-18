#ifndef INCLUDED_CALC_ASTPATH
#define INCLUDED_CALC_ASTPATH



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // ASTPath declarations.
}



namespace calc {

class ASTNode;

// define in calc_astpathtest.cc
ASTNode *path(ASTNode* root, const char *pathStr);

template<typename T>
T *astCast(ASTNode* root, const std::string& pathStr) {
    return dynamic_cast<T *>(path(root,pathStr.c_str()));
  }



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
