#ifndef INCLUDED_COM_SPAWN
#define INCLUDED_COM_SPAWN



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



namespace com {
  // Spawn declarations.
}



namespace com {


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------


int spawn(const char *exeName, const char **args);

int spawn(const char *exeName);

int spawn(const std::string& exeName,
               const std::string& otherArgs);


} // namespace com

#endif
