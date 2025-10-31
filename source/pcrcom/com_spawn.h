#ifndef INCLUDED_COM_SPAWN
#define INCLUDED_COM_SPAWN

#include "stddefx.h"

#include <string>



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
