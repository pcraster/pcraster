#ifndef INCLUDED_APP_OPTIONS
#define INCLUDED_APP_OPTIONS


#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------
#ifdef __cplusplus
 extern "C" {
#endif

typedef struct SAVE_STRTOK {
  struct SAVE_STRTOK_IMPL *data;
} SAVE_STRTOK;

SAVE_STRTOK createSaveStrtok(const char *s);
void deleteSaveStrtok(SAVE_STRTOK s);
const char* nextSaveStrtok(SAVE_STRTOK s);

BOOL app_setDynamicLibraries( const char *flag);

#ifdef __cplusplus
 }
#endif


#endif
