#ifndef INCLUDED_CALC_CATCHALLEXCEPTIONS
#define INCLUDED_CALC_CATCHALLEXCEPTIONS

#include "com_catchallexceptions.h"
#include "calc_posexception.h"


#define CATCH_POS(stream) \
 catch (const calc::PosException &p) {   \
  (stream)<<p.messages(); }

#define CATCH_ALL_EXCEPTIONS(stream)     \
  CATCH_ALL(stream,"ERROR: ",CATCH_POS(stream))

#endif
