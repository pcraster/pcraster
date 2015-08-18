#ifndef INCLUDED_CALC_CATCHALLEXCEPTIONS
#define INCLUDED_CALC_CATCHALLEXCEPTIONS

#ifndef INCLUDED_COM_CATCHALLEXCEPTIONS
# include "com_catchallexceptions.h"
# define INCLUDED_COM_CATCHALLEXCEPTIONS
#endif

#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif

#define CATCH_POS(stream) \
 catch (const calc::PosException &p) {   \
  stream<<p.messages(); }

#define CATCH_ALL_EXCEPTIONS(stream)     \
  CATCH_ALL(stream,"ERROR: ",CATCH_POS(stream))

#endif
