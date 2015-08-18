#ifndef INCLUDED_CALC_FINDSYMBOL
#define INCLUDED_CALC_FINDSYMBOL

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
#ifndef INCLUDED_CALCTYPES
#include "calctypes.h"
#define INCLUDED_CALCTYPES
#endif

#ifndef INCLUDED_MAJOR_OP
#include "major_op.h"
#define INCLUDED_MAJOR_OP
#endif



namespace calc {
  // FindSymbol declarations.
  class ModelLink;
  class Operator;
}



namespace calc {

const Operator&    major2op            (MAJOR_CODE major);

      size_t       nrInternalOpCodes   ();

const Operator&    funcName2op         (const std::string& funcName);

void               externalBuildType   (VS& newVs,
                                        bool& isSpatial,
                                        MAJOR_CODE major,
                                        const OP_ARGS* args,
                                        int nrArgs);

int                externalExecute     (MAJOR_CODE major,
                                        void **out,
                                        const void **in,
                                        int nrArgs);

ModelLink*         createModelLink     (const std::string& name);

} // namespace calc

#endif
