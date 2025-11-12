#ifndef INCLUDED_CALC_FINDSYMBOL
#define INCLUDED_CALC_FINDSYMBOL

#include "stddefx.h"
#include "calctypes.h"
#include "major_op.h"

#include <string>


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
