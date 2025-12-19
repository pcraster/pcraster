#ifndef INCLUDED_CALC_FINDSYMBOL
#define INCLUDED_CALC_FINDSYMBOL

#include "stddefx.h"
#include "pcraster_model_engine_export.h"
#include "calc_types.h"
#include "major_op.h"


#include <string>


namespace calc {
  // FindSymbol declarations.
  class ModelLink;
  class Operator;
}



namespace calc {

PCR_ME_EXPORT const Operator* major2op     (MAJOR_CODE major);
PCR_ME_EXPORT const Operator* opName2op    (const std::string& opName,size_t nrArgs=2);

PCR_ME_EXPORT void     loadCalcLib         (const std::string& dllName);

size_t             nrInternalOpCodes   ();


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
