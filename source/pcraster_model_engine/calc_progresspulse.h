#ifndef INCLUDED_CALC_PROGRESSPULSE
#define INCLUDED_CALC_PROGRESSPULSE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // ProgressPulse declarations.
}



namespace calc {

//! describe pulse frequency of the progress call back
/*! 
 *  Describes what progress is made: next loop, next statement etc.
 */
typedef enum ProgressPulse {
    NoPulse=0,    /*!< Not used */
    LoopPulse,      /*!< Next loop/timestep */
    StatementPulse, /*!< Next statement */
    OperationPulse  /*!< next operation/expression, NOT YET IMPLEMENTED */
} ProgressPulse;

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
