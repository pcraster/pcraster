#ifndef INCLUDED_CALC_EXECUTESCRIPTSTATUS
#define INCLUDED_CALC_EXECUTESCRIPTSTATUS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // ExecuteScriptStatus declarations.
}

namespace calc {


//! status of executeScriptStatus()
/*!
 *  a script is either running or is terminated
 *  on some condition. ExecuteScriptStatus enumarates
 *  these conditions plus the running condition.
 *  \todo
 *    make IsRunningExecScript the 0 case
 */
typedef enum ExecuteScriptStatus  {
     ErrorExecScript=0,  /*!< syntax or runtime error occured */
     FinishedExecScript, /*!< script finished normally */
     FileOutputValueExecScript, /*!< -e was set, and and expr eval'ed to 0 */
     CanceledExecScript, /*!< user requested cancel, by a ProgressCallBack */
     IsRunningExecScript /*!< busy reading/parsing or executing script or
                              better: None of the above */
} ExecuteScriptStatus;

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

inline bool running(ExecuteScriptStatus  e) {
  return e==IsRunningExecScript;
}
inline bool finished(ExecuteScriptStatus  e) {
  return e!=IsRunningExecScript;
}

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
