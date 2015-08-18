#ifndef INCLUDED_CALC_OPERATIONTIMER
#define INCLUDED_CALC_OPERATIONTIMER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#define OPERATION_TIMER(key,doCount)
// #ifndef __linux__
// #define OPERATION_TIMER(key,doCount)
// #else
// #define OPERATION_TIMER(key,doCount) calc::OperationTimer otOnlyOne(key,doCount)

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.



namespace calc {
  // OperationTimer declarations.
}



namespace calc {

//! Implements a timer for operations
/*!
   Works only under Linux, because of gcc assembler code in 
   com_cpucyclecounter.cc. Use ENABLE_TIMER macro to hide
   it under non-linux.

  Added OperationTimer to:
   void calc::BranchExprImpl::execute 
   void calc::LookupExpr::execute 
   void calc::TimeinputExpr::execute 
  Other descendants of calc::FieldExpr are not relevant for lisflood

  <ul>
   <li>pow as multiply not corrected</li>
   <li> mul includes sqr</li>
   <li> sqrt seems too fast, recheck</li>
   <li> split up if( B then E ) and if ( B then E1 else E2)</li>
  </ul>
*/
class OperationTimer
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  OperationTimer&           operator=           (const OperationTimer&);

  //! Copy constructor. NOT IMPLEMENTED.
                   OperationTimer               (const OperationTimer&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   OperationTimer               (const std::string& operationId,bool count);

  /* virtual */    ~OperationTimer              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  static void print(size_t skipBits=0);
  static void setTimerOn(bool on);

};



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

// __linux__ defined
// #endif 

#endif
