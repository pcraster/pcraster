#ifndef INCLUDED_CALC_DYNAMICSECTION
#define INCLUDED_CALC_DYNAMICSECTION



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_BASICBLOCK
#include "calc_basicblock.h"
#define INCLUDED_CALC_BASICBLOCK
#endif


namespace calc {
  // DynamicSection declarations.
}



namespace calc {



//! dynamic section
class DynamicSection : public BasicBlock
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  DynamicSection&           operator=           (const DynamicSection& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DynamicSection               (const DynamicSection& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DynamicSection               (
                                  const Position*    posOfDynamicKeyword,
                                  ASTNode*           transferredStatements);

  /* virtual */    ~DynamicSection              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void            callEnter                     (ASTVisitor& v);
  void            callJump                      (ASTVisitor& v);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  DynamicSection*  createClone                  () const;
  bool             hasBackBranch                () const;
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

#endif
