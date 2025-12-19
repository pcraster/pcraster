#ifndef INCLUDED_CALC_DYNAMICSECTION
#define INCLUDED_CALC_DYNAMICSECTION

#include "stddefx.h"
#include "calc_basicblock.h"


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

  /* virtual */    ~DynamicSection              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void            callEnter                     (ASTVisitor& v) override;
  void            callJump                      (ASTVisitor& v) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  DynamicSection*  createClone                  () const override;
  bool             hasBackBranch                () const override;
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
