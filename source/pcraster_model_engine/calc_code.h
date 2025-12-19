#ifndef INCLUDED_CALC_CODE
#define INCLUDED_CALC_CODE

#include "stddefx.h"
#include "calc_basicblock.h"


namespace calc {
  // Code declarations.
}



namespace calc {



//! complete code
class Code : public BasicBlock
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Code&           operator=           (const Code& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Code               (const Code& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Code               (ASTNode*       transferredStatements);

  /* virtual */    ~Code              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void            callEnter                     (ASTVisitor& v) override;
  void            callJump                      (ASTVisitor& v) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  Code*           createClone                   () const override;
  bool            hasBackBranch                 () const override;
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
