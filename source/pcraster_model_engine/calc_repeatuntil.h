#ifndef INCLUDED_CALC_REPEATUNTIL
#define INCLUDED_CALC_REPEATUNTIL



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
  // RepeatUntil declarations.
}



namespace calc {



//! dynamic section
class RepeatUntil : public BasicBlock
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  RepeatUntil&           operator=           (const RepeatUntil& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   RepeatUntil               (const RepeatUntil& rhs);

  ASTNode*         d_condition{};
  void             init();
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RepeatUntil               (
                                  const Position*    posOfRepeatKeyword, // ?
                                  ASTNode*           statements);


  /* virtual */    ~RepeatUntil              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             callEnter                 (ASTVisitor& v) override;
  void             callJump                  (ASTVisitor& v) override;
  void             transferCondition         (ASTNode* condition);
  void             accept                    (ASTVisitor& v) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ASTNode*         condition                 () const;
  RepeatUntil*     createClone               () const override;
  bool             hasBackBranch             () const override;

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
