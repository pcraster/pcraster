#ifndef INCLUDED_CALC_ASTVISITOR
#define INCLUDED_CALC_ASTVISITOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // ASTVisitor declarations.
  class ASTNode;
  class ASTPar;
  class BaseExpr;
  class ASTNumber;
  class ASTAss;
  class ASTStat;
  class ASTNodeVector;
  class ASTNodeList;
  class PointCodeBlock;
  class DynamicSection;
  class Code;
  class RepeatUntil;
  class NonAssExpr;

  class JumpNode;
  class BlockEntrance;
}



namespace calc {



//! Visitor just as pattern from GangOf4 book
class ASTVisitor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ASTVisitor&           operator=           (const ASTVisitor&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ASTVisitor               (const ASTVisitor&);

protected:
                   ASTVisitor               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


     virtual       ~ASTVisitor              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  // TODO dir visit methos alleen protected en friends van die class?
  // TODO the default visit should visit everything ONCE
  virtual void visitPar (ASTPar      *p);

  //! a default visit all
  /*! The ASTVisitor always visits \a e before it visit its arguments.
   *  The CFGVisitor always vists \a e after it visit its arguments
   */
  virtual void visitExpr          (BaseExpr   *e);

  //! a default visit the embedded expr
  virtual void  visitNonAssExpr   (NonAssExpr *e);

  virtual void visitJumpNode      (JumpNode *n);
  virtual void visitBlockEntrance (BlockEntrance *n);

  virtual void visitNodeList      (ASTNodeList *l);
  virtual void visitPointCodeBlock(PointCodeBlock *);

  virtual void visitNumber        (ASTNumber *n);
  virtual void visitAss           (ASTAss *a);
  virtual void visitStat          (ASTStat *s);

  virtual void  enterDynamicSection(DynamicSection *d);
  virtual void  jumpOutDynamicSection(DynamicSection *d);

  virtual void  enterRepeatUntil(RepeatUntil *d);
  virtual void  jumpOutRepeatUntil (RepeatUntil *d);

  virtual void  enterCode(Code *d);
  virtual void  jumpOutCode (Code *d);


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
