#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_REWRITEPARSEDAST
#include "calc_rewriteparsedast.h"
#define INCLUDED_CALC_REWRITEPARSEDAST
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif
#ifndef INCLUDED_CALC_BASEEXPR
#include "calc_baseexpr.h"
#define INCLUDED_CALC_BASEEXPR
#endif
#ifndef INCLUDED_CALC_ASTSTAT
#include "calc_aststat.h"
#define INCLUDED_CALC_ASTSTAT
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_ASTNODEVECTOR
#include "calc_astnodevector.h"
#define INCLUDED_CALC_ASTNODEVECTOR
#endif
/*!
  \file
  This file contains the implementation of the rewriteParsedAST class.
*/



namespace calc {
 namespace detail {

  //------------------------------------------------------------------------------

  struct ASTRewriter: public ASTVisitor
  {
    ASTRewriter():
      d_timeoutput(false),
      d_inDynamicSection(false)
    {}
  private:
    bool d_timeoutput;
    bool d_inDynamicSection;
    void visitExpr(BaseExpr *e)
    {
     Operator const& op(e->op());
     if (op.opCode()==OP_TIMEOUTPUT)
       d_timeoutput=true;
     if (op.isDynamicSectionOperation())
      if (!d_inDynamicSection) {
       // pcrcalc37
       e->posError("function '"+e->name()+"' is only legal in the dynamic section");
      }
      e->args()->accept(*this);
    }
    void  enterDynamicSection (DynamicSection *) {
      d_inDynamicSection=true;
    }
    void  jumpOutDynamicSection (DynamicSection *) {
      d_inDynamicSection=false;
    }

    void visitStat(ASTStat *s)
    {
      d_timeoutput=false;
      s->stat()->accept(*this);
      if (d_timeoutput && !s->reportParsed())
        s->setReportParsed(true);
    }
  };
 }


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

/*!
 *  - check: no dynamic operations outside dynamic section
 *  - rewrite: timeoutput statements that are missing the report clause are
 *    fixed since they should always be written.
 *
 *  tests in ParserTest::testCheckAndRewriteParsedAST();
 */
void checkAndRewriteParsedAST(ASTNode *n) {
 detail::ASTRewriter rpa;
 n->accept(rpa);
}


} // namespace calc

