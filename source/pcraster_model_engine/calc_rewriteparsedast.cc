#include "stddefx.h"
#include "calc_rewriteparsedast.h"
#include "calc_astvisitor.h"
#include "calc_baseexpr.h"
#include "calc_aststat.h"
#include "calc_operator.h"
#include "calc_astnodevector.h"

/*!
  \file
  This file contains the implementation of the rewriteParsedAST class.
*/



namespace calc {
 namespace detail {

  //------------------------------------------------------------------------------

  struct ASTRewriter: public ASTVisitor
  {
    ASTRewriter()
      
    {}
  private:
    bool d_timeoutput{false};
    bool d_inDynamicSection{false};
    void visitExpr(BaseExpr *e) override
    {
     Operator const& op(e->op());
     if (op.opCode()==OP_TIMEOUTPUT)
       d_timeoutput=true;
     if (op.isDynamicSectionOperation()) {
        if (!d_inDynamicSection) {
         // pcrcalc37
         e->posError("function '"+e->name()+"' is only legal in the dynamic section");
        }
      }
      e->args()->accept(*this);
    }
    void  enterDynamicSection (DynamicSection *) override {
      d_inDynamicSection=true;
    }
    void  jumpOutDynamicSection (DynamicSection *) override {
      d_inDynamicSection=false;
    }

    void visitStat(ASTStat *s) override
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

