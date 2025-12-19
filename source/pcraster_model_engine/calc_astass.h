#ifndef INCLUDED_CALC_ASTASS
#define INCLUDED_CALC_ASTASS

#include "stddefx.h"
#include "calc_astnode.h"

#include <vector>


namespace calc {
  // ASTAss declarations.
  class ASTPar;
}



namespace calc {



//! Assignment par(s) = rhs; where rhs is some expression
/*!
   An assignment does (re-)define a value for a number of par(s)
  <pre>
  assignment
    par [ , par ]*
    ass:( = | += | -= | *= | /= )
        ( expr // incl. single functie
          |(
           | timeoutput // \pre ass-token is =
           | func1 , func2
           |  modelName::MethodName  // vergeten we even met een opt.string
           ) '(' exprList ')'
        )
    </pre>
 -  classes are Assignment/ModelLinkInit/ModelLinkMethod/Timeoutput
 - ass different then = are rewritten in parser to par = expr
*/
class ASTAss: public ASTNode
{

private:
  typedef std::vector<ASTPar *> Pars;

  //! list of Left Hand Sided parameters to assign to
  Pars             d_pars;

  //! the right hand side of the assignment
  ASTNode         *d_rhs{nullptr};

  void             clean();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ASTAss               ();
                   ASTAss& operator=    (const ASTAss&);

                   ASTAss               (const ASTAss&);

                   ASTAss               (const ASTPar&  par,
                                         const ASTNode* rhs);

  /* virtual */    ~ASTAss              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void     accept               (ASTVisitor& v) override;

  void             addPar               (const ASTPar&  p);
  void             transferRhs          (ASTNode*       rhs);
  void             setRhs               (const ASTNode* rhs);

  void             swap01               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------


  size_t           nrPars               () const;
  ASTPar*          par                  (size_t parIndex=0) const;
  const Pars&      pars                 () const;
  ASTNode*         rhs                  () const;
  ASTAss*          createClone          () const override;

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
