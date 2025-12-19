#ifndef INCLUDED_CALC_ASTNODELIST
#define INCLUDED_CALC_ASTNODELIST

#include "stddefx.h"
#include "calc_astnodecontainer.h"

#include <list>


namespace calc {
  // ASTNodeList declarations.
}



namespace calc {



//! List of nodes, used for statements
/*!
 * \todo
 *   needed for PointCodeBlockReplacer, but maybe it should merge with
 *   BasicBlock, ASTScript::d_code should always be a BasicBlock in good
 *   compiler terms.
*/
class ASTNodeList : public ASTNodeContainer<std::list<class ASTNode *> >
{

private:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  ASTNodeList&     operator=                 (ASTNodeList const& rhs);

                   ASTNodeList               (ASTNodeList const& rhs);


                   ASTNodeList               ();

  /* virtual */    ~ASTNodeList              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  iterator             begin                 ();
  iterator             end                   ();
  void                 replace               (ASTNode *by,
                                              iterator begin,
                                              iterator end);

  void accept      (ASTVisitor& v) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const_iterator       begin                 () const;
  const_iterator       end                   () const;

  ASTNodeList* createClone           () const override;
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
