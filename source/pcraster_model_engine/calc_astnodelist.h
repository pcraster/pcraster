#ifndef INCLUDED_CALC_ASTNODELIST
#define INCLUDED_CALC_ASTNODELIST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_LIST
#include <list>
#define INCLUDED_LIST
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTNODECONTAINER
#include "calc_astnodecontainer.h"
#define INCLUDED_CALC_ASTNODECONTAINER
#endif


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

  /* virtual */    ~ASTNodeList              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  iterator             begin                 ();
  iterator             end                   ();
  void                 replace               (ASTNode *by,
                                              iterator begin,
                                              iterator end);

  virtual void accept      (ASTVisitor& v);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const_iterator       begin                 () const;
  const_iterator       end                   () const;

  virtual ASTNodeList* createClone           () const;
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
