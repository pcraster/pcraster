#ifndef INCLUDED_CALC_ASTNODEVECTOR
#define INCLUDED_CALC_ASTNODEVECTOR

#include "stddefx.h"
#include "calc_astnodecontainer.h"

#include <deque>


namespace calc {
  // ASTNodeVector declarations.
}


namespace calc {



//! Deque of nodes, with random access, such as arguments
class ASTNodeVector: public ASTNodeContainer<std::deque<class ASTNode *> >
{

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  ASTNodeVector&           operator=           (const ASTNodeVector&);

                   ASTNodeVector               (const ASTNodeVector&);


                   ASTNodeVector               ();


  /* virtual */    ~ASTNodeVector              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  AC                 release               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ASTNodeVector* createClone           () const override;

  ASTNode*             operator[]            (size_t i) const;
  ASTNode*             at                    (size_t i) const;

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
