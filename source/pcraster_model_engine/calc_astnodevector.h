#ifndef INCLUDED_CALC_ASTNODEVECTOR
#define INCLUDED_CALC_ASTNODEVECTOR

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_DEQUE
#include <deque>
#define INCLUDED_DEQUE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTNODECONTAINER
#include "calc_astnodecontainer.h"
#define INCLUDED_CALC_ASTNODECONTAINER
#endif


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


  /* virtual */    ~ASTNodeVector              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  AC                 release               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual ASTNodeVector* createClone           () const;

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
