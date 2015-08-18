#ifndef INCLUDED_CALC_CFGNODE
#define INCLUDED_CALC_CFGNODE

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // CFGNode declarations.
  class ASTNode;
}



namespace calc {



//! Control flow graph node
/*!
 * keep copy ctor and assignment op hidden, this class should not have
 * copy semantics?
 */
class CFGNode
{
public:
  enum { Forward=0, Back=1, NrSuccs=2 } SuccType;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  CFGNode&           operator=           (const CFGNode& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CFGNode               (const CFGNode& rhs);


  //! this pointer is not owned
  ASTNode         *d_node;
  /*! (FTTB NOT USED) these pointers are back ptrs that are NOT deleted
   *  since they are also used in the CFG "thread" in d_succ
   *  Only partial tested
   */
  CFGNode         *d_pred[NrSuccs];
  /*!
   * JumpNode is the only ASTNode, using the 2nd d_succ[1]. d_succ[0] always
   * points Forward in a linear view of a script, d_succ[1] points Back.
   * For example the JumpNode of the DynamicSection will have d_succ[Forward] set 0;
   * no more code, and d_succ[Back] pointing to the beginning of the dynamic script.
   * 
   * Forward is created when building up the CFG, while Back is set to an already 
   * existing node.
   */
  CFGNode         *d_succ[NrSuccs];

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CFGNode               ();

                   CFGNode               (ASTNode *thisNode);

  /* virtual */    ~CFGNode              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setForward          (CFGNode *succ);
  void             setBack             (CFGNode *succ);
  void             setPred             (CFGNode *pred);
  void             init                ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  CFGNode*         succ                (size_t i) const;
  CFGNode*         pred                () const;
  ASTNode*         node                () const;
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
