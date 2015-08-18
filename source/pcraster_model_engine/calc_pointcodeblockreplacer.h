#ifndef INCLUDED_CALC_POINTCODEBLOCKREPLACER
#define INCLUDED_CALC_POINTCODEBLOCKREPLACER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_STACK
#include <stack>
#define INCLUDED_STACK
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTNODELIST
#include "calc_astnodelist.h"
#define INCLUDED_CALC_ASTNODELIST
#endif
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif
#ifndef INCLUDED_CALC_POINTCODEBLOCK
#include "calc_pointcodeblock.h"
#define INCLUDED_CALC_POINTCODEBLOCK
#endif
#ifndef INCLUDED_CALC_PARSET
#include "calc_parset.h"
#define INCLUDED_CALC_PARSET
#endif



namespace calc {
  // PointCodeBlockReplacer declarations.
}



namespace calc {

class PointCodeBlock;
class ASTSymbolTable;


/*!
 * \brief
 *  create a list() of PointCodeBlock classes on the accepting AST
 *   and insert them into the accepting AST
 *
 * Find ranges (begin,end) in ASTNodeList nodes that are
 * suitable for replacement by a PointCodeBlock.
 *
 * \todo
 *  result d_list kan sanity check krijgen, wat local is in pred kan geen 
 *  input zijn in succ *    -> tricky met repeat/dynamic ?
 */
class PointCodeBlockReplacer : public ASTVisitor
{

private:
  void visitNonAssExpr             (NonAssExpr *e);
  void visitExpr                   (BaseExpr *o);
  void visitAss                    (ASTAss *a);
  void visitNodeList               (ASTNodeList *l);
  void visitPar                    (ASTPar *p);

  class  BlockInfo {
  public:
   bool                   d_allPoint;
   ParSet                 d_pars;
   size_t                 d_nrPointsOps;
   BlockInfo():
     d_allPoint(false),
     d_nrPointsOps(0)                  {};
   BlockInfo(BlockInfo const& rhs):
     d_allPoint(rhs.d_allPoint),
     d_pars(rhs.d_pars),
     d_nrPointsOps(rhs.d_nrPointsOps) {};
   BlockInfo&  operator=( BlockInfo const& rhs) {
     if(this != &rhs) {
      d_allPoint   =rhs.d_allPoint;
      d_pars       =rhs.d_pars;
      d_nrPointsOps=rhs.d_nrPointsOps;
     }
     return *this;
   }
  };

  typedef ASTNodeList::iterator I;

  ASTSymbolTable const& d_symbols;
  //! ASTNodeList are embedded in each other, keep info in a stack
  std::stack<BlockInfo>   d_info;
  BlockInfo&        info() {
    return d_info.top();
  }

  ParSet                         d_parsInExpr;
  size_t                         d_nrPointsOpsInExpr;

  //! list of created PointCodeBlock's
  std::vector<PointCodeBlock *> d_list;


  //! Assignment operator. NOT IMPLEMENTED.
  PointCodeBlockReplacer&           operator=           (PointCodeBlockReplacer const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                    PointCodeBlockReplacer              (PointCodeBlockReplacer const& rhs);

  void              addBlock                            (ASTNodeList *l,I begin, I end);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    PointCodeBlockReplacer              (ASTSymbolTable const& symbols);

  /* virtual */    ~PointCodeBlockReplacer              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const std::vector<PointCodeBlock *>& list             () const;

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
