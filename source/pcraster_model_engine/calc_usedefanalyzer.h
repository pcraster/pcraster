#ifndef INCLUDED_CALC_USEDEFANALYZER
#define INCLUDED_CALC_USEDEFANALYZER

#include "stddefx.h"
#include "calc_parset.h"
#include "calc_cfgvisitor.h"
#include "calc_iotype.h"

#include <map>
#include <string>


namespace calc {
  // UseDefAnalyzer declarations.
  class IOType;
}



namespace calc {



//! A simplified version of live analysis
/*!
   The simplification is that with only the DynamicSection and RepeatUntil as
   control structures, we know that everything is executed/visited at least once.
   This (I think) in contrast with an if-then(-else) like structures.

   This visitor does not alter the CFG or any nodes in the CFG
*/
class UseDefAnalyzer : public CFGVisitor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  UseDefAnalyzer&           operator=           (const UseDefAnalyzer& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   UseDefAnalyzer               (const UseDefAnalyzer& rhs);

  class UseDefRecorder*  d_rec;
  bool                   d_keepLiveAtEnd;

  void             visitNumber                  (ASTNumber *) override;
  void             visitPar                     (ASTPar *) override;
  void             visitAss                     (ASTAss *a) override;
  void             visitJumpNode                (JumpNode *) override;
  void             visitBlockEntrance           (BlockEntrance *) override;
  void             visitExpr                    (BaseExpr* e) override;
  void             visitPointCodeBlock          (PointCodeBlock *b) override;

  void             visitNonAssExpr              (NonAssExpr   *e) override;

  void             doExpr(BaseExpr *e);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   UseDefAnalyzer               (CFGNode *cfg,
                                                 bool keepLiveAtEnd,
                                                 bool prefixUseByDefs);

  /* virtual */    ~UseDefAnalyzer              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              setLastUse                  ();

  void              enterDynamicSection         (DynamicSection* d) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  std::map<std::string,IOType> ioTypes           () const;
  ParSet            inputSet                    () const;
  ParSet            newLiveDefSet               () const;

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

void                         setLastUse   (CFGNode *cfg,
                                           bool keepLiveAtEnd=false);
ParSet                       inputSet     (CFGNode   *cfg);
ParSet                       newLiveDefSet(CFGNode   *cfg);
std::map<std::string,IOType> ioTypes      (CFGNode   *cfg);

} // namespace calc

#endif
