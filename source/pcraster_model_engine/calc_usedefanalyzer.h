#ifndef INCLUDED_CALC_USEDEFANALYZER
#define INCLUDED_CALC_USEDEFANALYZER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_PARSET
#include "calc_parset.h"
#define INCLUDED_CALC_PARSET
#endif
#ifndef INCLUDED_CALC_CFGVISITOR
#include "calc_cfgvisitor.h"
#define INCLUDED_CALC_CFGVISITOR
#endif
#ifndef INCLUDED_CALC_IOTYPE
#include "calc_iotype.h"
#define INCLUDED_CALC_IOTYPE
#endif



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

  void             visitNumber                  (ASTNumber *);
  void             visitPar                     (ASTPar *);
  void             visitAss                     (ASTAss *a);
  void             visitJumpNode                (JumpNode *);
  void             visitBlockEntrance           (BlockEntrance *);
  void             visitExpr                    (BaseExpr* e);
  void             visitPointCodeBlock          (PointCodeBlock *b);

  void             visitNonAssExpr              (NonAssExpr   *e);

  void             doExpr(BaseExpr *e);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   UseDefAnalyzer               (CFGNode *cfg,
                                                 bool keepLiveAtEnd,
                                                 bool prefixUseByDefs);

  /* virtual */    ~UseDefAnalyzer              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              setLastUse                  ();

  void              enterDynamicSection         (DynamicSection* d);

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
