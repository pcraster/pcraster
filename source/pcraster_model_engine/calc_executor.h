#ifndef INCLUDED_CALC_EXECUTOR
#define INCLUDED_CALC_EXECUTOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_CPUCYCLECOUNTER
#include "com_cpucyclecounter.h"
#define INCLUDED_COM_CPUCYCLECOUNTER
#endif

// Module headers.
#ifndef INCLUDED_CALC_CFGVISITOR
#include "calc_cfgvisitor.h"
#define INCLUDED_CALC_CFGVISITOR
#endif
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif

namespace geo {
  // Executor declarations.
  class RasterSpace;
}


namespace calc {

class ASTSymbolTable;
class RunTimeEnvSettings;
class ProgressCallBack;
struct ProgressInfo;

//! execute in CFG order
class Executor: private CFGVisitor
{
  friend class P5Stack;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Executor&           operator=           (const Executor&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Executor               (const Executor&);

                   Executor               ();


  RunTimeEnv        d_rte;
  bool              d_allCodeExecuted;
  BaseExpr*         d_timeoutput;
  ProgressCallBack* d_progressCallBack;
  ProgressInfo*     d_progressInfo;

  com::CpuCycleCounter d_counter;

  Field*            popResult       ();

  enum { COUNT_DYNAMIC=0, COUNT_REPEAT=1,COUNT_NR=2 };

  typedef void (CFGVisitor::*Visit)();

  void wrapVisitWithCatch(Visit  v);
  void setStep           ();

  void execOp            (BaseExpr   *o);

  void visitPar          (ASTPar    *p);
  void visitNumber       (ASTNumber *n);
  void visitExpr         (BaseExpr  *e);
  void visitAss          (ASTAss    *a);
  void visitStat         (ASTStat   *s);

  void visitPointCodeBlock  (PointCodeBlock* pcb);
  void enterDynamicSection  (DynamicSection* );
  void jumpOutDynamicSection(DynamicSection* );
  void enterRepeatUntil     (RepeatUntil * );
  void jumpOutRepeatUntil   (RepeatUntil * );
  void visitJumpNode        (JumpNode* j);
  void jumpOutCode          (Code * );

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    Executor        (CFGNode *cfg,
                                     const RunTimeEnvSettings& s,
                                     const ASTSymbolTable& table);

  /* virtual */    ~Executor        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              setProgressCallBack (ProgressCallBack* progressCallBack);

  void              execAll             ();
  void              execAllKeep         ();

  void              finishStepWise      ();
  bool              execInitialSection  ();
  bool              execDynamicSectionOnce();
  void              startStepWise       ();

  RunTimeEnv&       runTimeEnv          ();
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ProgressCallBack* progressCallBack    () const;

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
