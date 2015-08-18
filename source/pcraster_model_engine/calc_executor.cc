#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_EXECUTOR
#include "calc_executor.h"
#define INCLUDED_CALC_EXECUTOR
#endif

// Library headers.
#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_PROGRESSCALLBACK
#include "calc_progresscallback.h"
#define INCLUDED_CALC_PROGRESSCALLBACK
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h"
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif
#ifndef INCLUDED_CALC_BASEEXPR
#include "calc_baseexpr.h"
#define INCLUDED_CALC_BASEEXPR
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_POINTCODEBLOCK
#include "calc_pointcodeblock.h"
#define INCLUDED_CALC_POINTCODEBLOCK
#endif
#ifndef INCLUDED_CALC_JUMPNODE
#include "calc_jumpnode.h"
#define INCLUDED_CALC_JUMPNODE
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif

/*!
  \file
  This file contains the implementation of the Executor class.
*/



//#define TRACE_LOG(x)  x
#define TRACE_LOG(x)

//------------------------------------------------------------------------------
// DEFINITION OF STATIC EXECUTOR MEMBERS
//------------------------------------------------------------------------------
namespace calc {
static ProgressCallBack defaultProgressCallBack;
}

//------------------------------------------------------------------------------
// DEFINITION OF EXECUTOR MEMBERS
//------------------------------------------------------------------------------

/*! \brief ctor
 * \param cfg code to execute
 * \param s   settings to create a RunTimeEnv
 * \param table symbols to load into the RunTimeEnv
 * \throws SymException if a certain symbol can not be loaded
 *
 * After construction the Executor is ready to execute \a cfg in the
 * created RunTimeEnv using the symbol set \a symbols that are loaded.
 * No data (possible output) is yet overwritten.
 */
calc::Executor::Executor(
    CFGNode* cfg,
    const RunTimeEnvSettings& s,
    const ASTSymbolTable& table):
  CFGVisitor(cfg),
  d_rte(s),
  d_timeoutput(0),
  d_progressInfo(new ProgressInfo()),
  d_counter(COUNT_NR)
{
  d_progressCallBack=&defaultProgressCallBack;
  d_progressInfo->nrTimeSteps=d_rte.timer().lastInt();
  // start with 0 the initial
  d_progressInfo->inTimeStep =0;
  d_progressInfo->inStatementLineNr=0;

  // will load data table
  for( ASTSymbolTable::const_iterator i=table.begin(); i!=table.end(); ++i)
      d_rte.load(i->second);
}

calc::Executor::~Executor()
{
#ifdef DEBUG_DEVELOP
  if (!std::uncaught_exception()) {
    if (!d_rte.empty())
      DEVELOP_PRECOND(d_rte.empty());
  }
#endif
  delete d_progressInfo;
}

//! set value of d_progressCallBack
void calc::Executor::setProgressCallBack(ProgressCallBack* progressCallBack)
{
  PRECOND(progressCallBack);
  d_progressCallBack=progressCallBack;
}

//! get value of d_progressCallBack
calc::ProgressCallBack* calc::Executor::progressCallBack() const
{
  return d_progressCallBack;
}

//! execute entire cfg and assume all data is cleaned
void calc::Executor::execAll()
{
  execAllKeep();
  DEVELOP_PRECOND(d_rte.empty());
}
void calc::Executor::wrapVisitWithCatch(Visit  v)
{
  try {
    try {
      (this->*v)();
    } catch(...) {
      d_rte.cleanOnException();
      throw;
    }
  } catch(const calc::PosException& ) {
    throw;
  } catch(const com::Exception& e) {
    // if we have a current node, we can position
    // the error to that node
    ASTNode *n=current();
    if (n)
      n->runtimeError(d_rte.timer().currentInt(),e.messages());
    else // else rethrow
      throw;
  } catch(const std::exception& e) {
    ASTNode *n=current();
    if (n)
      n->runtimeError(d_rte.timer().currentInt(),e.what());
    else // else rethrow
      throw;
  }
}

//! execute entire cfg and data may be kept
void calc::Executor::execAllKeep()
{
  d_rte.start();
  wrapVisitWithCatch(&CFGVisitor::visit);
  d_rte.finish();
}

//! execute initial section
/*! a call to startStepWise() must precede execInitial
 *  execInitial executes all statements in the initial
 *  section of the cfg, if the cfg has an initial section.
 *
 *  \returns true if there is only an initial section, false if there is a dynamic
 *           section to be executed yet.
 */
bool calc::Executor::execInitialSection()
{
  // in initial the timestep is 0
  // do setStep until
  //  1: enterDynamicSection is executed or
  //  2: jumpOutCode is executed
  while (d_rte.timer().currentInt()==0 // 1
        && !d_allCodeExecuted          // 2
        ) setStep();

  return d_allCodeExecuted;
}

// Executor::execDynamicOnce (if present and timesteps left)
//! execute dynamic section once
/*! a call to execInitialSection() must precede execDynamicSectionOnce.
 *  If the cfg contains no DynamicSection or all timesteps are
 *  done, nothing is done.
 *  \returns wether all timesteps (if any) are done.
 */
bool calc::Executor::execDynamicSectionOnce()
{
  size_t t=d_rte.timer().currentInt();
  // call to execInitial done
  PRECOND(d_rte.timer().currentInt()!=0 || d_allCodeExecuted);
  // do setStep until
  //  1: enterDynamicSection is executed again
  //  2: jumpOutCode is executed
  while (t==d_rte.timer().currentInt() // 1
         && !d_allCodeExecuted      // 2
         )  {
         setStep();
  }
  return d_allCodeExecuted;
}

//! initialise step wise execution
/*!
 * called before sequence of setStep() calls.
 */
void calc::Executor::startStepWise()
{
  d_rte.start();
  reset();
  d_allCodeExecuted=false;
}

//! finalize step wise visitation
/*!
 * called after sequence of setStep() calls.
 */
void calc::Executor::finishStepWise()
{
  d_rte.finish();
}

//! execute a single node
void calc::Executor::setStep()
{
   wrapVisitWithCatch(&CFGVisitor::visitCurrent);
   advance();
}

/*! pop result field assuming an BaseExpr is visited
    and a result is left on the stack
    for testing only
 */
calc::Field* calc::Executor::popResult()
{
  return d_rte.popField();
}

calc::RunTimeEnv& calc::Executor::runTimeEnv()
{
  return d_rte;
}

//! visit only as rvalue in expression
void calc::Executor::visitPar(ASTPar *p)
{
   d_rte.pushValue(p);
}

void calc::Executor::visitNumber(ASTNumber *n)
{
  d_rte.pushField(n->createNonSpatial());
}

void calc::Executor::execOp(BaseExpr    *o)
{
    o->exec(&d_rte);
}

void calc::Executor::visitExpr(BaseExpr   *e)
{
  TRACE_LOG(std::cout << "executing " << e->name()
            << " at " << e->shortPosText() << std::endl);
  /*
   * alternative is om timeoutput
   * hier een no-op te maken en RunTimeEnv::assignStackTop
   * 2 velden te laten poppen en timeoutput onderdeel van
   * FieldWriter te maken, IS BETER wat we later ook willen
   */
  if (e->op().opCode()==OP_TIMEOUTPUT) {
     d_timeoutput=e;
  } else
     execOp(e);
}

void calc::Executor::visitAss (ASTAss    *a)
{
  if (!d_timeoutput)
   d_rte.assignStackTop(a->pars());
  else {
   // delay to reach  a->pars holding the tss
   PRECOND(a->pars().size()==1);
   d_rte.assignOutTss(a->par()->name());
   d_timeoutput=0;
  }
}

void calc::Executor::visitStat (ASTStat   *)
{
  d_timeoutput=0;
}

void calc::Executor::visitPointCodeBlock(PointCodeBlock* pcb)
{
  pcb->exec(d_rte);
}

//! start of dynamic: increment timestep
void calc::Executor::enterDynamicSection(DynamicSection* )
{
  // at start of DynamicSection
  d_rte.incCurrentTimeStep();
  d_progressInfo->inTimeStep =d_rte.timer().currentInt();
  d_progressCallBack->update(*d_progressInfo);

  d_counter.start(COUNT_DYNAMIC);
}

void calc::Executor::jumpOutDynamicSection(DynamicSection*)
{
  d_counter.stop(COUNT_DYNAMIC);
  // at end of DynamicSection
  if (d_rte.timer().currentInt() < d_rte.timer().lastInt())
    setTakeBackBranch(true);
  else {
    d_allCodeExecuted=true;
  }
  // incr and even set past at end
  d_progressInfo->inTimeStep = d_rte.timer().currentInt() + 1;
  if (d_progressInfo->inTimeStep > d_rte.timer().lastInt()){
    d_progressCallBack->update(*d_progressInfo);
  }
}


void calc::Executor::jumpOutCode(Code *)
{
  // never take back branch, this encloses all code

  d_allCodeExecuted=true;
}


void calc::Executor::enterRepeatUntil(RepeatUntil * )
{
  // at start of RepeatUntil
  d_counter.start(COUNT_REPEAT);
}

void calc::Executor::jumpOutRepeatUntil(RepeatUntil*)
{
  // repeat {   } until (thisIsTrue)
  if (!d_rte.stackedCondition()) {
    // repeat again: !thisIsTrue
    setTakeBackBranch(true);
  } else
    d_counter.stop(COUNT_REPEAT);
}

//! assume the JumpOut... is called first
void calc::Executor::visitJumpNode(JumpNode* j)
{
  if (!takeBackBranch())
    j->deleteForwards(d_rte);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
