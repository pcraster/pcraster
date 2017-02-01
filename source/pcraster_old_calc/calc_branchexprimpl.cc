#include "stddefx.h"

#ifndef INCLUDED_MISC
#include "misc.h" // bitset macro's
#define INCLUDED_MISC
#endif

#ifndef INCLUDED_CALC_BRANCHEXPRIMPL
#include "calc_branchexprimpl.h"
#define INCLUDED_CALC_BRANCHEXPRIMPL
#endif

#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif

#ifndef INCLUDED_CALC_FIELDSTACK
#include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif

#ifndef INCLUDED_CALC_STACKCR
#include "calc_stackcr.h"
#define INCLUDED_CALC_STACKCR
#endif

#ifndef INCLUDED_CALC_GLOBARGS
#include "calc_globargs.h"
#define INCLUDED_CALC_GLOBARGS
#endif
#ifndef INCLUDED_CALC_GLOBRESULT
#include "calc_globresult.h"
#define INCLUDED_CALC_GLOBRESULT
#endif
#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_GENERATESPATIALFUNC
#include "calc_generatespatialfunc.h"
#define INCLUDED_CALC_GENERATESPATIALFUNC
#endif

#ifndef INCLUDED_CALC_LIBERROR
#include "calc_liberror.h"
#define INCLUDED_CALC_LIBERROR
#endif

#ifndef INCLUDED_CALC_OPERATIONTIMER
#include "calc_operationtimer.h"
#define INCLUDED_CALC_OPERATIONTIMER
#endif

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

// needed for exec EXEC_DOUBLE
#ifndef INCLUDED_CALC_DOUBLEASS
#include "calc_doubleass.h"
#define INCLUDED_CALC_DOUBLEASS
#endif

#include <algorithm>

extern "C" {
/* implementation of the operations */
#include "dassfunc.h"
#include "dbinfunc.h"
#include "dunfunc.h"
#include "genfunc.h"
#include "globfunc.h"
#include "sbinfunc.h"
#include "sunfunc.h"

}

calc::BranchExprImpl::BranchExprImpl(
  const calc::Element& pos,
  const calc::Operator&  op,
        calc::FieldExprArgs& fieldArgs):
  calc::BranchExpr(pos,op,fieldArgs),
  d_countStarted(false)
{
}


#ifdef WIN32

#ifndef INCLUDED_WINDOWS
#include <windows.h>
#define INCLUDED_WINDOWS
#endif

/*
class OpTimer {
  OpTimer(const calc::Operator& o) {};
};
*/

void calc::BranchExprImpl::execute(calc::FieldStack& stack)
{
#ifdef BORLANDC
   try {
#endif 
    try {
    executeOperation(op(),stack);

    // ooit uitgezet waarom?
    if (errno == ERANGE)
       throw std::runtime_error("range error");

#ifdef BORLANDC
    /* MINGW does suppot this see ~/SEH*txt for
       some possible alternatives, or do signal/trap?
     */
    } __except(EXCEPTION_EXECUTE_HANDLER) {
     DWORD e = GetExceptionCode();
     const char *m;
     char msg[128];
     switch(e) {
      case EXCEPTION_FLT_DIVIDE_BY_ZERO:
      case EXCEPTION_INT_DIVIDE_BY_ZERO:
      m = "division by (non-spatial) 0";
      break;
      case EXCEPTION_FLT_OVERFLOW:
      case EXCEPTION_INT_OVERFLOW:
      m = "numerical overflow, check input values of operation";
      break;
      case EXCEPTION_FLT_UNDERFLOW:
      m = "numerical underflow, check input values of operation";
      break;
      default: {
      sprintf(msg,
       "Internal error (code=%x), please report to support@pcraster.nl",e);
       m=msg;
      break;
      }
     }
     throw std::runtime_error(m);
    }
#endif 
   } catch(std::exception& v) {
     calc::FieldExpr::runtimeError(v.what());
   }
}

#else

/*
#ifndef INCLUDED_HASH_SET
#include <hash_set>
#define INCLUDED_HASH_SET
#endif
struct eqstr
{
    bool operator()(const char* s1, const char* s2) const
    {
              return strcmp(s1, s2) == 0;
    }
};
class ExpensiveOperations {
public:
  std::hash_set<const char *,std::hash<const char *>,eqstr> d_ops;
  ExpensiveOperations() {
    d_ops.insert("**");
    d_ops.insert("asin");
    d_ops.insert("cos");
    d_ops.insert("exp");
    d_ops.insert("log10");
    d_ops.insert("rounddown");
    d_ops.insert("sin");
    d_ops.insert("sqrt");
    d_ops.insert("tan");
  };
  bool inSet(const char *op) {
     return d_ops.find(op) != d_ops.end();
  }
};
static ExpensiveOperations eo;
*/
#ifndef INCLUDED_COM_CPUCYCLECOUNTER
#include "com_cpucyclecounter.h"
#define INCLUDED_COM_CPUCYCLECOUNTER
#endif

class OpTimer {
  int d_timer;
  static int getTimer(const calc::Operator& o) {
  if (o.name() == "*")
      return 1;
  if (o.name() == "+")
      return 2;
  if (o.name() == "/")
      return 3;
/*
  if (eo.inSet(o.name().c_str()))
      return 4;
*/
  return 0;
 }
public:
  OpTimer(const calc::Operator& o) {
    d_timer=getTimer(o);
   // if (d_timer)
   //  startCpuCycleCounter(d_timer);
  }
  ~OpTimer() {
   // if (d_timer)
   //  stopCpuCycleCounter(d_timer);
  }
};

void calc::BranchExprImpl::execute(calc::FieldStack& stack)
{
  try {
   // count if result is spatial or first arg is spatial (maptotal)
   // bool count = spatial() || (nrArgs() && fieldArgs()[0]->spatial());
   OPERATION_TIMER(op().name(),count);
   executeOperation(op(),stack);
  } catch(std::exception& v) {
   calc::FieldExpr::runtimeError(v.what());
  }
}

#endif

static size_t nrCells(
    const calc::FieldHandle o1,
    const calc::FieldHandle o2)
{
  DEVELOP_PRECOND(std::max<size_t>(o1->nrValues(),o2->nrValues()) > 0);
  return std::max<size_t>(o1->nrValues(),o2->nrValues());
}

static void checkMv(
  const calc::Operator& op,
  const calc::FieldHandle v)
{
  if (v->isMv())
   throw std::runtime_error("Domain error on function/operator "+op.name());
}

/* return the proper value scale conversion based
 * on internal conversion matrix
 */
static MAJOR_CODE VsConvoperator(
  VS from, VS to)
{
  const MAJOR_CODE convTable[6][6] = {
  /* indexed by 2log valuescale                                         */
  /* from:  |   to:                                                      */
  /*        |   VS_B 1    VS_N 4    VS_O 4    VS_S s    VS_D s    VS_L 1 */
  /* VS_B 1 */ {OP_NOP  , OP_1_2_4, OP_1_2_4, OP_1_2_S, OP_1_2_D ,OP_ILL },
  /* VS_N 4 */ {OP_4_2_B, OP_NOP  , OP_NOP  , OP_4_2_S, OP_4_2_D ,OP_4_2_L },
  /* VS_O 4 */ {OP_4_2_B, OP_NOP  , OP_NOP  , OP_4_2_S, OP_4_2_D ,OP_4_2_L },
  /* VS_S s */ {OP_S_2_B, OP_S_2_4, OP_S_2_4, OP_NOP  , OP_S_2_D ,OP_S_2_L },
  /* VS_D s */ {OP_S_2_B, OP_S_2_4, OP_S_2_4, OP_D_2_S, OP_NOP   ,OP_D_2_L },
  /* VS_L 1 */ {OP_1_2_B, OP_1_2_4, OP_1_2_4, OP_1_2_S, OP_L_2_D ,OP_NOP }};
  from = biggestVs(from);
  PRECOND(NRBITSET_TYPE(from,VS) == 1);
  PRECOND(NRBITSET_TYPE(to  ,VS) == 1);
  PRECOND(FIRSTBITSET_TYPE(from,VS) < 6);
  PRECOND(FIRSTBITSET_TYPE(to  ,VS) < 6);
  /* OP_ILL entries should not have passed the typechecking rules: */
  PRECOND(convTable[FIRSTBITSET_TYPE(from,VS)][FIRSTBITSET_TYPE(to  ,VS)]
          != OP_ILL);

  return convTable[FIRSTBITSET_TYPE(from,VS)][FIRSTBITSET_TYPE(to  ,VS)];
}

void calc::BranchExprImpl::executeVarArgOperation(
  const calc::Operator& implOp,
  calc::FieldStack& stack)
{
  const Args& args = fieldArgs();
  size_t n = nrArgs();

  /* this must be the case if
   * the selection of OP_OP_[12S] must work
   */
  PRECOND(nrInSet(vs()) == 1);

  /* call each time with two operands
   * first time the last two args
   * then each time the result of the
   * previous (stack top) and a new arg
   * start with last argument
   * COVER needs this order
   * => leftmost arg on top
   */
  bool leftSpatial;
  bool rightSpatial = args[n-1]->spatial();
  args[n-1]->execute(stack);
  for (int i = n-2; i >= 0; i--) {
    leftSpatial  = args[i]->spatial();
    args[i]->execute(stack);
    execSameBin(implOp,stack,leftSpatial,rightSpatial,true);
    rightSpatial = leftSpatial || rightSpatial;
  }
}


void calc::BranchExprImpl::executeOperation(
  const calc::Operator& implOp,
  calc::FieldStack& stack)
{
  const Args& args = fieldArgs();
  size_t n = nrArgs();

  const MAJOR_CODE polys[][3] = {
#include "polyjmp.inc"
  { OP_NOP, OP_NOP, OP_NOP }};

  const MAJOR_CODE trigs[][2] = {
#include "trigjmp.inc"
  { OP_NOP,  OP_NOP }};

  /* pick an implementation operator for some
   * functions or call special handler
   */
  switch(implOp.exec()) {
   case EXEC_MISC:
     // ONLY THIS OPERATION
     PRECOND(implOp.opCode() == OP_TEST_UNTIL);
     executeArgs(stack);
     break;
   case EXEC_POLY:
   {/* pick the implementation, using the LAST
     * argument of the node
     * (first is bool in if/if_else)
     */
     if (implOp.opCode() == OP_SPATIAL) {
       if (args[0]->spatial()) {
        executeArgs(stack); /* avoid typecasting a spatial to a spatial */
        return;
       }
     }
     MAJOR_CODE newOp = polys[implOp.execId()][stackCellRepr(args[n-1]->vs())];
     POSTCOND(newOp != OP_NOP);
     if (implOp.cg() == CG_VARARG) {
          // handle there
         executeVarArgOperation(major2op(newOp),stack);
         return;
     }
     executeOperation(major2op(newOp),stack);
   } break;
   case EXEC_TRIG:
   {/* pick the implementation,
     * depending if the argument if the node is VS_S or VS_D
     */
     int ds; /* 0 dir implementation, 1 scalar implementation */
     PRECOND(n==1); /* cos,sin,tan */
     ds = isIn(VS_S, args[0]->vs()) ? 1 : 0;
     executeOperation(major2op(trigs[implOp.execId()][ds]),stack);
   } break;
   case EXEC_CONV: {
           PRECOND(n == 1);
           MAJOR_CODE newOp = VsConvoperator(args[0]->vs(), vs());
           /* this can introduce OP_NOP if no conversion
            * is neccessary
            */
           if (newOp == OP_NOP) {
             executeArgs(stack);
             return;
           }
           executeOperation(major2op(newOp),stack);
         } break;
   case  EXEC_DOUBLE: {
          MAJOR_CODE newOp = dassImplementor(implOp.opCode());
          MAJOR_CODE delOp = otherDouble(implOp.opCode());
          VS         resultVs[2];
          int        resultPos = stackPosition(implOp.opCode());
          int        delPos    = stackPosition(delOp);
          resultVs[resultPos] = vs();
          resultVs[delPos]    = biggestVs(major2op(delOp).vs());

          executeDoubleAss(major2op(newOp), stack, resultVs);

          FieldHandle top   = stack.popReadOnly();
          FieldHandle under = stack.popReadOnly();
          if (resultPos)
            stack.push(top); // result on top
           else
            stack.push(under);
        } break;
  case EXEC_SAME_UN:    execSameUn(implOp,stack); break;
  case EXEC_SAME_BIN:   execSameBin(implOp,stack,
                                    args[0]->spatial(),
                                    args[1]->spatial(),false);
                                    break;
  case EXEC_GLOBAL:     execGlob(implOp,stack);          break;
  case EXEC_GEN_NS:     execGenNonSpatial(implOp,stack); break;
  case EXEC_GEN_SP:     execGenSpatial(implOp,stack);    break;
  case EXEC_DIFF_UN:    execDiffUn(implOp,stack);        break;
  case EXEC_DIFF_BIN:   execDiffBin(implOp,stack);       break;
  case EXEC_IFTHENELSE: execIfThenElse(implOp,stack);    break;
  case EXEC_IFTHEN:     execIfThen(implOp,stack);        break;
  case EXEC_EXTERN:     execExternal(implOp, stack);     break;
  default: /* do nothing */ ;
                        POSTCOND(FALSE);
 }
}

/*!
   \todo in calc-lib: do fgrep Error on calc lib to see all funcs that need
    a catch of domain errors, and streamline error messages for
    them. execGlob and execExternal also does this, a new domain check object is already
    in fieldapi and applied to dynamicwave
 */
void calc::BranchExprImpl::executeDoubleAss(
    const calc::Operator& implOp,
    calc::FieldStack& stack,
    VS   resultVs[2])
{
  executeArgs(stack);
  PRECOND(implOp.exec() == EXEC_DASS);

  typedef int (*DO_DASS)(void *out1, void *out2, const void **ins);
   /* return 0 if everything went well
    *  non-zero otherwise
    */
  typedef struct DASS_JMP {
    MAJOR_CODE op[2];
    DO_DASS    func;
  } DASS_JMP;
  static const DASS_JMP jmpTableDass[] = {
  # include "dassjmp.inc"
   {{OP_NOP, OP_NOP}, (DO_DASS)NULL}
  };
  const DASS_JMP *f = jmpTableDass+(implOp.execId());

  int nrOpds = implOp.nrArgs();
  POSTCOND(nrOpds > 0);
  (void)nrOpds; // shut up compiler

  GlobResult result0(major2op(f->op[0]).vs(), resultVs[0], compressor());
  GlobResult result1(major2op(f->op[1]).vs(), resultVs[1], compressor());
  GlobArgs      args(implOp,compressor(),stack);

  if (f->func(result0.MAPinterface(),result1.MAPinterface(), args.mapVals())) {
   // pcrcalc/test349-351
   FieldExpr::runtimeError(
    "Domain error on function/operator "+op().name()+":\n"+getLibError());
  }

  stack.push(result0.createField());
  stack.push(result1.createField());
}


void calc::BranchExprImpl::execSameUn(
  const Operator& op,
  FieldStack& stack)
{
  executeArgs(stack);
  PRECOND(op.exec() == EXEC_SAME_UN);

  typedef void (*DO_SAME_UN)(void *values, size_t nrValues);
  static const DO_SAME_UN jmpTableSameUn[] = {
#   include "sunjmp.inc"
   (DO_SAME_UN)NULL
  };

  DO_SAME_UN f = jmpTableSameUn[op.execId()];
  POSTCOND(f != NULL);

  FieldHandle v = stack.popDest(vs());

  { // OpTimer t(op);
    f(v->destValue(),v->nrValues());
  }
  checkMv(op,v);
  stack.push(v);
}

void calc::BranchExprImpl::execDiffUn(
  const calc::Operator& op,
  calc::FieldStack& stack)
{
  executeArgs(stack);
  PRECOND(op.exec() == EXEC_DIFF_UN);
  typedef void (*DO_DIFF_UN)(void *result, const void *values, size_t nrValues);
  static const DO_DIFF_UN jmpTableDiffUn[] = {
#   include "dunjmp.inc"
   (DO_DIFF_UN)NULL
  };
        DO_DIFF_UN f = jmpTableDiffUn[op.execId()];
  POSTCOND(f != NULL);

  FieldHandle res = createResultField();
  FieldsPopped inp(stack,1);
  if (!inp[0]->isSpatial())
    switch(op.opCode()) {
    // these 2 always have spatial result, no matter if their
    // argument is spatial or not.
     case OP_UNIFORM: f = (DO_DIFF_UN)Do_uniform_1; break;
     case OP_NORMAL:  f = (DO_DIFF_UN)Do_normal_1;
     default : ;
    }
  // note that one or both can be nonspatial
  f(res->destValue(),inp[0]->srcValue(),nrCells(inp[0],res));
  checkMv(op,res);
  stack.push(res);
}

static int currentTime;
/* ARGSUSED */
void Do_time(REAL4 *values, size_t n)
{
    PRECOND(n==1);
    (void)n; // shut up compiler
    *values = (REAL4)currentTime;
}
/* ARGSUSED */
void Do_timeslice(REAL4 *values, size_t n)
{
    PRECOND(n==1);
    (void)n; // shut up compiler
    *values = (REAL4)1;
}

void calc::BranchExprImpl::execGenNonSpatial(
  const Operator& op,
  FieldStack& stack)
{
  // no args: so no executeArgs(stack)
  PRECOND(op.exec() == EXEC_GEN_NS);
  typedef void (*DO_GEN_NS)(void *values, size_t nrvalues);
  static const DO_GEN_NS jmpTableGen[] = {
#  include "genjmp.inc"
   (DO_GEN_NS)NULL
  };
        DO_GEN_NS f = jmpTableGen[op.execId()];
  POSTCOND(f != NULL);

  currentTime= scriptConst().currentTimeStep();

  FieldHandle res = createResultField();
  f(res->destValue(),res->nrValues());
  checkMv(op,res);
  stack.push(res);
}

void calc::BranchExprImpl::execGenSpatial(
  const Operator& op,
  FieldStack& stack)
{
  executeArgs(stack);
  PRECOND(op.exec() == EXEC_GEN_SP);
  // xcoordinate, ycoordinate and uniqid

  FieldsPopped inp(stack,1);

  FieldHandle res = createResultField();
  REAL4 *valRes=static_cast<REAL4 *>(res->destValue());

  GenerateSpatialFunc f(
    static_cast<const UINT1 *>(inp[0]->srcValue()),inp[0]->nrValues(),
    compressor());
  switch(op.opCode()) {
    case OP_XCOORDINATE:
      f.xcoordinate(valRes); break;
    case OP_YCOORDINATE:
      f.ycoordinate(valRes); break;
    case OP_UNIQUEID:
      f.uniqueid(valRes); break;
    default:
      POSTCOND(FALSE);
  }
  stack.push(res);
}

typedef enum  FIELD_ARG { NN=0, SS=1, NS=2, SN=3 } FIELD_ARG;

static FIELD_ARG fieldArg(
  bool leftSpatial,
  bool rightSpatial)
{
        if (leftSpatial)
     return rightSpatial ? SS : SN;
  return rightSpatial ? NS : NN;
}

static int getSubIndex(
  FIELD_ARG fieldArg)
{
  switch (fieldArg) {
    case SS:
    case NN: return 0;
    case NS: return 1;
    case SN: return 2;
  }
  return 2;
}


void calc::BranchExprImpl::execDiffBin(
  const calc::Operator& op,
  calc::FieldStack& stack)
{
  executeArgs(stack);
  typedef void (*DO_DIFF_BIN)(UINT1 *r, const void *val1, const void *val2, size_t nrValues);

#  define Do_eq_s_sn  NULL
#  define Do_eq_1_sn  NULL
#  define Do_eq_4_sn  NULL
#  define Do_ne_s_sn  NULL
#  define Do_ne_1_sn  NULL
#  define Do_ne_4_sn  NULL
  static const DO_DIFF_BIN jmpTableDiffBin[][3] = {
#  include "dbinjmp.inc"
   {(DO_DIFF_BIN)NULL , (DO_DIFF_BIN)NULL, (DO_DIFF_BIN)NULL}
  };

  PRECOND(op.exec() == EXEC_DIFF_BIN);

  calc::FieldHandle res = createResultField();
  calc::FieldsPopped inp(stack,2);

  FIELD_ARG fa = fieldArg(inp[0]->isSpatial(),inp[1]->isSpatial());
  DO_DIFF_BIN f = jmpTableDiffBin[op.execId()][getSubIndex(fa)];
  POSTCOND(f != NULL);

  f((UINT1 *)res->destValue(),inp[0]->srcValue(),inp[1]->srcValue(),nrCells(inp[0],inp[1]));
  checkMv(op,res);
  stack.push(res);
}

void calc::BranchExprImpl::execSameBin(
  const calc::Operator& op,
  calc::FieldStack& stack,
  bool leftSpatial,
  bool rightSpatial,
  bool argsAlreadyOnStack)
{
  if (!argsAlreadyOnStack)
    executeArgs(stack);
  PRECOND(op.exec() == EXEC_SAME_BIN);
  /* 0 = SS, NN
   * 1 = NS
   * 2 = SN
   */
  typedef int (*DO_SAME_BIN)(void *val1, void *val2, size_t nrValues);
       /* return 1 if in case of NS or SN the non-spatial value
        *    creates a domain error (for example A/0)
        *        0 if no error
        */

  /* omitted funcs due to
   * communativity:
   */

#  define Do_max_s_sn NULL
#  define Do_max_4_sn NULL
#  define Do_min_s_sn NULL
#  define Do_min_4_sn NULL
#  define Do_or_sn    NULL
#  define Do_xor_sn   NULL
#  define Do_and_sn   NULL
#  define Do_mul_sn   NULL
#  define Do_badd_sn  NULL
  static const DO_SAME_BIN jmpTableSameBin[][3] = {
#  include "sbinjmp.inc"
   {(DO_SAME_BIN)NULL , (DO_SAME_BIN)NULL, (DO_SAME_BIN)NULL}
  };

  FIELD_ARG fa = fieldArg(leftSpatial,rightSpatial);

  int i = getSubIndex(fa);
  DO_SAME_BIN f = jmpTableSameBin[op.execId()][i];

  POSTCOND(f != NULL);

  /* right only as target in case of NS: */
  if (fa == NS) {
    calc::FieldHandle left(stack.popReadOnly());
    calc::FieldHandle right(stack.popDest(vs()));
    // CW note I do not need this (void *)
    //  if the jmpTableSameBin is better organized
    { // OpTimer t(op);
      f((void *)left->srcValue(),right->destValue(),nrCells(left,right));
    }
    checkMv(op,right);
    stack.push(right);
  } else {
    calc::FieldHandle dest(stack.popReadOnly());
    calc::FieldHandle other(stack.popReadOnly());
    if (fa == SS && op.cg() == CG_COMM && !dest.isOnlyHandle()) {
      //! try if swap, get a stack tmp for dest
      calc::FieldHandle tmp(other);
      other = dest;
      dest  = tmp;
    }
    stack.push(dest);
    dest = stack.popDest(vs());

    // CW note I do not need this (void *)
    //  if the jmpTableSameBin is better organized
    { // OpTimer t(op);
      f(dest->destValue(),(void *)other->srcValue(),nrCells(dest,other));
    }
    checkMv(op,dest);
    stack.push(dest);
  }
}

calc::FieldHandle calc::BranchExprImpl::conditionalBranch(
  bool skip,
  calc::FieldExpr *branch,
  calc::FieldStack& stack)
{
  if (skip)  {
    branch->skipExecution();
    return branch->createResultField();
  } else {
    branch->execute(stack);
    return stack.popReadOnly();
  }
}

void calc::BranchExprImpl::execIfThen(
  const calc::Operator& op,
  calc::FieldStack& stack)
{
  typedef void (*DO_IFTHEN)(void *result,
    const UINT1 *test, const void *true_expr, size_t nrValues);

  PRECOND(op.exec() == EXEC_IFTHEN);
  /*      if ( test_expr(inp[0])
           then true_expr(inp[1])
     else MV same type as true  )
     ==>
    execute test_expr
    if ( none are true) {
      create dummy true_expr field,
        which is never used in the impl-func
    } else
      execute true_expr
    exec_func
   */

  const Args& args = fieldArgs();

  args[0]->execute(stack);
  calc::FieldHandle testBranch = stack.popReadOnly();
  PRECOND(testBranch->vs() == VS_B); // pcrcalc/test66
  bool noneAreTrue,noneAreFalse;
  testBranch->analyzeBoolean(noneAreTrue,noneAreFalse);

  calc::FieldHandle trueBranch = conditionalBranch(noneAreTrue, args[1], stack);

  static const DO_IFTHEN jmpTableIfThen[][3] = {
#  include "ifthenjmp.inc"
   {(DO_IFTHEN)NULL , (DO_IFTHEN)NULL, (DO_IFTHEN)NULL}
  };

  calc::FieldHandle res = createResultField();

  FIELD_ARG fa = fieldArg(testBranch->isSpatial(),trueBranch->isSpatial());
  DO_IFTHEN f = jmpTableIfThen[op.execId()][getSubIndex(fa)];
  POSTCOND(f != NULL);

  f(res->destValue(),(const UINT1 *)testBranch->srcValue(),trueBranch->srcValue(),
      nrCells(testBranch,trueBranch));

  checkMv(op,res);
  stack.push(res);
}

void calc::BranchExprImpl::execIfThenElse(
  const calc::Operator& op,
  calc::FieldStack& stack)
{
  PRECOND(op.exec() == EXEC_IFTHENELSE);
  const Args& args = fieldArgs();
  /*      if ( test_expr(inp[0])
           then true_expr(inp[1])
     else false_expr(inp[2]) )
     ==>
    execute test_expr
    if ( none are true) {
      create dummy true_expr field,
        which is never used in the impl-func
    } else
      execute true_expr
    if ( none are false)
      create dummy false_expr field,
        which is never used in the impl-func
      else
        execute false_expr
    exec_func
   */
  calc::FieldHandle *inp[3];

  args[0]->execute(stack);
  calc::FieldHandle testBranch = stack.popReadOnly();
  inp[0] = &testBranch;
  PRECOND(testBranch->vs() == VS_B); // pcrcalc/test65
  bool noneAreTrue,noneAreFalse;
  testBranch->analyzeBoolean(noneAreTrue,noneAreFalse);

  calc::FieldHandle falseBranch = conditionalBranch(noneAreFalse, args[2], stack);
  inp[2] = &falseBranch;
  calc::FieldHandle trueBranch = conditionalBranch(noneAreTrue, args[1], stack);
  inp[1] = &trueBranch;

  typedef void (*DO_IFTHENELSE)(void *r, const UINT1 *test,
           const void *t, const void *f, size_t nrValues);

  static const DO_IFTHENELSE jmpTableIfThenElse[][8] = {
#  include "ifthenelsejmp.inc"
  {(DO_IFTHENELSE)NULL,(DO_IFTHENELSE)NULL,(DO_IFTHENELSE)NULL,(DO_IFTHENELSE)NULL,
  (DO_IFTHENELSE)NULL,(DO_IFTHENELSE)NULL,(DO_IFTHENELSE)NULL,(DO_IFTHENELSE)NULL},
  };

  calc::FieldHandle res = createResultField();
  size_t n=0;
  int indM[3] = { 4,2,1},ind=0;
  for (size_t i=0; i < 3 ; i++) {
    n = MAX(n,(*inp[i])->nrValues());
    ind |= (*inp[i])->isSpatial() ? indM[i] : 0;
  }
  POSTCOND(ind >= 0 && ind < 8);
  DO_IFTHENELSE f = jmpTableIfThenElse[op.execId()][ind];
  POSTCOND(f != NULL);

  f(res->destValue(),(const UINT1*)testBranch->srcValue(),
      trueBranch->srcValue(),falseBranch->srcValue(),n);
  checkMv(op,res);
  stack.push(res);
}

void calc::BranchExprImpl::execGlob(
  const Operator& implOp,
  FieldStack& stack)
{
  executeArgs(stack);
  PRECOND(implOp.exec() == EXEC_GLOBAL);

  typedef int (*DO_GLOBAL)(void *out, const void **ins);
  /* return 0 if everthing went well
   * non-zero otherwise
   */
  static const DO_GLOBAL jmpTableGlob[] = {
  # include "globjmp.inc"
   (DO_GLOBAL)NULL
  };
  DO_GLOBAL f = jmpTableGlob[implOp.execId()];
  POSTCOND(f != NULL);

  int nrOpds = implOp.nrArgs();
  POSTCOND(nrOpds > 0);
  (void) nrOpds; // shut up compiler

  GlobResult result(implOp.vs(),vs(),compressor());
  GlobArgs   args(implOp,compressor(),stack);

  if (f(result.MAPinterface(),args.mapVals()))
     FieldExpr::runtimeError( // pcrcalc/test348
         std::string("Domain error on function/operator "+op().name()
                              +":\n"+getLibError()));

  stack.push(result.createField());
}


void calc::BranchExprImpl::execExternal(const Operator& op, FieldStack& stack)
{
  executeArgs(stack);
  try {
    PRECOND(op.exec() == EXEC_EXTERN);

    GlobResult result(vs(),vs(),compressor());
    GlobArgs args(op,compressor(),stack,nrArgs());
    void *r[1];
    r[0]=result.MAPinterface();
    if (externalExecute(op.opCode(),r, args.mapVals(),nrArgs())) {
     FieldExpr::runtimeError(
     "Domain error on (dll) function "+op.name()+":\n"+getLibError());
    }

    stack.push(result.createField());
  } catch(std::exception& v) {
    FieldExpr::runtimeError(v.what());
  }
}
