#include "stddefx.h"

#ifndef  INCLUDED_CALC_DOUBLEASS
#include "calc_doubleass.h"
#define  INCLUDED_CALC_DOUBLEASS
#endif

#ifndef  INCLUDED_CALC_FIELDLEFT
#include "calc_fieldleft.h"
#define  INCLUDED_CALC_FIELDLEFT
#endif

#ifndef  INCLUDED_CALC_BRANCHEXPRIMPL
#include "calc_branchexprimpl.h"
#define  INCLUDED_CALC_BRANCHEXPRIMPL
#endif

#ifndef  INCLUDED_CALC_FIELDSTACK
#include "calc_fieldstack.h"
#define  INCLUDED_CALC_FIELDSTACK
#endif

#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif

#ifndef  INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define  INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_OPERATIONTIMER
#include "calc_operationtimer.h"
#define INCLUDED_CALC_OPERATIONTIMER
#endif

static const struct {
    MAJOR_CODE dassOpImplementor;
    int        index; /* 0 means stack top, first arg of func
                       * 1 below that, 2nd arg of func
           */
 } doubleTable[] = {
#include  "double.inc"
    { OP_NOP, -1 }};

#ifdef  DEBUG_DEVELOP
static void doubleTableSanityCheck()
{
  size_t i;
  for (i = 0; i < ARRAY_SIZE(doubleTable)-1; i++)
    if (doubleTable[i].index == 0)
      PRECOND(nrInSet(calc::major2op(doubleTable[i].dassOpImplementor).vs()) == 1);
}
#endif


MAJOR_CODE calc::dassImplementor(MAJOR_CODE op)
{
    PRECOND(calc::major2op(op).exec() == EXEC_DOUBLE);
    PRECOND(calc::major2op(op).execId() < (int)ARRAY_SIZE(doubleTable)-1);
    return doubleTable[calc::major2op(op).execId()].dassOpImplementor;
}

int calc::stackPosition(MAJOR_CODE op)
{
    PRECOND(calc::major2op(op).exec() == EXEC_DOUBLE);
    PRECOND(calc::major2op(op).execId() < (int)ARRAY_SIZE(doubleTable)-1);
    return doubleTable[calc::major2op(op).execId()].index;
}

MAJOR_CODE calc::otherDouble(MAJOR_CODE op)
{
  MAJOR_CODE pairs[][2] = {
#include "dassop.inc"
         { OP_NOP, OP_NOP }}; /* dummy entry */
        size_t i;

  for (i = 0; i < ARRAY_SIZE(pairs)-1; i++)
   if (op == pairs[i][0])
     break;

   /* if not found then pairs[i][j] == OP_NOP, the dummy entry */
  return pairs[i][1];
}

namespace calc {

static void CheckDoubleAss(
  const Element&  posFunc,
  const Operator& f0,
  const Operator& f1)
{
  if (f0.opCode() != calc::otherDouble(f1.opCode()) ) { /* pcrcalc/test11[bc] */
    posFunc.posError("Functions "+quote(f0.name())+" and "+quote(f1.name())
        +" can not be combined");
  }
}

static bool  NeedSwap(
  const Element&     posFunc,
  const Operator& f0,
  const Operator& f1)
{
  CheckDoubleAss(posFunc,f0,f1);
  PRECOND( f0.exec() == EXEC_DOUBLE && f1.exec()==EXEC_DOUBLE);

  PRECOND(f0.execId() < (int)ARRAY_SIZE(doubleTable)-1);
  PRECOND(f1.execId() < (int)ARRAY_SIZE(doubleTable)-1);
  bool swap = doubleTable[f0.execId()].index > doubleTable[f1.execId()].index;
  return swap;
}

/* this is a user error, only one id on left side
 */
void wrongDoubleAssignment(
  const Element& left0,
  const Operator& func0,
  const Operator& func1)

{
  CheckDoubleAss(left0,func0,func1);
  /* functions are ok but we miss a left argument pcrcalc/test101
   * current grammar does not pick this wrong syntax
   */
  left0.posError("Combination of functions "+quote(func0.name())+" and "+quote(func1.name())+
                          "requires two arguments left of '='-symbol");
}

}

// calc::DoubleAssignment(inclIn,leftPars[0],w,
//  leftPars[0],leftPars[1],sym0,op0,op1,args));
calc::DoubleAssignment::DoubleAssignment(
    StatementBlock *b,
    const Element& pos,
    const WriteInfo& w,
    const UsePar& p0,
    const UsePar& p1,
    const Element&     posFunc,
    const Operator& f0,
    const Operator& f1,
          FieldExprArgs& args):
  Statement(pos),
  d_swapped(NeedSwap(posFunc,f0,f1)),
  d_op0(d_swapped ? f1 : f0),
  d_op1(d_swapped ? f0 : f1),
  d_right(0)
{
  try {
   d_left[0]=0; d_left[1]=0;
   d_right   = new calc::BranchExprImpl(posFunc,d_op1,args);
   d_left[0] = new calc::FieldLeft(b,w,d_swapped ? p1:p0, d_op0.vs());
   d_left[1] = new calc::FieldLeft(b,w,d_swapped ? p0:p1,d_right->vs());
   buildTypes();
  } catch (...) {
   cleanUp();
   throw;
  }
}

calc::DoubleAssignment::~DoubleAssignment()
{
 cleanUp();
}

void calc::DoubleAssignment::cleanUp()
{
  delete d_right;
  delete d_left[0];
  delete d_left[1];
}

bool calc::DoubleAssignment::buildTypes()
{
#ifdef  DEBUG_DEVELOP
  // all current doubles have 1 arguments that is always
  //  (VS_S or VS_LLD (lddcreate),spatial)
  PRECOND(nrInSet(d_op0.vs()) == 1);
  doubleTableSanityCheck();
#endif
  //  the other one is in case of spread depend
  //  on the first arg of the func,
  //  put the non-VS  in the
  //   d_left->restrictUser(d_right)
  //  construct, the other can we can check by a fixed FieldType.
  d_right->buildTypesRecursive(d_left[1]->vs());
  bool spatialPromotion=false;
  if (d_left[1]->restrictUser(d_right))
    spatialPromotion=true;
  if (d_left[0]->restrictUser(calc::FieldType(d_op0.vs(),ST_SPATIAL)))
    spatialPromotion=true;
  return spatialPromotion;
}

void calc::DoubleAssignment::prepareExecution()
{
  d_right->prepareExecution();
  d_left[0]->prepareExecution();
  d_left[1]->prepareExecution();
}

void calc::DoubleAssignment::run()
{
  calc::FieldStack stack;

  VS resultVs[2];
  resultVs[0] = d_left[0]->vs();
  resultVs[1] = d_left[1]->vs();

  const calc::Operator& op = calc::major2op(dassImplementor(d_op1.opCode()));
  OPERATION_TIMER(op.name(),true);
  d_right->executeDoubleAss(op,stack,resultVs);
  d_left[1]->assign(stack.popDest(d_left[1]->vs()));
  d_left[0]->assign(stack.popDest(d_left[0]->vs()));
}

void calc::DoubleAssignment::print(calc::InfoScript& i)const
{
  d_left[0]->print(i);
  i.stream() << " , ";
  d_left[1]->print(i);
  i.stream() << " = ";
  d_right->print(i);
}
