#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_BUILDTYPESVISITOR
#include "calc_buildtypesvisitor.h"
#define INCLUDED_CALC_BUILDTYPESVISITOR
#endif

// Library headers.
#ifndef INCLUDED_STACK
#include <stack>
#define INCLUDED_STACK
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_REPEATUNTIL
#include "calc_repeatuntil.h"
#define INCLUDED_CALC_REPEATUNTIL
#endif
#ifndef INCLUDED_CALC_NONASSEXPR
#include "calc_nonassexpr.h"
#define INCLUDED_CALC_NONASSEXPR
#endif
#ifndef INCLUDED_CALC_BASEEXPR
#include "calc_baseexpr.h"
#define INCLUDED_CALC_BASEEXPR
#endif
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif
#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_CFGCREATOR
#include "calc_cfgcreator.h" // setKeyTypes only
#define INCLUDED_CALC_CFGCREATOR
#endif
#ifndef INCLUDED_CALC_ASTSTAT
#include "calc_aststat.h"
#define INCLUDED_CALC_ASTSTAT
#endif
#ifndef INCLUDED_CALC_DATATYPECLASH
#include "calc_datatypeclash.h"
#define INCLUDED_CALC_DATATYPECLASH
#endif

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

#ifndef INCLUDED_CALC_POSITION
#include "calc_position.h"
#define INCLUDED_CALC_POSITION
#endif

/*!
  \file
  This file contains the implementation of the BuildTypesVisitor class.
*/




//------------------------------------------------------------------------------

/*
namespace calc {

class BuildTypesVisitorPrivate
{
public:

  BuildTypesVisitorPrivate()
  {
  }

  ~BuildTypesVisitorPrivate()
  {
  }

};

} // namespace calc
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BUILDTYPESVISITOR MEMBERS
//------------------------------------------------------------------------------

namespace calc {

static std::string argSTError(
  std::string const& name,
  STClash const&  s,
  size_t nr)
{
  std::string legal, illegal;

  /* SYNTAX_OPER never have a strict spatial type
     and table-types are already catched by the vs check
     so the only conflict possible is nonspatial vs. spatial
     in functions
   */
   if (s.spatialFound()) {
     illegal = "spatial"; legal = "non-spatial";
   } else {
     legal = "spatial"; illegal = "non-spatial"; 
   }
  std::ostringstream msg;
  msg <<  "argument nr. "<<nr+1;
  msg <<" of function '"<<name<<"' is "<<illegal<<" only "<<legal<<" allowed";
  return msg.str();
}

static std::string vsError(
  const std::string&    offender,
  const calc::VSClash&  v)
{
  std::ostringstream msg;
    /* CW a position range in the script would be nice, now
     * just the operator itself
     * pcrcalc/test10c
     */
  // pcrcalc/test10[bc]
  msg << offender <<": type is ";
  msg << v.isOneOf() << ", legal type is "<< v.mustBeOneOf();
  return msg.str();
}

static std::string argVSError(
  const calc::Operator& o,
  const calc::VSClash&  v,
  int nr)
{
  return vsError(o.strInput(nr),v);
}

static std::string argCombError(
  const calc::Operator& o,
  const calc::VSClash&  v,
  int nr)
{
  std::ostringstream msg;
  msg << o.strInput(nr) << ": type is " << v.isOneOf() <<", while ";
  if (o.syntax() == "function") {
     int prev = (nr > 1) ? -1 : 0;
     // prev forcing a type or
     // -1 if the comb of arg 0 to nr-1
     // forced an illegal type for arg nr
     if (prev == -1) {
      // pcrcalc/test259a
      msg << "previous argument types are ";
     }
     else {
      // pcrcalc/test259
      msg << "type of argument nr. "<< (prev+1) <<" is ";
     }
  }  else {
    // pcrcalc/test260
     PRECOND(nr == 1);
     msg << "type of left argument is ";
  }
  msg << v.mustBeOneOf();
  return msg.str();
}

std::string assError(
  const calc::VSClash&  v,
  const calc::ASTSymbolInfo& p)
{
  return p.vsClashError(v,"redefined");
}
static std::string useError(
  const calc::VSClash&  v)
{
  std::ostringstream msg;
   msg << "used as " <<v.mustBeOneOf()<<
     " type, but has "<<v.isOneOf()<<" type";
  return msg.str();
}


//! travels an expr top down to push type info down
/*!
 * this is only needed:
 * 1) to push type info down from
 *    operations that are polymorphic, if, eq, see
 *    calc::BuildTypesVisitorTest::testTopDownExprRestrictor() for more examples
 * 2) to sub type certain DataType's: e.g. subTypeDataStorage();
 *
 * \bug
 *    DOEN DUS!
 *    Poly arg stuff stinks (see bool calc::Operator::isPolyInput(size_t i) const)
 *    maak duidelijk onderscheiding  in kenmerken:
 *    - resultaat gelijk aan arg x
 *    - arg x en Y zijn gelijk
 *    - etc.
 *    maak dat explicitiet in xml b.v. ResultType en ResulTypes i.p.v Result
 *     met attributes voor bovenstaande kenmerken.
 */
class TopDownExprRestrictor: public ASTVisitor {

  ASTSymbolTable&         d_table;
  std::stack<DataType>    d_dt;
  DataTypeChanger         d_dtc;

  DataType               pop() {
    PRECOND(d_dt.size());
    DataType f=d_dt.top();
    d_dt.pop();
    return f;
  }
 public:
  /*!
   * \param table table that will be updated
   * \param r     Field type to assign/restrict
   */
  TopDownExprRestrictor(
      ASTSymbolTable& table,
      const DataType& r):
     d_table(table)
    {
     d_dt.push(r);
    };

  ~TopDownExprRestrictor() {};

  //! update use of number
  /*!
   *  should solve pcrcalc214c: put scalar here in a 2nd run of
   *    BuildTypes
   */
  void visitNumber(ASTNumber *n) {
    d_dtc.restrict(n->returnDataType(0),pop());
  }

  //! update symbol table
  void visitPar (ASTPar *p) {
    d_dtc.restrict(d_table[p].dataType(),pop());
    d_dtc.update(p->returnDataType(0), d_table[p].dataType());
  }

  size_t nrChanges() const
  { return d_dtc.nrChanges(); }

  void visitExpr(BaseExpr    *e) {

   const Operator& op(e->op());
   // type description
   DataType       dtOp(op.resultType());
   // actual type of this expr:
   DataType       dtExpr(pop());

   dtExpr.restrict(dtOp);

   // push down
   if ( !dtOp.singleVs() && /* WHY THIS CLAUSE?: */!op.firstFieldInput()) {
    // push type down in poly types
    for(size_t i = op.firstPolyInput();
        i < e->nrArgs() && op.isPolyInput(i); ++i) {
      DataType ftArg(op.argType(i));
      ftArg.restrict(dtExpr.vs());
      d_dt.push(ftArg);
      e->arg(i)->accept(*this);
    }
   }

   // set this type
   d_dtc.restrict(e->returnDataType(0),dtExpr);

   // if 1st arg is not a field it is a DataStorage
   if (op.firstFieldInput()) {
      POSTCOND(op.firstFieldInput()==1);
      ASTPar *p=dynamic_cast<ASTPar *>(e->arg(0));
      if (!p) { // can happen if arg is incorrect for operation
        return;
      }
      try {
       subTypeDataStorage(p,e,op,dtExpr);
      } catch (const TableClash& c) {
         p->symError(c.message());
      } catch (const MapStackClash& c) {
         p->symError(c.message());
      } catch (const VSClash& c) {
         p->symError(argVSError(op,c,0));
      }
    }

   }

  //! should never be visited only expressions are visited here
  void visitAss(ASTAss   *)
  { PRECOND(FALSE); };
  //! should never be visited only expressions are visited here
  void visitStat(ASTStat *)
  { PRECOND(FALSE); };

  void subTypeDataStorage(ASTPar *p,
       const BaseExpr*  e,
       const Operator& op,
       const DataType& dtExpr)
  {
    POSTCOND(d_table.contains(p));

    OVS ovs(op.argType(0).vs());
    // restiction on data type of Arg0
    DataType reqArg0(ovs);

    switch(ovs) {
      case VS_TSS:
         // all timeinput... funcs are typed
         PRECOND(dtExpr.vs() == op.vs());
         // fall through
      case VS_MAPSTACK: {
         reqArg0.setResultType(ovs,dtExpr.vs());
         MapStackType mst(reqArg0.mapStackType());
         switch(op.opCode()) {
           case OP_TIMEINPUT      : mst.setUse(MapStackType::Full); break;
           case OP_TIMEINPUTSPARSE: mst.setUse(MapStackType::Sparse); break;
           case OP_LOOKUPMAPSTACK : mst.setUse(MapStackType::Lookup); break;
           case OP_TIMEINPUTMODULO: {
                    mst.setUse(MapStackType::Modulo);
                    PRECOND(e->nrArgs() == 2);
                    ASTNumber *n= dynamic_cast<ASTNumber *>(e->arg(1));
                    // typecheck already forces n to be ordinal integer
                    if (!n || n->value() < 1)
                      e->arg(1)->posError("highestTimestepAvailable argument of "
                                          "timeinputmodulo must be an integer > 0");
                    mst.setHighestTimestepAvailable(static_cast<size_t>(n->value()));
                    break;
                   }
           default:;
         }
         reqArg0.setMapStackType(mst);
         }
         break;
      case VS_TABLE:
       {
         std::vector<VS> tableColTypes;
         switch(op.opCode()) {
          case OP_DYNWAVE_MRF:
          case OP_DYNWAVESTATE:
          case OP_DYNWAVEFLUX:
          case OP_LOOKUPSTATE:
          case OP_LOOKUPPOTENTIAL:
            tableColTypes=std::vector<VS>(4,VS_S);
            tableColTypes[0]=VS_NO;
            break;
          default: // lookup.... funcs
           for(size_t k=1; k < e->nrArgs(); ++k) {
             ScopedCFG c(e->arg(k));
             BuildTypesVisitor btv(c.cfg);
             // TODO very inefficient copying a
             // full symbol table, btv should also
             // accept a reference to instead of owning
             // a copy, this copy is not assumed to be altered
             btv.init(d_table);
             btv.visit();
             DataType dt=e->arg(k)->returnDataType();
             tableColTypes.push_back(dt.vs());
           }
           // all lookup.... funcs are strictly typed
           PRECOND(dtExpr.vs() == op.vs());
           tableColTypes.push_back(dtExpr.vs());
         }
         reqArg0.setTableColTypes(tableColTypes);
       }
       break;
      case VS_INDEXTABLE:
         {
           PRECOND(false); // get ArrayDefVector from p
         }
        break;
       default:
        POSTCOND(false); // never reached
     }
    // arg info to update
    d_dtc.restrict(d_table[p].dataType(),reqArg0);
   }
}; // TopDownExprRestrictor

}

//------------------------------------------------------------------------------
// DEFINITION OF BUILDTYPESVISITOR MEMBERS
//------------------------------------------------------------------------------

//! ctor, results in a state where any type of field can be accepted and empty table
calc::BuildTypesVisitor::BuildTypesVisitor(CFGNode *cfg):
  CFGVisitor(cfg),
  d_containsDynamicSection(false),
  d_hasStatementWithReportKeyword(false)
{
}

calc::BuildTypesVisitor::~BuildTypesVisitor()
{
}

//! re-initialize table
/*!
 * \param table  table info to initialize the internal table
 */
void calc::BuildTypesVisitor::init(const ASTSymbolTable& table)
{
  d_table=table;
}

void calc::BuildTypesVisitor::checkOnTimeinput(BaseExpr   *o)
{
  // proceed only when first arg is VS_MAPSTACK
  if (!o->nrArgs() || o->op().argType(0).vs() != VS_MAPSTACK)
    return;

  DataType eResult=o->op().computeResultType(o->dataTypeArgs(),0);

  // timeinput(sparse|modulo)
  ASTPar *ms=dynamic_cast<ASTPar *>(o->arg(0));
  if (ms) {
      POSTCOND(d_table.contains(ms));
      DataType req(d_table[ms].dataType().resultType(),ST_SPATIAL);
      eResult.restrict(req);
      d_dtc.restrict(o->returnDataType(),eResult);
  }
}

#ifndef INCLUDED_CALC_LINKINEXPR
#include "calc_linkinexpr.h"
#define INCLUDED_CALC_LINKINEXPR
#endif

/*!
 * \todo
 *  also do like updateDynamicOperation for index? operations only in binding
 * \todo
 *  most of the body should be refactored into calc::Operator,
 *   then EXEC_* enums can dissapear into OpImpl
 */
void calc::BuildTypesVisitor::visitExpr(BaseExpr*        o)
{
  LinkInExpr *lie=dynamic_cast<LinkInExpr *>(o);
  if (lie) {

   std::string libraryName;
   if (lie->isConstructor()) {
     // constructor type already determined at parsing
     try {
      d_table[lie->objectPar()].setObjectLinkConstructor(lie);
      } catch (const VSClash& v) {
        // pcrcalc548
        lie->objectPar()->posError(vsError(lie->objectPar()->name(),v));
      }
      libraryName=lie->nameBefore();
   } else {
     // function or method
     if (d_table.contains(lie->nameBefore())) {
       // method call

       // check if it IS an object
       try {
         DataType dt(d_table[lie->nameBefore()].vs());
         dt.restrict(VS_OBJECT);
       } catch (const VSClash& v) {
          // pcrcalc548
          lie->posError(vsError(lie->nameBefore(),v));
       }

       LinkInExpr* ctor= d_table[lie->nameBefore()].objectLinkConstructor();
       PRECOND(ctor);
       lie->setAsMethod(ctor->nameAfter());
       libraryName=ctor->nameBefore();
     } else {
       lie->setAsFunction();
       libraryName=lie->nameBefore();
     }
   }

   try {
     lie->loadLibrary(d_table.linkInLibrary(libraryName));
    } catch(ASTSymbolTable::LinkInLibraryException const& e) {
      lie->posError(e.message);
    }
    lie->check();
  }

  const Operator& op(o->op());

  if (op.opCode() == OP_TIMEOUTPUT && o->nrArgs() >= 2)
    d_outputTssVs = o->arg(1)->returnDataType().vs();

  // TEST CORRECT NR OF ARGUMENTS
  // pcrcalc/test25[23]
  //pcrcalc/test14
  std::string msg(op.checkNrInputs(o->nrArgs()));
  if (!msg.empty()) {
     if (o->nrArgs())
      o->arg(0)->posError(msg);
     else // pcrcalc/test252a
      o->posError(msg);
  }

  // CHECK IF ARGUMENTS ARE VALID FOR OPERATION
  size_t i; // outside, catch clause uses it
  try {
   for(i=0; i < o->nrArgs(); ++i)
   {
     DataType dt(op.argType(i));
     d_dtc.restrict(o->arg(i)->returnDataType(),dt);

     if (i > op.polyEqualityInputBegin()) {
       // this next arg must have equal value scale then prev
       d_dtc.restrict(o->arg(i)->returnDataType(),
                      o->arg(i-1)->returnDataType().vs());
      // bubble back
      d_dtc.restrict(o->arg(i-1)->returnDataType(),
                     o->arg(i)->returnDataType().vs());
     }

     TopDownExprRestrictor trArg(d_table,o->arg(i)->returnDataType());
     o->arg(i)->accept(trArg);
     d_dtc.incr(trArg.nrChanges());
   }
  } catch(const STClash& s) {
     // pcrcalc258
     o->posError(argSTError(op.name(),s,i));
  } catch(const VSClash& v) {
    // if allowed by the op-arg then it
    // is a combination error
    DataType dt(op.argType(i));
    if (intersect(v.isOneOf(),dt.vs()) != VS_UNKNOWN)
      o->arg(i)->posError(argCombError(op,v,i));
    // todo
    // if the argument is use ASTPar then like test pcrcalc501
    o->posError(argVSError(op,v,i));
  }

  for (size_t i=0; i < o->nrReturns(); ++i) {
   DataType eResult=o->op().computeResultType(o->dataTypeArgs(),i);
   eResult.setAllowPromotion(true);
   checkOnTimeinput(o);
   d_dtc.restrict(o->returnDataType(i),eResult);

   if (i == 0) {
    TopDownExprRestrictor tr(d_table,o->returnDataType(0));
    o->accept(tr);
    d_dtc.incr(tr.nrChanges());
   }
  }

}

//! evaluate   p=expr
void calc::BuildTypesVisitor::singleAss (
    ASTPar          *p,
    const DataType&  dtExpr)
{
  if (!d_table.contains(p)) {
   // insert new info, dtExpr is expected type
   // and p is created the ass visited, since not
   // yet present in the table
   d_table[p].dataType()=dtExpr;
   d_table[p].setFirstIsCreation();


   d_dtc.incr();

  } else {
    // first the non field returns, with more type specifics
    switch(dtExpr.vs()) {
      case VS_TSS: {
       DataType dt;
       dt.setResultType(VS_TSS, d_outputTssVs);
       d_dtc.restrict(d_table[p].dataType(),dt);
      } break;
      default: ;
    }

    try {
     // combine types with possible assigment spatial-cast
     // this happens if parameter is multiple times assigned pcrcalc2

     DataType r(dtExpr);

     DataType req(d_table[p].dataType());
     req.setAllowPromotion(true);
     r.restrict(req);
     d_dtc.update(p->returnDataType(0),r);
     d_dtc.update(d_table[p].dataType(),r);

    } catch(const VSClash& v) {
       p->symError(assError(v,d_table[p]));
    } catch(const STClash& ) {
       p->symError("bad spatial/nonspatial assignment");
       //  msg = quote(userName())+" is defined as "+s.d_oldSt+" value on "
       // +shortPosText()+" and assigned here a "+s.d_newSt+" value";
    }
  }
  // TODO UseDefAnalyzer is a better place for getting this info
  d_table[p].setFirstAss(p);
}

/*!
 * \todo
 *   true L-grammar eval should test on forehand, now the value
 *   could generate an error first. That is: first visit par at
 *   left hand and then visit right hand, now it is other way around.
 *
 * \todo
 *   add a test for a STClash
 * \todo
 *   TopDownExprRestrictor to push lhs type to rhs only for a normal
 *   expr, how for a multiple return expr?
 */
void calc::BuildTypesVisitor::visitAss (ASTAss    *a)
{
  // test/pcrcalc101
  if (a->nrPars() != a->rhs()->nrReturns()) {
       a->posError("function on right side of = does not return"
                   " the number of parameters defined on left of =");
  }


  for(size_t p=0; p<a->nrPars(); ++p)
      singleAss(a->par(p),a->rhs()->returnDataType(p));

  // FTTB no multiple return
  if (a->rhs()->nrReturns()==1 && a->rhs()->returnDataType(0).vs()!=VS_OBJECT) {
    DataType dt(a->par(0)->returnDataType().vs());
    TopDownExprRestrictor t(d_table,dt);
    a->rhs()->accept(t);
    d_dtc.incr(t.nrChanges());
  }

}

void calc::BuildTypesVisitor::visitStat(ASTStat   *s)
{
  if (s->reportParsed())
   d_hasStatementWithReportKeyword=true;
}

void calc::BuildTypesVisitor::visitPar(ASTPar *p)
{
 try {
  DataType r(p->returnDataType(0));
  r.setAllowPromotion(true);

  DataType req(d_table[p].dataType());
  req.restrict(r);
  d_dtc.update(p->returnDataType(0),req);
  d_dtc.update(d_table[p].dataType(),req);

  // RF1 ??  d_table[p].setIoType(ioType(p->name()));
 } catch(const VSClash& v) {
    p->symError(useError(v));
 }
}

void calc::BuildTypesVisitor::visitNumber(ASTNumber *)
{
}

void calc::BuildTypesVisitor::enterDynamicSection(DynamicSection *)
{
  d_containsDynamicSection=true;
}

void calc::BuildTypesVisitor::visitNonAssExpr(NonAssExpr   *e)
{
  d_dtc.update(e->returnDataType(),e->expr()->returnDataType());
}

void calc::BuildTypesVisitor::jumpOutRepeatUntil(RepeatUntil *r)
{
  try {
    d_dtc.restrict(r->condition()->returnDataType(),VS_B);
    TopDownExprRestrictor c(d_table,r->condition()->returnDataType());
    r->condition()->accept(c);
    d_dtc.incr(c.nrChanges());
  } catch (const VSClash& v) {
    r->condition()->posError(vsError("until condition",v));
  }
}

const calc::ASTSymbolTable& calc::BuildTypesVisitor::table() const
{
  return d_table;
}

size_t calc::BuildTypesVisitor::nrChanges() const
{
  return d_dtc.nrChanges();
}

bool calc::BuildTypesVisitor::containsDynamicSection() const
{
  return d_containsDynamicSection;
}

//! get value of d_hasStatementWithReportKeyword
bool calc::BuildTypesVisitor::hasStatementWithReportKeyword() const
{
  return d_hasStatementWithReportKeyword;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

