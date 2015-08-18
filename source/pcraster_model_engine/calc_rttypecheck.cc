#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_RTTYPECHECK
#include "calc_rttypecheck.h"
#define INCLUDED_CALC_RTTYPECHECK
#endif

// Library headers.
#ifndef INCLUDED_DEQUE
#include <deque>
#define INCLUDED_DEQUE
#endif
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_CFGCREATOR
#include "calc_cfgcreator.h"
#define INCLUDED_CALC_CFGCREATOR
#endif
#ifndef INCLUDED_CALC_ASTNODEVECTOR
#include "calc_astnodevector.h"
#define INCLUDED_CALC_ASTNODEVECTOR
#endif
#ifndef INCLUDED_CALC_BUILDTYPESVISITOR
#include "calc_buildtypesvisitor.h"
#define INCLUDED_CALC_BUILDTYPESVISITOR
#endif
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_DATASTORAGEID
#include "calc_datastorageid.h"
#define INCLUDED_CALC_DATASTORAGEID
#endif
#ifndef INCLUDED_CALC_POSITIONNAME
#include "calc_positionname.h"
#define INCLUDED_CALC_POSITIONNAME
#endif
#ifndef INCLUDED_CALC_ASTEXPR
#include "calc_astexpr.h"
#define INCLUDED_CALC_ASTEXPR
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif


/*!
  \file
  This file contains the implementation of the RtTypeCheck class.
*/



//------------------------------------------------------------------------------


namespace calc {

class RtTypeCheckPrivate :
  public boost::noncopyable
{
public:
  const Operator&              d_op;
  RunTimeEnv&                  d_rte;
  size_t                       d_nrActualArgs;
  ASTSymbolTable               d_syms;
  std::deque<const DataValue *>d_args;

  RtTypeCheckPrivate(
    const Operator&  op,
    RunTimeEnv&      rte,
    size_t           nrActualArgs):
     d_op(op),d_rte(rte),d_nrActualArgs(nrActualArgs),
     // fill with already known symbols
     d_syms(d_rte.dataTable().symbols())
  {
  }

  void check()
  {
    // build AST
    PositionName pn("checkAndExec");

    ASTExpr e(&pn,d_op);
    std::auto_ptr<ASTNodeVector> av(new ASTNodeVector());

    for(size_t r=0; r < d_nrActualArgs; ++r) {
      // reverser order popping
      size_t argNr=d_nrActualArgs-r-1;
      DataValue *argI =d_rte.popDataValue();
      switch(argI->ovs()) {
        case VS_STRING: {
           DataStorageId *ds=dynamic_cast<DataStorageId *>(argI);
           if (d_syms.contains(ds->id())) {
             // d_rte already contains a value with this name
             d_args.push_front(d_rte.dataTable()[ds->id()]);
             deleteFromPcrme(ds);
           } else {
             // keep the DataStorageId and resolve it after
             // the BuildTypesVisitor pass
             d_args.push_front(argI);
           }
           av->transferPushFront(new ASTPar(ds->id()));
        } break;
        default:
         if (isField(argI->ovs())) {
           // put back
           d_rte.pushDataValue(argI);
           // popDataValue ensures field is read,
           // StackedValue has Field ovs but is NOT a Field
           d_args.push_front(d_rte.popField());
           av->transferPushFront(createFieldArg(d_args.front(),argNr));
         } else {
           // other DataValues
           d_args.push_front(argI);
           av->transferPushFront(createOtherArg(d_args.front(),argNr));
         }
     }
    }
    e.transferFunctionArgs(av.release());

    ScopedCFG cfg(&e);
    // eigenlijk while closure
    BuildTypesVisitor btv(cfg.cfg);
    btv.init(d_syms);
    btv.visit();

    // check restriction to one result type
    for (size_t i=0; i < e.nrReturns(); ++i) {
      VS vs = e.returnDataType(i).vs();
      checkOneVs(vs,"a data type ");
    }

    // transfer arg types set on e in BuildTypesVisitor to stack d_args
    for(size_t i=0; i < d_nrActualArgs; ++i) {
      switch(d_args[i]->ovs()) {
       case VS_STRING: {
            const DataStorageId *ds=dynamic_cast<const DataStorageId *>(d_args[i]);
            // should now be resolved
            ASTSymbolInfo t(btv.table()[ds->id()]);
            // load will read data (maybe slow), must we defer it further?
            d_rte.load(t);
            d_args[i] = d_rte.dataTable()[ds->id()];
            deleteFromPcrme(ds);
            //  since d_rte.dataTable() owns Field data when loaded
            //  while below we assume ownership in d_args
            POSTCOND(!isField(t.ovs()));
            d_rte.pushDataValue(d_args[i]);
       } break;
         default:
         if (isField(d_args[i]->ovs())) {
           Field *f=asField(d_args[i]);
           f->resetVs(e.arg(i)->returnDataType().vs());
           d_rte.pushField(f);
         } else
           d_rte.pushDataValue(d_args[i]);
      }
      d_args[i]=0;
    }
  }

  void clean() {
    // delete what we own, if still around here
    for(size_t i=0; i < d_args.size(); ++i)
      deleteFromPcrme(d_args[i]);
    d_args.clear();
  }

  static ASTPar tmpPar(size_t i) {
   std::ostringstream name;
   // putting # in is essential, user can't do that
   // avoiding clash with id's of DataStorageId
   name << "arg#" << i+1;
   return ASTPar(name.str());
  }

  ASTPar* createFieldArg(const DataValue *fDv,size_t i) {
   ASTPar p(tmpPar(i));
   Field  *f=asField(fDv);
   p.returnDataType()=f->type();
   if (f->isSpatial() && f->nrValues()!=d_rte.rasterSpace().nrCells())
       p.symError("Number of cells is different than clone or previous argument");
   return new ASTPar(p);
  }

  ASTPar* createOtherArg(const DataValue *dv,size_t i) {
   ASTPar p(tmpPar(i));
   p.returnDataType()=DataType(dv->ovs(),ST_NON);
   return new ASTPar(p);
  }

  static Field* asField(const DataValue *dv) {
    Field *f=dynamic_cast<Field *>(const_cast<DataValue *>(dv));
    POSTCOND(f);
    return f;
  }

  ~RtTypeCheckPrivate()
  {
    clean();
  }

  };

} // namespace calc



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RTTYPECHECK MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RTTYPECHECK MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


/*!
 * type check and (pre-)load data in preparation of performing
 * operator \a op on \a nrActualArgs arguments that are present at the stack
 * of \a rte.
 * \todo
 *   min/max/cover with more than 2 arguments fails, pcrcalcpy catches this
 *   see void calc::RtTypeCheckTest::testBuildExpr()
 */
void calc::rtTypeCheck(
    const Operator&  op,
    RunTimeEnv*      rte,
    size_t           nrActualArgs)
{
  PRECOND(rte);
  RtTypeCheckPrivate rt(op,*rte, nrActualArgs);
  rt.check();
}
