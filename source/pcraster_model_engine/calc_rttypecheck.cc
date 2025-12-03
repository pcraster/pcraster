#include "stddefx.h"
#include "calc_rttypecheck.h"
#include "calc_cfgcreator.h"
#include "calc_astnodevector.h"
#include "calc_buildtypesvisitor.h"
#include "calc_vs.h"
#include "calc_field.h"
#include "calc_operator.h"
#include "calc_runtimeenv.h"
#include "calc_datastorageid.h"
#include "calc_positionname.h"
#include "calc_astexpr.h"
#include "calc_astpar.h"
#include "geo_rasterspace.h"

#include <deque>

/*!
  \file
  This file contains the implementation of the RtTypeCheck class.
*/


//------------------------------------------------------------------------------


namespace calc
{

class RtTypeCheckPrivate
{
public:
  const Operator &d_op;
  RunTimeEnv &d_rte;
  size_t d_nrActualArgs;
  ASTSymbolTable d_syms;
  std::deque<const DataValue *> d_args;

  RtTypeCheckPrivate(const Operator &op, RunTimeEnv &rte, size_t nrActualArgs)
      : d_op(op), d_rte(rte), d_nrActualArgs(nrActualArgs),
        // fill with already known symbols
        d_syms(d_rte.dataTable().symbols())
  {
  }

  RtTypeCheckPrivate(const RtTypeCheckPrivate &other) = delete;

  RtTypeCheckPrivate &operator=(const RtTypeCheckPrivate &other) = delete;

  void check()
  {
    // build AST
    PositionName pn("checkAndExec");

    ASTExpr e(&pn, d_op);
    std::unique_ptr<ASTNodeVector> av(new ASTNodeVector());

    for (size_t r = 0; r < d_nrActualArgs; ++r) {
      // reverser order popping
      size_t const argNr = d_nrActualArgs - r - 1;
      DataValue *argI = d_rte.popDataValue();
      switch (argI->ovs()) {
        case VS_STRING: {
          auto *ds = dynamic_cast<DataStorageId *>(argI);
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
            av->transferPushFront(createFieldArg(d_args.front(), argNr));
          } else {
            // other DataValues
            d_args.push_front(argI);
            av->transferPushFront(createOtherArg(d_args.front(), argNr));
          }
      }
    }
    e.transferFunctionArgs(av.release());

    ScopedCFG const cfg(&e);
    // eigenlijk while closure
    BuildTypesVisitor btv(cfg.cfg);
    btv.init(d_syms);
    btv.visit();

    // check restriction to one result type
    for (size_t i = 0; i < e.nrReturns(); ++i) {
      VS const vs = e.returnDataType(i).vs();
      checkOneVs(vs, "a data type ");
    }

    // transfer arg types set on e in BuildTypesVisitor to stack d_args
    for (size_t i = 0; i < d_nrActualArgs; ++i) {
      switch (d_args[i]->ovs()) {
        case VS_STRING: {
          const auto *ds = dynamic_cast<const DataStorageId *>(d_args[i]);
          // should now be resolved
          ASTSymbolInfo const t(btv.table()[ds->id()]);
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
            Field *f = asField(d_args[i]);
            f->resetVs(e.arg(i)->returnDataType().vs());
            d_rte.pushField(f);
          } else {
            d_rte.pushDataValue(d_args[i]);
          }
      }
      d_args[i] = nullptr;
    }
  }

  void clean()
  {
    // delete what we own, if still around here
    for (auto &d_arg : d_args) {
      deleteFromPcrme(d_arg);
    }
    d_args.clear();
  }

  static ASTPar tmpPar(size_t i)
  {
    std::ostringstream name;
    // putting # in is essential, user can't do that
    // avoiding clash with id's of DataStorageId
    name << "arg#" << i + 1;
    return ASTPar(name.str());
  }

  ASTPar *createFieldArg(const DataValue *fDv, size_t i)
  {
    ASTPar p(tmpPar(i));
    Field *f = asField(fDv);
    p.returnDataType() = f->type();
    if (f->isSpatial() && f->nrValues() != d_rte.rasterSpace().nrCells()) {
      p.symError("Number of cells is different than clone or previous argument");
    }
    return new ASTPar(p);
  }

  ASTPar *createOtherArg(const DataValue *dv, size_t i)
  {
    ASTPar p(tmpPar(i));
    p.returnDataType() = DataType(dv->ovs(), ST_NON);
    return new ASTPar(p);
  }

  static Field *asField(const DataValue *dv)
  {
    auto *f = dynamic_cast<Field *>(const_cast<DataValue *>(dv));
    POSTCOND(f);
    return f;
  }

  ~RtTypeCheckPrivate()
  {
    clean();
  }
};

}  // namespace calc

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
void calc::rtTypeCheck(const Operator &op, RunTimeEnv *rte, size_t nrActualArgs)
{
  PRECOND(rte);
  RtTypeCheckPrivate rt(op, *rte, nrActualArgs);
  rt.check();
}
