#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_OPIMPL
#include "calc_opimpl.h"
#define INCLUDED_CALC_OPIMPL
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SCOPED_PTR
#include <boost/scoped_ptr.hpp>
#define INCLUDED_BOOST_SCOPED_PTR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_CR
#include "calc_cr.h"
#define INCLUDED_CALC_CR
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif
#ifndef INCLUDED_CALC_VSPATIAL
#include "calc_vspatial.h"
#define INCLUDED_CALC_VSPATIAL
#endif
#ifndef INCLUDED_CALC_AREAOPERATIONS
#include "calc_areaoperations.h"
#define INCLUDED_CALC_AREAOPERATIONS
#endif
#ifndef INCLUDED_CALC_ORDEROPERATIONS
#include "calc_orderoperations.h"
#define INCLUDED_CALC_ORDEROPERATIONS
#endif
#ifndef INCLUDED_CALC_ARGORDER
#include "calc_argorder.h"
#define INCLUDED_CALC_ARGORDER
#endif

/*!
  \file
  This file contains the implementation of the IOpImpl class.
*/

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_EXECARGUMENTS
#include "calc_execarguments.h"
#define INCLUDED_CALC_EXECARGUMENTS
#endif
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOPIMPL MEMBERS
//------------------------------------------------------------------------------
namespace calc {

AreaTotal                 builtIn_areatotal;
AreaAverage               builtIn_areaaverage;
AreaMinimum               builtIn_areaminimum;
AreaMaximum               builtIn_areamaximum;
Order                     builtIn_order;
AreaOrder                 builtIn_areaorder;
ArgOrder                  builtIn_argorder;
ArgOrderWithId            builtIn_argorderwithid;
ArgOrderAreaLimited       builtIn_argorderarealimited;
ArgOrderWithIdAreaLimited builtIn_argorderwithidarealimited;
ArgOrderAddAreaLimited       builtIn_argorderaddarealimited;
ArgOrderWithIdAddAreaLimited builtIn_argorderwithidaddarealimited;

  template<class V, class I>
    static void initOp(V& fArray, const I *f) {
      if (f)
        fArray[f->cri()]=f;
    }


  struct BinArg {
   typedef enum LR { Left=0, Right=1} LR;
   typedef enum T { SS_NN=0, NS=1, SN=2 } T;
   T       t;
   size_t  n; // max array size
   BinArg(const ExecArguments& a,size_t left=Left,size_t right=Right)
   {
      n=std::max<>(a[left].nrValues(),a[right].nrValues());
      t=SS_NN;
      if (a[left].isSpatial() != a[right].isSpatial()) {
        t = a[left].isSpatial() ? SN : NS;
      }
   }
  };

}


//------------------------------------------------------------------------------
// DEFINITION OF IOPIMPL MEMBERS
//------------------------------------------------------------------------------

calc::SameUn::SameUn(const ISameUn* fieldOp):
 d_fieldOp(fieldOp)
{}

calc::SameUn::~SameUn()
{}

void calc::SameUn::exec(RunTimeEnv* rte,const Operator& op, size_t nrArgs) const
{
  PRECOND(nrArgs==1);
  ExecArguments   a(op,rte,nrArgs);
  d_fieldOp->f(a.srcDest(0),a[0].nrValues());
  a.pushResults();
}

void calc::SameUn::genPointCode      (PointCodeGenerator* /*g*/) const
{}

calc::SameBin::SameBin( const ISameBin* op1,
                        const ISameBin* op2,
                        const ISameBin* op3):
  d_fieldOp(3)
{
 initOp(d_fieldOp,op1);
 initOp(d_fieldOp,op2);
 initOp(d_fieldOp,op3);
}

calc::SameBin::~SameBin()
{}

void calc::SameBin::exec              (RunTimeEnv* rte,const Operator& op,size_t DEBUG_ARG(nrArgs)) const
{
  PRECOND(nrArgs==2);
  typedef BinArg B;
  ExecArguments   a(op,rte,2);
  B             b(a);

  POSTCOND(a[B::Left].cri() == a[B::Right].cri()); // same binary operands

  CRIndex i = a[B::Left].cri();
  POSTCOND(d_fieldOp[i]);
  switch(b.t) {
    case B::SS_NN: {
        size_t left =B::Left;
        size_t right=B::Right;
        // optimize a redundant field copy
        if (op.commutative() && a[left].readOnlyReference())
          std::swap(left,right);
        d_fieldOp[i]->ss(a.srcDest(left),a[right].src(),b.n);break;
      }
    case B::SN:
      // if (op.opCode() == OP_POW) // TODO optimize 0.5,1,2,3,4,etc.
      //    std::cout << "pow exp " << *a[B::Right].src_f()  << std::endl;
      d_fieldOp[i]->sn(a.srcDest(B::Left),a[B::Right].src(),b.n);
      break;
    case B::NS:
      d_fieldOp[i]->ns(a[B::Left].src(),a.srcDest(B::Right),b.n);
      break;
  }

  a.pushResults();
}

void calc::SameBin::genPointCode      (PointCodeGenerator* /*g*/) const
{}

void calc::DiffBin::exec(RunTimeEnv* rte,const Operator& op,size_t DEBUG_ARG(nrArgs)) const
{
  PRECOND(nrArgs==2);
  typedef BinArg B;
  ExecArguments a(op,rte,2);
  B             b(a);

  UINT1 *r = static_cast<UINT1 *>(a.dest());
  CRIndex i = a[1].cri(); // selection on 2nd/right operand in case of IfThenArray
  POSTCOND(d_fieldOp[i]);
  switch(b.t) {
    case B::SS_NN:
      d_fieldOp[i]->ss(r,a[0].src(),a[1].src(),b.n);break;
    case B::SN:
      d_fieldOp[i]->sn(r,a[0].src(),a[1].src(),b.n);break;
    case B::NS:
      d_fieldOp[i]->ns(r,a[0].src(),a[1].src(),b.n); break;
  }

  a.pushResults();
}

calc::DiffBin::DiffBin(const IDiffBin* op1,
                       const IDiffBin* op2,
                       const IDiffBin* op3):
  d_fieldOp(3)
{
 initOp(d_fieldOp,op1);
 initOp(d_fieldOp,op2);
 initOp(d_fieldOp,op3);
}

calc::DiffBin::~DiffBin()
{}
void calc::DiffBin::genPointCode      (PointCodeGenerator* /*g*/) const
{}

void calc::IfThenElse::exec(RunTimeEnv* rte,const Operator& op,size_t DEBUG_ARG(nrArgs) ) const
{
  PRECOND(nrArgs==3);
  typedef BinArg B;
  ExecArguments a(op,rte,3);
  B             b(a,1,2);

  if (a[0].isSpatial()) {
    // compute field
    CRIndex i = a[1].cri(); // selection on true branch of IfThenElseArray
    // always spatial
    size_t n = std::max<>(a[0].nrValues(),b.n);
    POSTCOND(d_fieldOp[i]);
    switch(b.t) {
      case B::SS_NN:
       if (a[1].isSpatial())
        d_fieldOp[i]->ss(a.dest(),a[0].src_1(),a[1].src(),a[2].src(),n);
       else
        d_fieldOp[i]->nn(a.dest(),a[0].src_1(),a[1].src(),a[2].src(),n);
       break;
      case B::SN:
        d_fieldOp[i]->sn(a.dest(),a[0].src_1(),a[1].src(),a[2].src(),n);
        break;
      case B::NS:
        d_fieldOp[i]->ns(a.dest(),a[0].src_1(),a[1].src(),a[2].src(),n);
        break;
    }
    a.pushResults();
  } else {
    // condition is nonspatial: select an entire branch field as result
    PRECOND(!a[0].isMV());
    bool           cond=a[0].src_1()[0]==1;
    size_t resultBranch=cond ?  1 : 2;
    size_t  otherBranch=3-resultBranch;

    // set result to this argument
    (void)a.srcDest(resultBranch);
    bool cast=!a.result().isSpatial() && a[otherBranch].isSpatial();
    a.pushResults();
    if (cast)
      major2op(OP_SPATIAL)->exec(rte,1);
  }
}

calc::IfThenElse::IfThenElse(const IIfThenElse* op1,
                       const IIfThenElse* op2,
                       const IIfThenElse* op3):
  d_fieldOp(3)
{
 initOp(d_fieldOp,op1);
 initOp(d_fieldOp,op2);
 initOp(d_fieldOp,op3);
}

calc::IfThenElse::~IfThenElse()
{}
void calc::IfThenElse::genPointCode      (PointCodeGenerator* /*g*/) const
{}

calc::DiffUn::DiffUn(const IDiffUn* op1,
                     const IDiffUn* op2,
                     const IDiffUn* op3):
 d_fieldOp(3)
{
 initOp(d_fieldOp,op1);
 initOp(d_fieldOp,op2);
 initOp(d_fieldOp,op3);
}

calc::DiffUn::~DiffUn()
{}

void calc::DiffUn::exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  PRECOND(nrArgs==1);
  ExecArguments a(op,rte,nrArgs);
  Field& r(a.createResult());

  size_t n=std::max(r.nrValues(),a[0].nrValues());
  CRIndex i = a[0].cri();
  POSTCOND(d_fieldOp[i]);
  d_fieldOp[i]->f(r.dest(),a[0].src(),n);

  a.pushResults();
}

void calc::DiffUn::genPointCode      (PointCodeGenerator* /*g*/) const
{}

calc::SpatialImpl::SpatialImpl(const IDiffUn* op1,
                     const IDiffUn* op2,
                     const IDiffUn* op3):
 DiffUn(op1,op2,op3)
{
}

calc::SpatialImpl::~SpatialImpl()
{}

void calc::SpatialImpl::exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  Field *in=rte->popField();
  bool  nop=in->isSpatial();
  rte->pushField(in);
  if (!nop)
    DiffUn::exec(rte,op,nrArgs);
}

#ifndef INCLUDED_CALC_GENERATEFIELD
#include "calc_generatefield.h"
#define INCLUDED_CALC_GENERATEFIELD
#endif
calc::GenNonSpatial::GenNonSpatial()
{}
calc::GenNonSpatial::~GenNonSpatial()
{}
void calc::GenNonSpatial::exec          (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  PRECOND(nrArgs==0);
  ExecArguments a(op,rte,nrArgs);
  Field& r(a.createResult());

  GenerateNonSpatial gsf(rte->rasterSpace());
  switch(op.opCode()) {
    case OP_CELLLENGTH:
      gsf.celllength(r.dest_f()); break;
    case OP_CELLAREA:
      gsf.cellarea(r.dest_f()); break;
    case OP_MAPUNIFORM:
      gsf.mapuniform(r.dest_f()); break;
    case OP_MAPNORMAL:
      gsf.mapnormal(r.dest_f()); break;
    case OP_TIME:
     *(r.dest_f())=rte->timer().currentInt(); break;
    case OP_TIMESLICE:
     *(r.dest_f())=1; break;
    default: PRECOND(FALSE);
  }
  a.pushResults();
}

calc::GenSpatial::GenSpatial()
{}
calc::GenSpatial::~GenSpatial()
{}

/*!
 * \bug  the Operator::exec d_impl->setOp() fix makes it possible to
 *  have a single GenSpatial and NonSpatial class, if that is fixed
 *  this is not possible
 */
void calc::GenSpatial::exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  PRECOND(nrArgs==1);
  ExecArguments a(op,rte,nrArgs);
  Field& r(a.createResult());
  GenerateSpatial gsf(a[0],rte->spatialPacking(),rte->rasterSpace());

  switch(op.opCode()) {
    case OP_UNIQUEID:
      gsf.uniqueid(r.dest_f()); break;
    case OP_XCOORDINATE:
      gsf.xcoordinate(r.dest_f()); break;
    case OP_YCOORDINATE:
      gsf.ycoordinate(r.dest_f()); break;
    case OP_NORMAL:
      gsf.normal(r.dest_f()); break;
    case OP_UNIFORM:
      gsf.uniform(r.dest_f()); break;
    default: PRECOND(FALSE);
  }
  a.pushResults();
}

void calc::GenSpatial::genPointCode(PointCodeGenerator* /*g*/) const
{
}
void calc::GenNonSpatial::genPointCode(PointCodeGenerator* /*g*/) const
{
}

calc::Conversion::Conversion()
{}
calc::Conversion::~Conversion()
{}

#ifndef INCLUDED_MISC
#include "misc.h"  // BITSET
#define INCLUDED_MISC
#endif
void calc::Conversion::exec          (RunTimeEnv* rte,const Operator& op,size_t DEBUG_ARG(nrArgs)) const
{
  PRECOND(nrArgs==1);

  /*!return the proper value scale conversion based
   * on internal conversion matrix
   */
  struct ConvTable {
  MAJOR_CODE operator()(VS from, VS to)
  {
    const MAJOR_CODE convTable[6][6] = {
    // indexed by 2log valuescale
    // from:  |   to:
    //        |   VS_B 1      VS_N 4      VS_O 4      VS_S s      VS_D s     VS_L 1
    /* VS_B 1 */ {OP_NOP    , OP_C_1_2_N, OP_C_1_2_O, OP_C_1_2_S, OP_C_1_2_D ,OP_ILL     },
    /* VS_N 4 */ {OP_C_4_2_B, OP_NOP    , OP_NOP    , OP_C_4_2_S, OP_C_4_2_D ,OP_C_4_2_L },
    /* VS_O 4 */ {OP_C_4_2_B, OP_NOP    , OP_NOP    , OP_C_4_2_S, OP_C_4_2_D ,OP_C_4_2_L },
    /* VS_S s */ {OP_C_S_2_B, OP_C_S_2_N, OP_C_S_2_O, OP_NOP    , OP_C_S_2_D ,OP_C_S_2_L },
    /* VS_D s */ {OP_C_S_2_B, OP_C_D_2_N, OP_C_D_2_O, OP_C_D_2_S, OP_NOP     ,OP_C_D_2_L },
    /* VS_L 1 */ {OP_C_1_2_B, OP_C_1_2_N, OP_C_1_2_O, OP_C_1_2_S, OP_C_L_2_D ,OP_NOP   }};
    from = biggestVs(from);
    PRECOND(NRBITSET_TYPE(from,VS) == 1);
    PRECOND(NRBITSET_TYPE(to  ,VS) == 1);
    PRECOND(FIRSTBITSET_TYPE(from,VS) < 6);
    PRECOND(FIRSTBITSET_TYPE(to  ,VS) < 6);
    return convTable[FIRSTBITSET_TYPE(from,VS)][FIRSTBITSET_TYPE(to  ,VS)];
   }
  };

  Field *in=rte->popField();
  MAJOR_CODE doOp=ConvTable()(in->vs(),op.vs());
  POSTCOND(doOp!=OP_ILL);
  if (doOp != OP_NOP) {
    rte->pushField(in);
    major2op(doOp)->exec(rte,1);
  } else {
    // e.g VS_SD  -> VS_S
    //     VS_N  <-> VS_O
    if (in->vs() != op.vs()) {
     // in == out for OP_NOP
     in->resetVs(op.vs());
    }
    rte->pushField(in);
  }
}

void calc::Conversion::genPointCode(PointCodeGenerator* /*g*/) const
{
}

calc::Trig::Trig(const ISameUn* fieldOp):
 d_fieldOp(fieldOp)
{}

calc::Trig::~Trig()
{}

void calc::Trig::exec              (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  // need conversion?
  Field *in=rte->popField();
  if (in->vs() != VS_D) { // VS_S or VS_SD
    rte->pushField(in);
    major2op(OP_C_S_2_D)->exec(rte,1);
  } else
    rte->pushField(in);

  PRECOND(nrArgs==1);
  ExecArguments   a(op,rte,nrArgs);
  PRECOND(a[0].vs()==VS_D);
  d_fieldOp->f(a.srcDest(0),a[0].nrValues());

  a.pushResults();
}

void calc::Trig::genPointCode      (PointCodeGenerator* /*g*/) const
{
  // select on base of VS_S or VS_D
}

namespace calc {
  template <typename AreaStatisticalOperation>
   void areaStatisticalOperation(RunTimeEnv* rte,const Operator& op,size_t nrArgs)
  {
      ExecArguments args(op, rte, nrArgs);
      AreaStatisticalOperation ao;
      PRECOND(args[0].nrValues()==args[1].nrValues());
      REAL4 *r = (REAL4 *)args.srcDest(0);

      switch(args[1].cri()) {
        case CRI_1:
          ao.apply(r,args[1].src_1(),args[1].nrValues()); break;
        case CRI_4:
          ao.apply(r,args[1].src_4(),args[1].nrValues()); break;
        default : POSTCOND(false);
      }
      args.pushResults();
  }
}

void calc::AreaTotal::exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
   areaStatisticalOperation<AreaTotalOperation>(rte, op, nrArgs);
}

void calc::AreaAverage::exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
   areaStatisticalOperation<AreaAverageOperation>(rte, op, nrArgs);
}

void calc::AreaMinimum::exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
   areaStatisticalOperation<AreaMinimumOperation>(rte, op, nrArgs);
}

void calc::AreaMaximum::exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
   areaStatisticalOperation<AreaMaximumOperation>(rte, op, nrArgs);
}

void calc::Order::exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
    ExecArguments args(op, rte, nrArgs);

    REAL4 *r = (REAL4 *)args.dest();
    boost::scoped_ptr< IVSpatial<double> > expr(0);

    switch(args[0].cri()) {
      case CRI_4:
        expr.reset(new VSpatial<double,INT4>(args[0].src_4()));
        break;
      case CRI_f:
        expr.reset(new VSpatial<double,REAL4>(args[0].src_f()));
        break;
      default : POSTCOND(false);
    }
    orderOperation(r,*expr, args[0].nrValues());

    args.pushResults();
}

void calc::AreaOrder::exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
    ExecArguments args(op, rte, nrArgs);

    REAL4 *r = (REAL4 *)args.dest();
    boost::scoped_ptr< IVSpatial<double> > expr(0);
    boost::scoped_ptr< IVSpatial<INT4> >   areaClass(0);

    switch(args[0].cri()) {
      case CRI_4:
        expr.reset(new VSpatial<double,INT4>(args[0].src_4()));
        break;
      case CRI_f:
        expr.reset(new VSpatial<double,REAL4>(args[0].src_f()));
        break;
      default : POSTCOND(false);
    }
    switch(args[1].cri()) {
      case CRI_1:
        areaClass.reset(new VSpatial<INT4,UINT1>(args[1].src_1()));
        break;
      case CRI_4:
        areaClass.reset(new VSpatial<INT4,INT4>(args[1].src_4()));
        break;
      default : POSTCOND(false);
    }

    areaOrderOperation(r,*expr, *areaClass, args[0].nrValues());

    args.pushResults();
}

void calc::ArgOrderWithIdAddAreaLimited::exec (
    RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
    ExecArguments args(op, rte, nrArgs);

    PRECOND(args.size()%3 == 1);
    std::vector<ArgOrderIdInfo> argOrderArgs;
    for(size_t i=1; i < args.size(); i+=3) {

        PRECOND(args[i].isSpatial());
        const REAL4* chances=args[i].src_f();

        PRECOND(!args[i+1].isSpatial());
        INT4  id=args[i+1].src_4()[0];

        PRECOND(!args[i+2].isSpatial());
        double areaLimit=args[i+2].src_f()[0];

        argOrderArgs.push_back(ArgOrderIdInfo(chances,id,areaLimit));
    }
    INT4 *r = (INT4 *)args.dest();
    PRECOND(args[0].isSpatial());
    ArgOrderAndAddArea::argOrderAddAreaLimited(
        argOrderArgs,
        args[0].src_4(),
        r,args[0].nrValues());
    args.pushResults();
}

void calc::ArgOrderAddAreaLimited::exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
    ExecArguments args(op, rte, nrArgs);

    PRECOND(args.size()%2 == 1);
    std::vector<ArgOrderIdInfo> argOrderArgs;
    for(size_t i=1; i < args.size(); i+=2) {

        PRECOND(args[i].isSpatial());
        const REAL4* chances=args[i].src_f();

        INT4  id=argOrderArgs.size()+1;

        PRECOND(!args[i+1].isSpatial());
        double areaLimit=args[i+1].src_f()[0];

        argOrderArgs.push_back(ArgOrderIdInfo(chances,id,areaLimit));
    }
    INT4 *r = (INT4 *)args.dest();
    PRECOND(args[0].isSpatial());
    ArgOrderAndAddArea::argOrderAddAreaLimited(
        argOrderArgs,
        args[0].src_4(),
        r,args[0].nrValues());
    args.pushResults();
}

void calc::ArgOrderWithIdAreaLimited::exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
    ExecArguments args(op, rte, nrArgs);

    PRECOND(args.size()%3 == 0);
    std::vector<ArgOrderIdInfo> argOrderArgs;
    for(size_t i=0; i < args.size(); i+=3) {

        PRECOND(args[i].isSpatial());
        const REAL4* chances=args[i].src_f();

        PRECOND(!args[i+1].isSpatial());
        INT4  id=args[i+1].src_4()[0];

        PRECOND(!args[i+2].isSpatial());
        double areaLimit=args[i+2].src_f()[0];

        argOrderArgs.push_back(ArgOrderIdInfo(chances,id,areaLimit));
    }
    INT4 *r = (INT4 *)args.dest();
    ArgOrderAndAddArea::argOrderAreaLimited(argOrderArgs,r,args[0].nrValues());
    args.pushResults();
}

void calc::ArgOrderAreaLimited::exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
    ExecArguments args(op, rte, nrArgs);

    PRECOND(args.size()%2 == 0);
    std::vector<ArgOrderIdInfo> argOrderArgs;
    for(size_t i=0; i < args.size(); i+=2) {

        PRECOND(args[i].isSpatial());
        const REAL4* chances=args[i].src_f();

        INT4  id=argOrderArgs.size()+1;

        PRECOND(!args[i+1].isSpatial());
        double areaLimit=args[i+1].src_f()[0];

        argOrderArgs.push_back(ArgOrderIdInfo(chances,id,areaLimit));
    }
    INT4 *r = (INT4 *)args.dest();
    ArgOrderAndAddArea::argOrderAreaLimited(argOrderArgs,r,args[0].nrValues());
    args.pushResults();
}

void calc::ArgOrderWithId::exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
    ExecArguments args(op, rte, nrArgs);

    PRECOND(args.size()%2 == 0);
    std::vector<ArgOrderIdInfo> argOrderArgs;
    for(size_t i=0; i < args.size(); i+=2) {

        PRECOND(args[i].isSpatial());
        const REAL4* chances=args[i].src_f();

        PRECOND(!args[i+1].isSpatial());
        INT4  id=args[i+1].src_4()[0];

        argOrderArgs.push_back(ArgOrderIdInfo(chances,id));
    }

    INT4 *r = (INT4 *)args.dest();
    ArgOrderAndAddArea::argOrder(argOrderArgs,r,args[0].nrValues());
    args.pushResults();
}

void calc::ArgOrder::exec (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
    ExecArguments args(op, rte, nrArgs);

    std::vector<ArgOrderIdInfo> argOrderArgs;
    for(size_t i=0; i < args.size(); ++i) {

        PRECOND(args[i].isSpatial());
        const REAL4* chances=args[i].src_f();

        INT4  id=argOrderArgs.size()+1;

        argOrderArgs.push_back(ArgOrderIdInfo(chances,id));
    }

    INT4 *r = (INT4 *)args.dest();
    ArgOrderAndAddArea::argOrder(argOrderArgs,r,args[0].nrValues());
    args.pushResults();
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



