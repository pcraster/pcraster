#include "stddefx.h"
#include "calc_operations.h"
#include "calc_operator.h"
#include "calc_calclib.h"
#include "calc_objectlinkmeta.h"
#include "calc_functiontable.h"
#include "calc_opimpl.h"
#include "calc_global.h"
#include "calc_accuimpl.h"
#include "calc_dynamicwave.h"
#include "calc_cellop.h"
#include "calc_globfunc.h"
#include "calc_lookup.h"
#include "calc_timeinputtssop.h"
#include "calc_timeinputstackop.h"
#include "calc_foarrayimplmanual.h"
#include "calc_fopointarray.h"
#include "calc_fopointimpl.h"

#include <format>

/*!
  \file
  This file contains the implementation of the Operations class.
*/



//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC OPERATIONS MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF OPERATIONS MEMBERS
//------------------------------------------------------------------------------

calc::Operations::Operations()
{
  try {
   init();
  } catch(...) {
   clean();
   throw;
  }
}


calc::Operations::~Operations()
{
  clean();
}

void calc::Operations::clean()
{
  for(auto & i : d_nameOp)
    delete i.second;
  for(auto & d_lib : d_libs)
    delete d_lib.second;
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::Operations& calc::Operations::operator=(const Operations& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::Operations::Operations(const Operations& rhs):
  Base(rhs)
{
}
*/

void calc::Operations::add(const Operator *o)
{
  d_nameOp.insert(std::make_pair(o->name(),o));
  // d_codeOp owns the Operator objects
  d_codeOp.insert(std::make_pair(o->opCode(),o));
}

//! add the MRF relations
/*!
 * \pre all 3 operations in play are already added (add() in d_codeOp)
 */
void calc::Operations::add(
    MAJOR_CODE mrf, MAJOR_CODE oneOf1, MAJOR_CODE oneOf2)
{
  MRFRelatives r;
  r.d_mrf=mrf;

  r.d_otherOneOfMRF = oneOf2;
  r.d_stackTop      = false;
  d_mrfRelations.insert(std::make_pair(oneOf1, r));

  r.d_otherOneOfMRF = oneOf1;
  r.d_stackTop      = true;
  d_mrfRelations.insert(std::make_pair(oneOf2, r));
}

//! find other operation as used in <tt> a,b=oneOfMRF,other(....)</tt>
/*!
 * \returns MAJOR_CODE of other operation or OP_NOP if not found.
 */
MAJOR_CODE calc::Operations::otherOneOfMRF(MAJOR_CODE oneOfMRF)  const
{
  auto i= d_mrfRelations.find(oneOfMRF);
  if (i == d_mrfRelations.end())
    return OP_NOP;
  return i->second.d_otherOneOfMRF;
}

MAJOR_CODE calc::Operations::oneOf2Mrf(MAJOR_CODE oneOfMRF)  const
{
  auto i= d_mrfRelations.find(oneOfMRF);
  POSTCOND(i != d_mrfRelations.end());
  return i->second.d_mrf;
}

bool calc::Operations::oneOfMrfIsStackTop(MAJOR_CODE oneOfMRF) const
{
  auto i= d_mrfRelations.find(oneOfMRF);
  POSTCOND(i != d_mrfRelations.end());
  return i->second.d_stackTop;
}

void calc::Operations::init()
{
 static const AggregateArray< MapTotal< REAL4 > > foAr_maptotal_f;
 static const AggregateArray< MapAnd< UINT1 > > foAr_mapand_1;
 static const AggregateArray< MapOr< UINT1 > > foAr_mapor_1;
 static const AggregateArray< MapMaximum< REAL4 > > foAr_mapmaximum_f;
 static const AggregateArray< MapMaximum<  INT4 > > foAr_mapmaximum_4;
 static const AggregateArray< MapMinimum< REAL4 > > foAr_mapminimum_f;
 static const AggregateArray< MapMinimum<  INT4 > > foAr_mapminimum_4;
 static const AggregateArray< MapArea<REAL4, UINT1 > > foAr_maparea_1;
 static const AggregateArray< MapArea<REAL4,  INT4 > > foAr_maparea_4;
 static const AggregateArray< MapArea<REAL4, REAL4 > > foAr_maparea_f;

 static DiffUn builtIn_maptotal(&foAr_maptotal_f);
 static DiffUn builtIn_mapand(&foAr_mapand_1);
 static DiffUn builtIn_mapor(&foAr_mapor_1);
 static DiffUn builtIn_mapmaximum(&foAr_mapmaximum_4,&foAr_mapmaximum_f);
 static DiffUn builtIn_mapminimum(&foAr_mapminimum_4,&foAr_mapminimum_f);
 static DiffUn builtIn_maparea(&foAr_maparea_1,&foAr_maparea_4,&foAr_maparea_f);

 static const PitArray foAr_pit;
 static DiffUn builtIn_pit(&foAr_pit);

 static const DefinedArray< UINT1 > foAr_defined_1;
 static const DefinedArray<  INT4 > foAr_defined_4;
 static const DefinedArray< REAL4 > foAr_defined_f;
 static DiffUn builtIn_defined(&foAr_defined_1,&foAr_defined_4,&foAr_defined_f);

 static const SpatialArray< UINT1 > foAr_spatial_1;
 static const SpatialArray<  INT4 > foAr_spatial_4;
 static const SpatialArray< REAL4 > foAr_spatial_f;
 static SpatialImpl builtIn_spatial(&foAr_spatial_1,&foAr_spatial_4,&foAr_spatial_f);

 static const CoverArray   < UINT1 > foAr_cover_1;
 static const CoverArray   <  INT4 > foAr_cover_4;
 static const CoverArray   < REAL4 > foAr_cover_f;
 static SameBin builtIn_cover(&foAr_cover_1,&foAr_cover_4,&foAr_cover_f);

 static const IfThenArray   < UINT1 > foAr_ifthen_1;
 static const IfThenArray   <  INT4 > foAr_ifthen_4;
 static const IfThenArray   < REAL4 > foAr_ifthen_f;
 static DiffBin builtIn_ifthen(&foAr_ifthen_1,&foAr_ifthen_4,&foAr_ifthen_f);

 static const IfThenElseArray   < UINT1 > foAr_ifthenelse_1;
 static const IfThenElseArray   <  INT4 > foAr_ifthenelse_4;
 static const IfThenElseArray   < REAL4 > foAr_ifthenelse_f;
 static IfThenElse builtIn_ifthenelse(&foAr_ifthenelse_1,
                   &foAr_ifthenelse_4,&foAr_ifthenelse_f);
// builtIn_ifthenelse.setPointFunction("a1 ? a2 : a3");

static GenSpatial  builtIn_GenSP;
#define builtIn_uniqueid builtIn_GenSP;
#define builtIn_xcoordinate builtIn_GenSP;
#define builtIn_ycoordinate builtIn_GenSP;
#define builtIn_normal builtIn_GenSP;
#define builtIn_uniform builtIn_GenSP;
static GenNonSpatial  builtIn_GenNS;
#define builtIn_celllength builtIn_GenNS;
#define builtIn_cellarea   builtIn_GenNS;
#define builtIn_time       builtIn_GenNS;
#define builtIn_timeslice  builtIn_GenNS;
#define builtIn_mapuniform builtIn_GenNS;
#define builtIn_mapnormal  builtIn_GenNS;
static Conversion      builtIn_Conversion;
#define builtIn_directional builtIn_Conversion;
#define builtIn_scalar      builtIn_Conversion;
#define builtIn_nominal     builtIn_Conversion;
#define builtIn_ordinal     builtIn_Conversion;
#define builtIn_ldd         builtIn_Conversion;
#define builtIn_boolean_    builtIn_Conversion;
static TimeinputTssOp          builtIn_TimeinputTss;
#define builtIn_timeinputboolean     builtIn_TimeinputTss;
#define builtIn_timeinputordinal     builtIn_TimeinputTss;
#define builtIn_timeinputnominal     builtIn_TimeinputTss;
#define builtIn_timeinputscalar      builtIn_TimeinputTss;
// #define builtIn_timeinputlinear      builtIn_TimeinputTss;
#define builtIn_timeinputdirectional builtIn_TimeinputTss;
#define builtIn_timeinputldd         builtIn_TimeinputTss;
static TimeinputStackOp        builtIn_TimeinputStackOp;
#define builtIn_timeinput            builtIn_TimeinputStackOp;
#define builtIn_timeinputsparse      builtIn_TimeinputStackOp;
#define builtIn_timeinputmodulo      builtIn_TimeinputStackOp;
static LookupMapStack                builtIn_lookupmapstack;
#include "calc_fopointarrayimpl.inc"
#include "calc_builtinops.inc"
#include "operationsInit.inc"
 POSTCOND(size());
 {
   const FuncNameOpcode *f=funcName2OpCode+0;
   while(f->d_name[0] != '\0') {
    d_funcs.insert(std::make_pair(f->d_name,f->d_op));
     f++;
   }
   POSTCOND(!d_funcs.empty());
 }
}

//! find by user name
/*!
 * Use only for names known to be a function, finding by Operator::name()
 * is unspecified for  -,+, ifthen(else) and other special constructs.
 * Garantueed to work with externals
 *
 * \return 0 if not found or the Operator
 */
const calc::Operator* calc::Operations::operator[](const std::string& name) const
{
  auto i=d_nameOp.find(name);
  if (i==d_nameOp.end())
    return nullptr;
  return i->second;
}

//! find by MAJOR_CODE
/*!
 * only sensible for built-ins, externals do not have a distinct MAJOR_CODE
 *
 * \return 0 if not found or the Operator
 */
const calc::Operator* calc::Operations::operator[](const MAJOR_CODE   major)const
{
  auto i=d_codeOp.find(major);
  if (i==d_codeOp.end())
    return nullptr;
  return i->second;
}

//! find a function, return OP_NOP if no such function
MAJOR_CODE calc::Operations::function(const std::string& name) const {
 auto i= d_funcs.find(name);
 if (i != d_funcs.end())
      return i->second;
 return OP_NOP;
}

size_t calc::Operations::size()const
{
  return d_codeOp.size();
}

//! load library libName if not already loaded
void   calc::Operations::loadLib(const std::string& libName)
{
  auto i=d_libs.find(libName);
  if (i == d_libs.end()) {
    auto *l= new CalcLib(libName);
    load(l->getMeta());
    d_libs.insert(std::make_pair(libName,l));
  }
}

//! load single ObjectLink by its ObjectLinkMeta info
/*!
 * \throws
 *   com::Exception if className already known as function or objectlink
 */
void   calc::Operations::load(const CalcLib::GetMeta& gm)
{
  ObjectLinkMeta olm(gm());
  std::string className(olm.className());
  const ObjectLinkMeta::MethodMap& m(olm.methodMap());

  if (d_nameOp.find(className) != d_nameOp.end()){
    throw com::Exception(
     std::format("objectlink name '{0}' not unique", className));
  }

  for(const auto & i : m) {
     // ctor
     std::string methodName(className);
     if (!i.first.empty()) // not ctor
       methodName += "::" + i.first;
     d_nameOp.insert(std::make_pair(methodName,
           new Operator(methodName,
                        i.first,
                        i.second.d_result,
                        i.second.d_input,
                        olm.objectLinkFactory())));
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
