#include "stddefx.h"

#ifndef INCLUDED_CALC_TSSOUTPUTVALUE
#include "calc_tssoutputvalue.h"
#define INCLUDED_CALC_TSSOUTPUTVALUE
#endif

// std libraries

#ifndef INCLUDED_IOMANIP
#include <iomanip>
#define INCLUDED_IOMANIP
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

#ifndef INCLUDED_BOOST_STATIC_ASSERT
#include <boost/static_assert.hpp>
#define INCLUDED_BOOST_STATIC_ASSERT
#endif

// our libraries

#ifndef INCLUDED_DAL_TYPE
#include "dal_Type.h"
#define INCLUDED_DAL_TYPE
#endif

#ifndef INCLUDED_CALC
#include "calc.h"      // TssRow stuff
#define INCLUDED_CALC
#endif

#ifndef INCLUDED_APPARGS
# include "appargs.h"
#define INCLUDED_APPARGS
#endif

#ifndef INCLUDED_NEW
#include "com_new.h"
#define INCLUDED_NEW
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

// modules

#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif

#ifndef INCLUDED_CALC_AVERAGEMAP
#include "calc_averagemap.h"
#define INCLUDED_CALC_AVERAGEMAP
#endif
#ifndef INCLUDED_CALC_APIMAP
#include "calc_apimap.h"
#define INCLUDED_CALC_APIMAP
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif


#ifndef INCLUDED_CALC_IOSTRATEGY
#include "calc_iostrategy.h"
#define INCLUDED_CALC_IOSTRATEGY
#endif

#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEM
#include "calc_MemoryExchangeItem.h"
#define INCLUDED_CALC_MEMORYEXCHANGEITEM
#endif

const size_t calc::FileTimeoutput::maxCacheSize=128;

namespace calc { namespace detail {

static int doTimeoutput(
  double      *val,
  const Field *id,
  const Field *expr,
  size_t nrCols)
{
  int result=0;
  if (expr->cri() == CRI_f) {
    AverageMap am;
    if (id->cri()==CRI_1)
      am.apply(id->src_1(),id->nrValues(),
               expr->src_f(),expr->nrValues());
    else
      am.apply(id->src_4(),id->nrValues(),
               expr->src_f(),expr->nrValues());
    am.setResults(val,nrCols);
  } else {
     // timeoutputting class data the old "inefficient" way
     //
     // CRI_1 CRI_4
     // create a rasterDim of 1 row by nrCells cols
     //   allowing to work with MaskPacking data
     size_t nrCells=std::max(id->nrValues(),expr->nrValues());
     geo::RasterDim rs(1,nrCells);
     calc::ApiMapC<MAP_INT4> idM(rs,id->src(),id->isSpatial(),id->cr());

     calc::ApiMapC<MAP_INT4> exprM(rs,expr->src(),expr->isSpatial(),expr->cr());
     result = AddToTssRowINT4(val,nrCols, idM.map(), exprM.map());
  }
  return result;
}

static size_t maxId(const Field *id)
{
  size_t nrCols= 0;
  size_t maxIndex;
  switch(id->cri()) {
    case CRI_1: {
     const UINT1* m = com::csfCellMax(id->src_1(),id->src_1()+id->nrValues());
     maxIndex=m-id->src_1();
     if (maxIndex!=id->nrValues())
       nrCols=id->src_1()[maxIndex];
    } break;
    case CRI_4: {
     const INT4* m = com::csfCellMax(id->src_4(),id->src_4()+id->nrValues());
     maxIndex=m-id->src_4();
     if (maxIndex!=id->nrValues())
       nrCols=id->src_4()[maxIndex];
    } break;
    default: PRECOND(FALSE);
  }
  return nrCols;
}

 template<typename T>
  class MemoryExchangeItemTssRow: public MemoryExchangeItem
  {
   size_t  d_len;
   char   *d_buffer;
   struct Header {
     UINT4  id; // identification of header type
     UINT4  vt; // value type
     UINT4  nrDim; // number of dimensions
     UINT4  lenDim1; // length of dimension 1
   };
   public:
     MemoryExchangeItemTssRow(std::string const& name,
                              size_t id,
                              const std::vector<double>& data):
          MemoryExchangeItem(name, id),
          d_len(sizeof(Header)+(sizeof(T)*data.size())),
          d_buffer(new char[d_len])
        {
          Header header;
          header.id = 1;
          header.vt = dal::TypeTraits<T>::csfCr;
          header.nrDim=1;
          header.lenDim1=(UINT4)data.size();

          BOOST_STATIC_ASSERT(sizeof(Header) == (4 * sizeof(UINT4)));
          *((Header *)d_buffer) = header;

          T *dest = (T *)(d_buffer+sizeof(Header));
          for(size_t i=0; i < data.size(); ++i) {
             if(pcr::isMV(data[i]))
               pcr::setMV(dest[i]);
             else
               dest[i] = (T)(data[i]);
          }
        }
    ~MemoryExchangeItemTssRow() {
     delete[] d_buffer;
    }
    void *rawValue    () const
    {
      return d_buffer;
    }
    void  beMemCpySrc (void *dest) const
    {
      std::memcpy(d_buffer, dest, d_len);
    }
  };

} }


calc::TssOutputValue::TssOutputValue()
{
}
calc::TssOutputValue::~TssOutputValue()
{
}


size_t calc::FileTimeoutput::initNrRowsCached() const
{
  size_t nrt=d_stackInfo.lastInt();

  if (appHeader == APP_NOHEADER) {
    // then write at last timeStep, to support current tcl-interface
    return nrt;
  }
  if (d_stackInfo.flushTssAtEachTimeStep())
    return 1;
  return std::min<size_t>(nrt,maxCacheSize);
}

//! ctor
/*!
 * Note that the file is not created here, but in timeStep nr 1.
 */
calc::FileTimeoutput::FileTimeoutput(
    const StackInfo& stackInfo,
    size_t nrCols):
  TssOutputValue(),
  d_stackInfo(stackInfo),
  d_nrCols(nrCols),
  d_fileErrorOccured(false),
  d_fileCreated(false),
  d_lastStepToFile(0),
  d_value(0)
{
  PRECOND(nrInSet(d_stackInfo.vs()) == 1);
  PRECOND(d_nrCols > 0);
  d_nrRowsCached=initNrRowsCached();
  PRECOND(d_nrRowsCached > 0);
  d_value=com::new2d<double>(d_nrRowsCached,d_nrCols);
}

calc::FileTimeoutput::~FileTimeoutput()
{
  if (! d_fileErrorOccured) {
    // bugzilla 87 and 160  pcrcalc561
    // if tss creation was the cause of the exception then
    //  it fails again.
    // do not generate new exceptions here!
    try {
     finish();
    } catch(...) {
    }
  }
  com::delete2d<double>(d_value);
}

void calc::FileTimeoutput::finish()
{
  // if data left, write it otherwise not
  // the if prevent the case that we only
  // write a header and no data
  if (d_step.size())
    flushToFile();
}

bool calc::FileTimeoutput::reportTimeStep(size_t t) const
{
  if (appHeader == APP_NOHEADER) // obey report clause
   return d_stackInfo.reportTimeStep(t);
  return true;
}

//! set row to correct position and return buffer of row
/*!
 * \returns 0 if currentTimeStep does not have to be written
 */
double *calc::FileTimeoutput::setRow(size_t currentTimeStep)
{
  /* pcrcalc/test13[cd] */
  if (!reportTimeStep(currentTimeStep))
    return 0;
  if (d_step.size() && currentTimeStep == d_step.back()) {
    // pcrcalc/test317 writing a value twice in loop!
    return d_value[d_step.size()-1];
  }

  if (d_step.size() == d_nrRowsCached)
    flushToFile();

  POSTCOND(d_step.size() < d_nrRowsCached);
  d_step.push_back(currentTimeStep);

  return d_value[d_step.size()-1];
}

void calc::FileTimeoutput::openFile(std::ofstream& ofs)
{
  if (d_fileCreated) {
   // otherwise we open for appending
   com::open(ofs,d_stackInfo.stackName(), std::ios::app);
   return;
  }
  // begin, create file
  d_fileCreated = true;
  com::open(ofs,d_stackInfo.stackName());

  if (appHeader == APP_DEFHEADER) {
    // print header
    CSF_VS vs = vs2CsfVs(d_stackInfo.vs());
    if (vs == VS_UNDEFINED)
     ofs << "summary\n";
    else
     ofs << "timeseries " << toString(d_stackInfo.vs()) << "\n";
    ofs << d_nrCols+1 << "\n"
        << "timestep\n";
    for(size_t c = 0; c < d_nrCols; c++)
     ofs << c+1 << "\n";
  }
}
/*
 * \todo  dangerous typecase VS->CSF_VS
 */
void calc::FileTimeoutput::printLine(
  size_t         t,
  double        *v,
  std::ofstream& f)
{
  const char *mvFmt;
  int         valFmt;

  switch((CSF_VS)d_stackInfo.vs()) { // dangerous typecast
     case VS_LDD    :
     case VS_BOOLEAN: mvFmt = "1e31"; // len 4
                      valFmt = 4; break;
     case VS_ORDINAL:
     case VS_NOMINAL: mvFmt = "      1e31"; // len 10
                      valFmt = 10; break;
     default        : mvFmt = "       1e31"; // len 11
                      valFmt = 11; break;
                      // was %11.6g 6 is default precision
  }

  // print time at begin of line
  f << std::setw(8) << t;

  // print columns of row on line of file
  for(size_t c = 0; c < d_nrCols; c++)
    if (!v || IS_MV_REAL8(v+c))
       f << mvFmt;
    else {
       double d = v[c];
       if (d_stackInfo.vs() == VS_D)
         d = AppOutputDirection(d);
       f << " " << std::setw(valFmt) << d;
    }
  f << std::endl;
  if (!f.good()) {
      d_fileErrorOccured=true;
      throw com::FileError(d_stackInfo.stackName(),"writing data failed");
  }
}

void calc::FileTimeoutput::flushToFile()
{
  assert(!d_fileErrorOccured);
  std::ofstream f;
  try {
   openFile(f);
  } catch(...) {
    d_fileErrorOccured=true;
    throw;
  }

  // print each row of time table on new line in file
  for(size_t i=0; i < d_step.size(); ++i) {
   if (appHeader != APP_NOHEADER) {
     // write intervening TODO last end is not written
     for(size_t j=d_lastStepToFile+1; j < d_step[i]; ++j)
      printLine(j,0,f);
   }
   printLine(d_step[i],d_value[i],f);
   d_lastStepToFile=d_step[i];
  }
  d_step.clear();
}

void calc::FileTimeoutput::nonspatial(
  const Field *f,
  size_t currentTimeStep)
{
  double *v=setRow(currentTimeStep);
  if (v)
   f->getCell(*v,0);
}


void calc::FileTimeoutput::timeoutput(
  const Field *id,
  const Field *expr,
  size_t currentTimeStep)
{
  // val[0] will hold for id 1, val[2] for id 2, etc.
  double *val = setRow(currentTimeStep);

  if (!val) // do not write this time step
    return;

  int result= detail::doTimeoutput(val,id,expr, d_nrCols);

  if (result) {
    d_fileErrorOccured=true;
    throw std::runtime_error("Failed to add data to timeseries");
  }
}


//! create if \a id contains values > 0, return 0 otherwise
/*! StackInfo::d_vs is updated if created
 */
calc::FileTimeoutput* calc::createFileTimeoutput(
    const StackInfo& stackInfo,
    const Field* id)
{
  size_t nrCols = detail::maxId(id);
  if (nrCols) {
    return new FileTimeoutput(stackInfo,nrCols);
  }
  return 0;
}

calc::MemoryTimeoutput::MemoryTimeoutput(
    const std::string& name,
    size_t             memoryId,
    IOStrategy&        ios):
     TssOutputValue(),
     d_name(name),
     d_memoryId(memoryId),
     d_ios(ios)
{
}

void calc::MemoryTimeoutput::finish()
{
}


void calc::MemoryTimeoutput::timeoutput(
    const Field *id,
    const Field *expr,
    size_t )
{
  size_t nrCols = detail::maxId(id);
  // reuse code above with std::vector<double> in
  // then recast in case of INT4 or UINT1
  std::vector<double> val(nrCols);
  int result = detail::doTimeoutput(&(val[0]), id, expr, nrCols);
  if (result) {
    throw std::runtime_error("Failed to add data to timeseries");
  }

  MemoryExchangeItem *data(0);
  switch(expr->cri()) {
   case CRI_1:
   {
     data = new detail::MemoryExchangeItemTssRow<UINT1>(d_name, d_memoryId,val);
     break;
   }
   case CRI_4:
   {
     data = new detail::MemoryExchangeItemTssRow<INT4>(d_name, d_memoryId,val);
     break;
   }
   case CRI_f:
   {
     data = new detail::MemoryExchangeItemTssRow<REAL4>(d_name, d_memoryId,val);
     break;
   } break;
   default:
    assert(false);
  }
  d_ios.transferMemoryExchangeItemIntoDataTransferArray(data);
}
