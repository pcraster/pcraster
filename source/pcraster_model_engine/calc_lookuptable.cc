#include "stddefx.h"

#ifndef INCLUDED_CALC_LOOKUPTABLE
#include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif

#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif

#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_TABLE
#include "table.h"    // LOOK_UP_TABLE
#define INCLUDED_TABLE
#endif

#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif

#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#include "calc_globallibdefs.h"
#define INCLUDED_CALC_GLOBALLIBDEFS
#endif

#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif

#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif

#ifndef INCLUDED_CALC_ASTSYMBOLINFO
#include "calc_astsymbolinfo.h"
#define INCLUDED_CALC_ASTSYMBOLINFO
#endif
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_CALC_PCRASTERXSD
#endif

namespace calc {
 namespace detail {
 //! map: key->[begin,end)
 struct LookupTablePrefixMap :
   public std::map<
     RelationRecord::Float,
     std::pair<
       LookupTable::const_iterator,
       LookupTable::const_iterator
       >
     >
 {
 };
 class AddRecord {
   private:
     LookupTable::Records *d_records;
   public:
      AddRecord(LookupTable::Records& records):
        d_records(&records)
       {}
      void addCol(pcrxml::LookupColumn const& c)
      {
        std::string cStr(c);
        d_records->back().push_back(
         com::createIntervalFromLookupTableKey<RelationRecord::Float>(cStr));
      }
      void operator()(pcrxml::LookupRow const& r)
      {
        d_records->push_back(RelationRecord());
        std::for_each(r.lookupColumn().begin(), r.lookupColumn().end(),
             boost::bind(&AddRecord::addCol,this,_1));
      }
 };

 class MemoryInputTableCreator {
  public:
    virtual void setData(void const* data)=0;
    virtual bool find(double& result, const std::vector<float>& prefixKey) const=0;
    virtual bool empty()const=0;
 };

 template <typename ArrayValueDataType>
  class T_MemoryInputTableCreator : public MemoryInputTableCreator {
     std::vector<ArrayValueDataType> d_values;
   public:
     void setData(void const* memoryArray) {

       d_values.clear();
       if (!memoryArray)
         return;

       std::vector<size_t> dimensionSize;

       // UINT4 as xs:unsignedInt;
       UINT4 const* dimensions((UINT4 const *)memoryArray);
       size_t nrDimensions = *dimensions;
       for(size_t i=0; i < nrDimensions; ++i) {
          dimensions++;
          dimensionSize.push_back(*dimensions);
       }

       // current limitation
       if (dimensionSize.size() != 1)
          throw std::range_error("Only 1 dimension supported");

       // copy the values
       // FTTB only 1-dimension
       dimensions++;
       ArrayValueDataType const* values((ArrayValueDataType const*)dimensions);
       d_values.reserve(dimensionSize[0]);
       for(size_t i=0; i < dimensionSize[0]; ++i)
          d_values.push_back(values[i]);
     }
     /*!
      * \todo Checking for d_values.empty() runtime error here is not efficient.
      *       Should be done when data is loading for operation e.g. on push or pop of execution stack
      */
     bool find(double& result, const std::vector<float>& prefixKey) const
     {
       PRECOND(!d_values.empty()); // catched in LookupTable::load()
       if (prefixKey.size() != 1)
          throw std::range_error("Only 1 dimension supported");
       // INDEX is always 1-based currently, outside that MV return
       if (prefixKey[0] < 1 || prefixKey[0] > d_values.size())
          return false;
       size_t i(prefixKey[0]-1);
       result = d_values[i];
       return true;
     }
     bool empty() const
     {
       return d_values.empty();
     }
  };
 }
}

//! create with lookup table from table C-library
LOOK_UP_TABLE *calc::LookupTable::createOldStyle(
  const std::string &fileName)
{
  LOOK_UP_TABLE *t(0);
  FILE  *f = fopen(fileName.c_str(), "r");
  if (!f)
    libError("Can't open lookup table "+quote(fileName));

  try {
   std::vector<CSF_VS> csfVs(d_vs.size());
   for(size_t i=0; i < d_vs.size(); i++)
     csfVs[i] = vs2CsfVs(biggestVs(d_vs[i]));
   // discern between last (back) and rest of columns
   t = ReadLookupTable(f, &(csfVs[0]), csfVs.size()-1, csfVs.back());
   if (!t) {
     // pcrcalc10a
     libError("while parsing lookuptable "+quote(fileName));
   }
  } catch ( ... ) {
     fclose(f);
     throw;
  }

  fclose(f);
  return t;
}


calc::LookupTable::LookupTable():
     d_prefixMap(0),
     d_memoryInputTableCreator(0)
{
}

/*!
 * \todo check length en types van LookupRow / LOOKUP_TABLE post mortem
 *         met error als row/col index
 */
calc::LookupTable::LookupTable(
    const ASTSymbolInfo& i):
     d_prefixMap(0),
     d_memoryInputTableCreator(0),
     d_vs(i.dataType().resultType())
{
  std::vector<VS> vs(i.dataType().tableColTypes());
  if (vs.empty())
    vs.push_back(VS_S);
  if (i.lookupTable())
   setXMLRecords(*(i.lookupTable()),vs);
  else
   setRecords(i.externalName(),vs);
  // TODO check sizes / rectangle
}

//! flush all data
void calc::LookupTable::reset(const std::vector<VS>& vs)
{
  d_vs=vs;
  clear();
}

void calc::LookupTable::clear()
{
  d_records.clear();
  delete d_prefixMap;
  d_prefixMap=0;
  delete d_memoryInputTableCreator;
  d_memoryInputTableCreator=0;
}

void calc::LookupTable::setXMLRecords(const pcrxml::Relation& r,
                                      const std::vector<VS>& vs)
{
  reset(vs);

  if (r.lookupTable().present()) {
    const pcrxml::LookupTable& l(r.lookupTable().get());
    detail::AddRecord ar(d_records);
    std::for_each(l.lookupRow().begin(), l.lookupRow().end(), ar);
  } else {
    PRECOND(r.indexedArray().present());
    const pcrxml::IndexedArray& a(r.indexedArray().get());
    PRECOND(a.dimensionDataType() == pcrxml::ArrayDimensionDataType::unsignedInt);
    switch (a.valueDataType()) {
     case pcrxml::ArrayValueDataType::int_:
        d_memoryInputTableCreator= new detail::T_MemoryInputTableCreator<INT4>();
       break;
     case pcrxml::ArrayValueDataType::float_:
        d_memoryInputTableCreator= new detail::T_MemoryInputTableCreator<REAL4>();
       break;
     case pcrxml::ArrayValueDataType::double_:
        d_memoryInputTableCreator= new detail::T_MemoryInputTableCreator<REAL8>();
       break;
     default:
      PRECOND(false);
   }
  }
}

/*!
 *  \throws  std::range_error if array format violates current limitations
 */
void calc::LookupTable::setArrayValue(void const* data)
{
  PRECOND(d_memoryInputTableCreator);
  d_memoryInputTableCreator->setData(data);
}

//! parse records from ASCII file
/*!
 * \throws com::Exception in case of error
 */
void calc::LookupTable::setRecords(
  const std::string     &fileName,
  const std::vector<VS>& vs)
{
  reset(vs);
  // table->nrKeys are the number of total columns plus 1
  LOOK_UP_TABLE *table(createOldStyle(fileName));

  try {
    d_records.reserve(table->nrRecords);
    for(size_t r=0; r < table->nrRecords; ++r) {
     DEVELOP_PRECOND(table->nrKeys+1==d_vs.size());
     d_records.push_back(RelationRecord(table->records[r],d_vs.size()));
    }
  } catch(...) {
    FreeLookupTable(table);
    throw;
  }
  FreeLookupTable(table);
}




//! set records
void calc::LookupTable::setRecords(
    const Records& records,
    const std::vector<VS>& vs)
{
    reset(vs);
    PRECOND(records.empty() || records[0].size() == vs.size());
    d_records=records;
}

calc::DataValue*  calc::LookupTable::load()
{
  // now we need! it
  if (d_memoryInputTableCreator) {
   if (d_memoryInputTableCreator->empty())
     throw std::range_error("0-ptr data buffer passed");
  }
  return this;
}

//! dtor
calc::LookupTable::~LookupTable()
{
  clear();
}

//! nr of keys columns in table
size_t calc::LookupTable::nrCols() const
{
  return d_vs.size();
}

calc::LookupTable::const_iterator
 calc::LookupTable::find(const Key& prefixKey) const
{
  return std::find_if(d_records.begin(),d_records.end(),
      boost::bind(&calc::RelationRecord::match,_1,boost::ref(prefixKey)));
}

//! find first match on key and return result
/*!
 * \param result       set to result, untouched if return value is false
 * \param prefixKey    the first keys to search for
 */
bool calc::LookupTable::find(double& result, const Key& prefixKey) const
{
  if (d_memoryInputTableCreator)
     return d_memoryInputTableCreator->find(result, prefixKey);

  PRECOND(prefixKey.size() == nrCols()-1);

  I p=find(prefixKey);
  if (p != d_records.end()) {
    result = p->back()->centre();
    return true;
  }
  return false;
}

//! lookuplinear interface, illnamed
/*!
 * \param result set to result, untouched if return value is false
 * \param key    the keys to search for
 */
bool calc::LookupTable::interpolate(double& result, const Key& key) const
{
  PRECOND(key.size() == 1);
  PRECOND(nrCols()   == 2);
  return interpolate(result, d_records.begin(),d_records.end(), key[0],0,1);
}

bool calc::LookupTable::interpolate(
    double& result,
    const   Key& prefixKey, // to search the [0,prefixKey.size()> first elements
    double  keyValue,
    size_t  keyCol,
    size_t  resultCol) const
{
  PRECOND(d_prefixMap);
  PRECOND(prefixKey.size()==1); // FTTB

  detail::LookupTablePrefixMap::const_iterator i=d_prefixMap->find(prefixKey[0]);
  if (i==d_prefixMap->end())
    return false;
  return interpolate(result,
      i->second.first,
      i->second.second,
      keyValue,keyCol, resultCol);

/*
  I begin=find(prefixKey);
  if (begin == d_records.end())
    return false;
  // find last
  I end=begin;
  while(end != d_records.end() && end->match(prefixKey))
    ++end;
  return interpolate(result,begin,end,keyValue,keyCol, resultCol);
 */
}


//! lookuplinear
/*!
 * \param result set to result, untouched if return value is false
 * \param begin  begin
 * \param end    end
 * \param keyValue  keyValue
 * \param keyCol    keyCol
 * \param resultCol  resultCol
 * \pre   [begin,end) is sorted on keyCol
 */
bool calc::LookupTable::interpolate(
    double& result,
    const_iterator  begin,
    const_iterator  end,
    double  keyValue,
    size_t  keyCol,
    size_t  resultCol) const
{
#ifdef DEBUG_DEVELOP
  for (I i=begin; i!=(end-1); ++i)
   DEVELOP_PRECOND(i->col(keyCol) < (i+1)->col(keyCol));
#endif
  DEVELOP_PRECOND(end <= d_records.end());

  // first element that has a value >= keyValue
  I gt= std::lower_bound(begin,end,keyValue,RelationRecordColLess(keyCol));
  if (gt == end)
    return false;
  // is keyValue equal to gt ?
  if (gt->col(keyCol).valid(keyValue)) {
         result=gt->col(resultCol).centre();
         return true;
  }
  // then > gt
  if (gt == begin)
    return false; // before begin

  // thus value between (gt-1) and gt
  I lt=gt-1;
  DEVELOP_PRECOND(lt->col(keyCol).max() != com::Interval<>::maxLimit());
  DEVELOP_PRECOND(gt->col(keyCol).min() != com::Interval<>::minLimit());
  // interpolate linear
  result = com::interpolate2(keyValue,
             (*lt).col(keyCol).max(),(*lt)[resultCol]->centre(),
             (*gt).col(keyCol).min(),(*gt)[resultCol]->centre());
  return true;
}

/*!
 *
 * \todo
 *   now only for the dynamicwave table: only column 1 being an equalTo value.
 */
void  calc::LookupTable::setPrefixStableSort(size_t prefixLen)
{
  // already constructed
  if (d_prefixMap)
    return;

#ifdef DEBUG_DEVELOP
  // current limitations
  PRECOND(prefixLen==1);
  for (I i = d_records.begin(); i!=d_records.end();++i)
   DEVELOP_PRECOND(i->col(0).equalTo());
#endif

  d_prefixMap= new detail::LookupTablePrefixMap();
  if (d_records.empty())
    return; // done
  std::stable_sort(d_records.begin(),d_records.end(),
                   RelationRecordColLess(0));


  I                 begin=d_records.begin();
  RelationRecord::Float k=begin->col(0).centre();

  for(I i=begin; i != d_records.end(); ++i) {
    if (k!=i->col(0).centre()) {
      d_prefixMap->insert(
        std::make_pair(k,std::make_pair(begin,i)));
      begin=i;
    }
    k=begin->col(0).centre();
  }
  if (begin!=d_records.end())
      d_prefixMap->insert(
        std::make_pair(k,std::make_pair(begin,d_records.end())));
}

calc::OVS calc::LookupTable::ovs() const
{
  return VS_TABLE;
}

calc::LookupTable::Records  const& calc::LookupTable::records() const
{
  return d_records;
}
