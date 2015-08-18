#include "stddefx.h"

#ifndef INCLUDED_CALC_LOOKUPTABLE
#include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif

#ifndef INCLUDED_TABLE
#include "table.h"    // LOOK_UP_TABLE
#define INCLUDED_TABLE
#endif
#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif
#ifndef INCLUDED_COM_CLONE
#include "com_clone.h"
#define INCLUDED_COM_CLONE
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#include <functional>
#include <boost/bind.hpp>

#ifndef INCLUDED_CALC_LIBERROR
#include "calc_liberror.h"
#define INCLUDED_CALC_LIBERROR
#endif

#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif

#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif


LOOK_UP_TABLE *calc::LookupTable::createOldStyle(
  const std::string &fileName)
{
  LOOK_UP_TABLE *t(0);
  FILE  *f = fopen(fileName.c_str(), "r");
  if (!f)
    libError("Can't open lookup table "+quote(fileName));

  try {
   std::vector<CSF_VS> keyTypes(d_keyVs.size());
   for(size_t i=0; i < d_keyVs.size(); i++)
     keyTypes[i] = vs2CsfVs(biggestVs(d_keyVs[i]));

   t = ReadLookupTable(f, &(keyTypes[0]), keyTypes.size(), vs2CsfVs(d_vs));
   if (!t) {
     // pcrcalc/test10a
     libError("while parsing lookuptable "+quote(fileName));
   }
  } catch ( ... ) {
     fclose(f);
     throw;
  }

  fclose(f);
  return t;
}


calc::LookupTable::LookupTable(
    VS outType):
    d_vs(outType)
{
}

/*! create subset with keys removed that match \a filterKeys
 */
calc::LookupTable::LookupTable(
    const LookupTable& t,
    const std::vector<bool>&   remove,
    const std::vector<double>& filterKeys):
  d_vs(t.d_vs)
{
  std::vector<size_t> keep;
  for(size_t k=0; k < t.d_keyVs.size(); ++k)
   if (!remove[k]) {
    d_keyVs.push_back(t.d_keyVs[k]);
    keep.push_back(k);
  }
  for(size_t i=0; i < t.d_records.size(); ++i)
    if (t.d_records[i].match(remove,filterKeys))
      d_records.push_back(LookupRecord(t.d_records[i],keep));
}

//! parse records from ASCII file
/*!
 * \throws com::Exception in case of error
 */
void calc::LookupTable::setRecords(
  const std::string &fileName,
  const std::vector<VS>& inKeys)
{
  d_keyVs = inKeys;
  LOOK_UP_TABLE *table(createOldStyle(fileName));

  try {
    d_records.reserve(table->nrRecords);
    for(size_t r=0; r < table->nrRecords; r++)
     d_records.push_back(LookupRecord(table->records[r],table->nrKeys));
  } catch(...) {
    FreeLookupTable(table);
    throw;
  }
  FreeLookupTable(table);
}

//! set records
void calc::LookupTable::setRecords(
    const Records& records,
    const std::vector<VS>& inKeys)
{
    d_records=records;
    d_keyVs=inKeys;
}

//! dtor
calc::LookupTable::~LookupTable()
{
}

calc::LookupRecord::LookupRecord(const Key& key, double result):
  d_result(result)
{
  for(size_t k=0; k < key.size(); k++) {
    d_key.push_back(key[k]->createClone());
  }
}

//! copy a selection of keys
calc::LookupRecord::LookupRecord(
    const LookupRecord& l,
    const std::vector<size_t>& select):
  d_result(l.d_result)
{
  for(size_t k=0; k < select.size(); k++) {
    d_key.push_back(l.d_key[select[k]]->createClone());
  }
}

calc::LookupRecord::LookupRecord(const LOOK_UP_KEY *keys,size_t nrKeys)
{
  d_key.resize(nrKeys);
  for(size_t k=0; k < nrKeys; k++) {
     const LOOK_UP_KEY *l=keys+k;
     switch (l->t) {
       case TEST_ONE    : d_key[k]= new com::EqualTo<>(l->l);
                          break;
       case TEST_INF_INF: d_key[k]= new com::AnythingInterval<>();       // infinity
                          break;
       case TEST_GE_INF : d_key[k]= new com::GreaterThanEqualTo<>(l->l);  // [l  ,inf>
                          break;
       case TEST_GT_INF : d_key[k]= new com::GreaterThan<>(l->l);         // <l  ,inf>
                          break;
       case TEST_INF_LE : d_key[k]= new com::LessThanEqualTo<>(l->h);     // <inf,h]
                          break;
       case TEST_GE_LE  : d_key[k]= new com::BetweenLimits<>(
                              com::GreaterThanEqualTo<>(l->l),
                              com::LessThanEqualTo<>(l->h));              // [l  ,h]
                          break;
       case TEST_GT_LE  : d_key[k]= new com::BetweenLimits<>(
                              com::GreaterThan<>(l->l),
                              com::LessThanEqualTo<>(l->h));              // <l  ,h]
                          break;
       case TEST_INF_LT : d_key[k]= new com::LessThan<>(l->h);            // <inf,h>
                          break;
       case TEST_GE_LT  : d_key[k]= new com::BetweenLimits<>(
                              com::GreaterThanEqualTo<>(l->l),
                              com::LessThan<>(l->h));                     // [l  ,h>
                          break;
       case TEST_GT_LT  : d_key[k]= new com::BetweenLimits<>(
                              com::GreaterThan<>(l->l),
                              com::LessThan<>(l->h));                     // <l  ,h>
                          break;
     }
   }
   POSTCOND(keys[nrKeys].t == TEST_ONE);
   d_result=keys[nrKeys].l;
}

void calc::LookupRecord::cloneKey(const Key& key)
{
  com::copyClone(key,d_key);
}

calc::LookupRecord&
calc::LookupRecord::operator=(const LookupRecord& r)
{
  if (this != &r) {
   d_result=r.d_result;
   cloneKey(r.d_key);
  }
  return *this;
}

calc::LookupRecord::LookupRecord(const LookupRecord& r):
 d_result(r.d_result)
{
   cloneKey(r.d_key);
}

//! delete all records of \a key and clear the vector
void calc::LookupRecord::deleteKey(Key& key)
{
  com::clearClone(key);
}

calc::LookupRecord::~LookupRecord()
{
  deleteKey(d_key);
}

//! match only parts of key where \a mustMatch is true
/*! if mustMatch is false then that part of the key always match
 */
bool calc::LookupRecord::match(
              const std::vector<bool>&   mustMatch,
              const std::vector<double>& key) const
{
  DEVELOP_PRECOND(d_key.size() == key.size());
  for(size_t i=0; i < d_key.size(); i++)
    if (mustMatch[i] && !d_key[i]->valid(key[i]))
      return false;
  return true;
}


//! predicate if LookupRecord matches
bool calc::LookupRecord::match(const std::vector<double>& key) const
{
  DEVELOP_PRECOND(d_key.size() == key.size());
  for(size_t i=0; i < d_key.size(); i++)
    if (!d_key[i]->valid(key[i]))
      return false;
  return true;
}

//! compare record in strcmp fashion
/*!
 * \returns 0 if \a key equals this, -1 (&lt; 0) if this is less than \a key,
 *                                   1 (&gt; 0) if this is greater than \a key
 */
int calc::LookupRecord::compare(const std::vector<double>& key) const
{
  DEVELOP_PRECOND(d_key.size() == key.size());
  for(size_t i=0; i < d_key.size(); i++) {
    if (!d_key[i]->valid(key[i])) {
        if (d_key[i]->operator<(key[i]))
          return -1;
        if (d_key[i]->operator>(key[i]))
          return  1;
    }
  }
  return 0;
}

//! try to find a key and return result
/*!
 * \param result set to result, untouched if return value is false
 * \param key    the keys to search for
 */
bool calc::LookupTable::find(double& result, const std::vector<double>& key) const
{
  PRECOND(key.size() == nrKeys());
  Records::const_iterator p = std::find_if(d_records.begin(),d_records.end(),
                   boost::bind(&calc::LookupRecord::match,_1,boost::ref(key)));
  if (p != d_records.end()) {
    result = p->result();
    return true;
  }
  return false;
}

const calc::LookupTable::Records& calc::LookupTable::records() const
{
  return d_records;
}

//! VS of value (last) column
VS calc::LookupTable::returnVs() const
{
  return d_vs;
}

//! nr of keys columns in table
size_t calc::LookupTable::nrKeys() const
{
  return d_keyVs.size();
}
