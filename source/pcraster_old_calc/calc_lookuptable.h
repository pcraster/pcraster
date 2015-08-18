#ifndef INCLUDED_CALC_LOOKUPTABLE
#define INCLUDED_CALC_LOOKUPTABLE

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

#ifndef INCLUDED_COM_INTERVAL
#include "com_interval.h"
#define INCLUDED_COM_INTERVAL
#endif
struct LOOK_UP_TABLE;
struct LOOK_UP_KEY;

namespace calc {

/*!
 * \todo
 *   check dangerouse code 
 *   double(0.2) =! float(0.2) in com_intervaltest.cc
 *   heeft dat hier ook gevolgen?
 */
class LookupRecord {
public:
 typedef std::vector<com::IntervalD* > Key;
 static void deleteKey(Key& key);
private:
 void cloneKey(const Key& key);
 Key    d_key;
 double d_result;

public:
 LookupRecord&           operator=           (const LookupRecord&);
 LookupRecord                                (const LookupRecord&);

 LookupRecord(const Key& key, double result);
 LookupRecord(const LOOK_UP_KEY *keys,size_t nrKeys);
 LookupRecord(const LookupRecord& l, const std::vector<size_t>& select);
~LookupRecord();

 bool match(const std::vector<double>& key) const;
 bool match(const std::vector<bool>& mustMatch,
            const std::vector<double>& key) const;

 int  compare(const std::vector<double>& key) const;
 double result() const {
   return d_result;
 }
 const com::IntervalD* interval(size_t i) const {
   PRECOND(i < d_key.size());
   return d_key[i];
 }
 size_t nrKeys() const {
   return d_key.size();
 }
};

//! a lookup table as used by the lookup...() functions
/*!
 * \todo
 *   voeg sequence nr toe aan keys, sorteer keys incl.
 *   seq.nr, dan lower_bound or equal_range: Orde van
 *   voorkomen in table is dan meegenomen.
 */
class LookupTable {
  public:
   typedef std::vector<LookupRecord>             Records;

   const Records& records() const;

  private:
   Records                                       d_records;
   std::vector<VS>                               d_keyVs;

  //! result vs, as known from func name: lookup<i>vs</i>(...)
  VS                      d_vs;

  //! create with lookup table from table C-library
  LOOK_UP_TABLE          *createOldStyle(const std::string &fileName);
 public:
  LookupTable(VS outType);

  LookupTable(const LookupTable& t,
              const std::vector<bool>&   filter,
              const std::vector<double>& filterKeys);

  void setRecords(
   const std::string &fileName,
   const std::vector<VS>& readKeys);

  void setRecords(
    const Records& records,
    const std::vector<VS>& inKeys);
  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  virtual ~LookupTable();
  // ACCESSORS
  size_t nrKeys() const;
  VS  returnVs() const;

  virtual bool find(double& result, const std::vector<double>& key) const;
};


}

#endif
