#ifndef INCLUDED_CALC_LOOKUPTABLEPARAMETER
#define INCLUDED_CALC_LOOKUPTABLEPARAMETER

#include "calc_subparameter.h"

#include <vector>
#include <string>



namespace pcrxml {
  class Data;
}

namespace calc {

class ParsPar;
class IndexTable;
class LookupTable;

//! holds a lookup table
/*! a lookup table parameter is always constant, and initiated with
 *  a constant value since we can not yet create tables in pcrcalc
 */
class  LookupTableParameter : public SubParameter {
 private:
  std::vector<LookupTable *>  d_vals;
  /*! this table is not uniquely owned by me
   *  symboltable will clean this one
   */
  const IndexTable *d_table;
 public:
  // CREATORS

  //! construct with table names from an index table, not known table values
  LookupTableParameter(const ParsPar& par,
             const IndexTable *table);

  //! construct from known values
  LookupTableParameter(
    const ParsPar& par,
    const std::vector<LookupTable *>& val);

  ~LookupTableParameter() override;

  // ACCESSORS
  VS symbolType() const override;
  LookupTable *value(size_t i);
  //! load values from the index table, now we know the type
  void loadValuesFromIndexTable(VS result,
        const std::vector<VS>& readKeys) /* THROW (StrErrorExcep)*/;

  void setDataSubType(pcrxml::Data *d) const override;
};


}

#endif
