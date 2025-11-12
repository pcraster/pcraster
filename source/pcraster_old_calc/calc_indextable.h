#ifndef INCLUDED_CALC_INDEXTABLE
#define INCLUDED_CALC_INDEXTABLE

#include "calc_parameter.h"
#include "calc_arraydefvector.h"
#include "calc_vs.h"

#include <map>


namespace calc {

//! An index table can initialize/create arrayed parameters
class IndexTable : public Parameter {
 public:
  typedef struct Value {
    std::string d_value;
    int    d_lineNr{};
  } Value;
 private:
  typedef std::vector<std::string> Key;
  typedef std::pair<Key, Value>Pair;
  typedef std::map<Key, Value> Table;
  Table d_table;
  std::string err_msg;

  //! applies to this array type
   ArrayDefVector d_array;

  const Value& find(const std::string& parExtName, size_t i) const; // THROWS

  void throwError(int lineNr, std::ostringstream& what) const;
 public:
  IndexTable(
    const Symbol& name,
    const ArrayDefVector& array ); // THROWS

  // MANIPULATORS

  void addRecord(const std::vector<std::string>& line, int lineNr); // THROWS

  // ACCESSORS

  //! returns VS_INDEXTABLE
  VS symbolType() const override;

   ArrayDefVector arrayDefVector() const { return d_array; }
  std::string arrayDefName() const;

  //! read non spatial values for par, vals should be 0 size
  void fieldNrValues(const BindedSymbol& par,VS vs, std::vector<double>& vals) const; // THROWS
  //! read spatial values for par, returns Vs of input map, vals should be 0 size
  VS fieldMapValues(const BindedSymbol& par, std::vector<std::string>& vals) const; // THROWS

  //! read names that are table names
  void nameValues(const BindedSymbol& par, std::vector<const Value*>& values) const;

  void tableNameVerify(const BindedSymbol& par) const; // THROWS

  void printSpecific(InfoScript& i)const override;
};

}

#endif
