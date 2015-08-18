#ifndef INCLUDED_CALC_DATATABLE
#define INCLUDED_CALC_DATATABLE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTSYMBOLINFO
#include "calc_astsymbolinfo.h"
#define INCLUDED_CALC_ASTSYMBOLINFO
#endif


namespace calc {
  // DataTable declarations.
  class DataValue;
  class IOStrategy;
}



namespace calc {

class Field;

//! Table holding the data used in the execution of commands
/*!
    The table is populated by insert(). Every symbol is inserted in the
    table, where outputs get an initial Entry::d_dv of 0.
 */
class DataTable
{
private:
  struct Entry: ASTSymbolInfo {
    DataValue    *d_dv;
    Entry(ASTSymbolInfo const& si,
          DataValue*           dv):
      ASTSymbolInfo(si),
      d_dv(dv) {
     }
  };
  typedef std::map<std::string, Entry> Table;
  typedef Table::iterator              iterator;

public:
  //! wrap an iterator as a DataTable entry
  /*!
   * the dataValue is modifiable, to (re)set value in
   * DataTable;
   */
  class DTE {
     iterator d_it;
     // only DataTable::dataLoad  constructs this
     friend class DataTable;
     DTE(iterator it):
       d_it(it)
     {}
    public:
     DataValue*& dataValue() {
       return d_it->second.d_dv;
     }
     const ASTSymbolInfo& symbol() const {
       return d_it->second;
     }
     DataValue *getOrReleaseValue(bool lastUse);
     void   resetValue(DataValue *value);
  };

private:

  //! Assignment operator. NOT IMPLEMENTED.
  DataTable&           operator=           (const DataTable& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DataTable               (const DataTable& rhs);


  Table                    d_table;
  std::vector<std::string> d_memoryInputLookupTables;

  //! reference to raw user's DataTransferArray, updated at each timestep
  /*! Here, in DataTable it is only used to retrieve the inputs,
      Outputs are handled by the RunTimeEnv.
   */
  void**                   d_memoryExchangeData;

public:
  static bool      d_useDiskStorage;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataTable               ();

  /* virtual */    ~DataTable              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             clean                   ();
  void             insert                  (ASTSymbolInfo const& i,
                                            size_t nrTimeStepsExpected,
                                            IOStrategy const& ios);
  DTE              dataLoad                (const std::string& name);
  void             setMemoryExchangeInputData(void **data);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void*            memoryExchangeInputBuffer(size_t memoryIndex) const;
  const DataValue* operator[]              (const std::string& name) const;
  bool             contains                (const std::string& name) const;
  void             print                   (std::ostream& s        ) const;
  bool             allNoValue              ()                        const;

  std::map<std::string,ASTSymbolInfo> symbols () const;
};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------


std::ostream &operator<<(std::ostream& s, const calc::DataTable& d);

} // namespace calc

#endif
