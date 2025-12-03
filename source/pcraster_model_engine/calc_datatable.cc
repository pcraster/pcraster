#include "stddefx.h"
#include "calc_datatable.h"
#include "com_exception.h"
#include "calc_timetable.h"
#include "calc_lookuptable.h"
#include "calc_field.h"
#include "calc_spatial.h"
#include "calc_nonspatial.h"
#include "calc_stackinput.h"
#include "calc_DynamicMemoryInput.h"
#include "calc_datavalue.h"

#include <memory>

/*!
  \file
  This file contains the implementation of the DataTable class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATATABLE MEMBERS
//------------------------------------------------------------------------------

bool calc::DataTable::d_useDiskStorage = true;

//------------------------------------------------------------------------------
// DEFINITION OF DATATABLE MEMBERS
//------------------------------------------------------------------------------

calc::DataTable::DataTable()

{
}

calc::DataTable::~DataTable()
{
  clean();
}

//! delete all DataValue's, set to 0
void calc::DataTable::clean()
{
  for (auto &i : d_table) {
    deleteAlways(i.second.d_dv);
    i.second.d_dv = nullptr;
  }
  d_table.clear();
  d_memoryInputLookupTables.clear();
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::DataTable& calc::DataTable::operator=(const DataTable& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::DataTable::DataTable(const DataTable& rhs):
  Base(rhs)
{
}
*/

bool calc::DataTable::contains(const std::string &name) const
{
  auto i(d_table.find(name));
  return i != d_table.end();
}

/*
 *  \pre DataTable::contains(name)
 */
const calc::DataValue *calc::DataTable::operator[](const std::string &name) const
{
  auto i(d_table.find(name));
  POSTCOND(i != d_table.end());
  return i->second.d_dv;
}

/*!
 * \pre
 *   \a name is part of DataTable
 */
calc::DataTable::DTE calc::DataTable::dataLoad(const std::string &name)
{
  auto i(d_table.find(name));
  PRECOND(i != d_table.end());
  return {i};
}

//! insert symbols in data table
/*!
 *  If the symbol \a i is already insert as i.name() then nothing is done, as in std::map::insert()
 *
 *  Constant DataValue's such as LookupTable and InTss are  also read here.
 *  Spatial's are not, that is where StackedValue is meant for.
 *  Parsing Mb's of timeseries here is OK,
 *  just before executing; script is assumed to be correct.
 */
void calc::DataTable::insert(const ASTSymbolInfo &i, size_t nrTimeStepsExpected, const IOStrategy &ios)
{
  if (d_table.count(i.name())) {
    return;
  }

  std::unique_ptr<DataValue> dv;
  try {
    switch (i.ovs()) {
      case VS_TABLE: {
        auto *lt(new LookupTable(i));
        dv.reset(lt);
        if (i.memoryInputId() != i.noMemoryExchangeId()) {
          d_memoryInputLookupTables.push_back(i.name());
        }
      } break;
      case VS_TSS:
        if (i.memoryOutputId() == i.noMemoryExchangeId()) {
          dv = std::make_unique<TimeTable>(i, nrTimeStepsExpected);
        }
        break;
      case VS_MAPSTACK:
        dv = std::make_unique<StackInput>(*(i.stackInput()));
        break;
      case VS_STATISTICS:  // StatTable ID
        break;
      case VS_OBJECT:  // DO NOT INSERT!
        return;
      default:  // assume field/map/constant
        PRECOND(isIn(i.ovs(), VS_FIELD));
        if (i.isConstant()) {
          dv = std::make_unique<NonSpatial>(i.ovs(), i.constantValue());
        } else {
          if (i.memoryInputId() != i.noMemoryExchangeId()) {
            if (i.ioType().input() == pcrxml::ModelInputType::Dynamic) {
              dv = std::make_unique<DynamicMemoryInput>(i.memoryInputId(), i.dataType(), *this, ios);
            }
          }
          // else
          //  Entry::d_dv is 0, StackedValue does reading
        }
    }
    if (dv.get()) {
      dv->setReadOnlyReference(true);
    }
    d_table.insert(std::make_pair(i.name(), Entry(i, dv.release())));

  } catch (const com::Exception &e) {
    // some read error
    i.throwAtFirst(e);
  }
}

/*! \brief Load  inputs for memory lookuptable
 *
 */
void calc::DataTable::setMemoryExchangeInputData(void **memoryExchangeData)
{
  d_memoryExchangeData = memoryExchangeData;

  for (std::string const &name : d_memoryInputLookupTables) {
    DTE e(dataLoad(name));
    size_t const dataIndex(e.symbol().memoryInputId());
    PRECOND(dataIndex != e.symbol().noMemoryExchangeId());
    DataValue *dv = e.getOrReleaseValue(false);
    auto *lu(dynamic_cast<LookupTable *>(dv));
    try {
      lu->setArrayValue(d_memoryExchangeData[dataIndex]);
    } catch (std::range_error const &error) {
      e.symbol().throwAtFirst(com::Exception(error.what()));
    }
  }
}

//! get the memory exchange buffer of position \a memoryIndex
void *calc::DataTable::memoryExchangeInputBuffer(size_t memoryIndex) const
{
  PRECOND(d_memoryExchangeData);
  return d_memoryExchangeData[memoryIndex];
}

void calc::DataTable::print(std::ostream &s) const
{
  auto i = d_table.begin();
  for (; i != d_table.end(); ++i) {
    s << "symbol: " << i->second << " dataValue: " << i->second.d_dv << '\n';
  }
}

//! do all assignable data items hold no value?
/*!
 * this is the normal case for a ManagedScript, setLastUse
 * calc::setLastUse(cfg,false) will anotate a CFG use as such
 * that after execution everything is deleted except the constants
 */
bool calc::DataTable::allNoValue() const
{
  for (const auto &i : d_table) {
    size_t const N = ASTSymbolInfo::noMemoryExchangeId();
    if (i.second.d_dv && !i.second.isConstant() && i.second.memoryInputId() == N &&
        i.second.memoryOutputId() == N) {
      // has a datavalue that is  not a constant or a memory exchange
      // both constants and  memory exchange id or NOT considered a value when checking post execution
      return false;
    }
  }
  return true;
}

//! get the value, release iff lastUse
/*!
 * \param lastUse is this value for the last time used here
 *
 * if lastUse is true then value is detached from this and returned
 * with a readOnlyReference==false.
 *
 */
calc::DataValue *calc::DataTable::DTE::getOrReleaseValue(bool lastUse)
{
  if (!dataValue()) {
    return nullptr;  // not yet loaded
  }

  DataValue *dv(dataValue());
  if (lastUse) {
    // lastUse: release value to caller,
    //  this will have d_value reset to 0
    dv->setReadOnlyReference(false);
    dataValue() = nullptr;
  }
  // BEGIN HACKED_UP
  else if (DataTable::d_useDiskStorage && symbol().ioType().input() != pcrxml::ModelInputType::None) {
    auto *s = dynamic_cast<Spatial *>(dv);
    if (s) {
      dv->setReadOnlyReference(false);
      dataValue() = nullptr;
    }
  }
  // END HACKED_UP
  return dv;
}

//! reset the value
/*!
 * as auto_ptr<>::reset, delete old value and set to \a value
 */
void calc::DataTable::DTE::resetValue(DataValue *value)
{
  if (value->readOnlyReference()) {
    // deref  pcrcalc380
    // Simple assignments as in tmp.res=inp1s.map;
    // may cause this.
    // Someone else owns, create a copy
    // FTTB only fields can be assigned, hence reset
    auto *f = dynamic_cast<Field *>(value);
    POSTCOND(f);
    value = f->createClone();
  }

  if (value != dataValue()) {
    value->setReadOnlyReference(true);
    deleteAlways(dataValue());
    dataValue() = value;
  }
}

//! return all symbols
/*!
  needed for:
   - RtTypeCheckPrivate ctor see Python/Todo wiki
   - IOStrategy::setMemoryExchangeData
 */
std::map<std::string, calc::ASTSymbolInfo> calc::DataTable::symbols() const
{
  std::map<std::string, calc::ASTSymbolInfo> s;
  for (const auto &i : d_table) {
    s.insert(std::make_pair(i.first, i.second));
  }
  return s;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

std::ostream &calc::operator<<(std::ostream &s, const calc::DataTable &d)
{
  d.print(s);
  return s;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
