#include "stddefx.h"
#include "calc_fieldwriter.h"
#include "com_file.h"
#include "com_exception.h"
#include "calc_iostrategy.h"
#include "calc_astsymbolinfo.h"
#include "calc_iofieldstrategy.h"
#include "calc_stackinfo.h"
#include "calc_tssoutputvalue.h"
#include <sstream>

/*!
  \file
  This file contains the implementation of the FieldWriter class.
*/


//------------------------------------------------------------------------------


namespace calc
{


class StaticWriter : public FileWriter
{
public:
  StaticWriter(const ASTSymbolInfo &symbol, const IOStrategy &fios) : FileWriter(symbol, fios)
  {
  }

  ~StaticWriter() override
  {
  }

  /*! write a field in the initial section
   * \param field     to write
   * \param timeStep  time step, 0 for initial/static write
   */
  std::string write(const Field *f, size_t timeStep) override
  {
    DEVELOP_PRECOND(!timeStep);
    if (!timeStep) {  // initial
      d_fios.writeField(outputFilePath(), f);
      return outputFilePath();
    }
    return "";
  }
};

class DynamicWriter : public FileWriter
{
protected:
  StackInfo d_stackInfo;

public:
  DynamicWriter(const ASTSymbolInfo &s, const IOStrategy &fios, bool tss) : FileWriter(s, fios)
  {
    d_stackInfo.setFlushTssAtEachTimeStep(fios.writeEachTimeStep());
    d_stackInfo.setStackName(outputFilePath());
    PRECOND(s.report());
    d_stackInfo.setReport(s.report());
    if (tss) {
      d_stackInfo.setVs(s.dataType().resultType());
    } else {
      d_stackInfo.setVs(s.vs());
    }
  }

  ~DynamicWriter() override
  {
  }

  std::string write(const Field *f, size_t timeStep) override
  {
    if (d_stackInfo.reportTimeStep(timeStep)) {
      return writeStep(f, timeStep);
    }
    return "";
  }

  virtual std::string writeStep(const Field *, size_t)
  {
    return "";
  }
};

class StackWriter : public DynamicWriter
{
public:
  StackWriter(const ASTSymbolInfo &s, const IOStrategy &fios) : DynamicWriter(s, fios, false)
  {
  }

  std::string writeStep(const Field *f, size_t timeStep) override
  {
    std::string fileName(d_fios.makeStackItemName(d_stackInfo.stackName(), timeStep));
    GridStat const s = d_fios.writeField(fileName, f);
    d_stackInfo.merge(s);
    return fileName;
  }

  void finish() override
  {
    d_fios.setStackInfo(d_stackInfo);
  }
};

class TimeoutputWriter : public DynamicWriter
{
  //! remains 0 if at first timestep id has all MV's
  TssOutputValue *d_tss{nullptr};

public:
  TimeoutputWriter(const ASTSymbolInfo &s, IOStrategy &ios) : DynamicWriter(s, ios, true)
  {
    if (s.memoryOutputId() != s.noMemoryExchangeId()) {
      d_tss = new MemoryTimeoutput(s.name(), s.memoryOutputId(), ios);
    }
  }

  ~TimeoutputWriter() override
  {
    delete d_tss;
  }

  //! tss is created the first time id contains values > 0
  void writeOutTss(const Field *id, const Field *expr, size_t timeStep) override
  {
    if (!d_tss) {
      d_tss = createFileTimeoutput(d_stackInfo, id);
    }
    if (d_tss) {
      d_tss->timeoutput(id, expr, timeStep);
    }
  }

  void finish() override
  {
    if (d_tss) {
      d_tss->finish();
    }
  }
};

//! non spatial dynamic to tss writer
class NSTssWriter : public DynamicWriter
{
  FileTimeoutput *d_tss;

public:
  NSTssWriter(const ASTSymbolInfo &s, const IOStrategy &fios) : DynamicWriter(s, fios, false)
  {
    d_tss = new FileTimeoutput(d_stackInfo, 1);
  }

  ~NSTssWriter() override
  {
    delete d_tss;
  }

  std::string write(const Field *f, size_t timeStep) override
  {
    d_tss->nonspatial(f, timeStep);
    return "";
  }

  void finish() override
  {
    d_tss->finish();
  }

  void remove() override
  {
    com::remove(d_stackInfo.stackName());
  }
};

}  // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF STATIC FIELDWRITER MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FIELDWRITER MEMBERS
//------------------------------------------------------------------------------

calc::FieldWriter::FieldWriter(const ASTSymbolInfo &symbol) : d_externalName(symbol.externalName())
{
}

calc::FieldWriter::~FieldWriter()
{
}

calc::FileWriter::FileWriter(const ASTSymbolInfo &symbol, const IOStrategy &fios)
    : FieldWriter(symbol), d_fios(fios)
{
}

calc::FileWriter::~FileWriter()
{
}

std::string calc::FileWriter::outputFilePath() const
{
  return d_fios.outputFilePath(externalName());
}

calc::MemoryWriter::MemoryWriter(const ASTSymbolInfo &symbol, const IOStrategy &mios)
    : FieldWriter(symbol), d_mios(mios)
{
}

calc::MemoryWriter::~MemoryWriter()
{
}

std::string calc::MemoryWriter::write(const Field *f, size_t)
{
  d_mios.writeField(externalName(), f);
  return "";
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::FileWriter& calc::FileWriter::operator=(const FileWriter& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::FileWriter::FileWriter(const FileWriter& rhs):
  Base(rhs)
{
}
*/

const std::string &calc::FieldWriter::externalName() const
{
  return d_externalName;
}

/*!
 * return the name under which the Field is written, empty string if not written
 */
std::string calc::FieldWriter::write(const Field *, size_t)
{
  return "";
}

void calc::FieldWriter::writeOutTss(const Field *, const Field *, size_t)
{
}

void calc::FieldWriter::finish()
{
}

void calc::FieldWriter::remove()
{
}

calc::FieldWriter *calc::IOStrategy::createFieldWriter(const ASTSymbolInfo &s)
{
  // order matters!
  if (s.vs() == VS_TSS) {
    // order matters because this also catches
    // the MemoryExchange output tss
    return new TimeoutputWriter(s, *this);
  }
  // otherwise possible Field MemoryWriter
  if (s.memoryOutputId() != s.noMemoryExchangeId()) {
    return new MemoryWriter(s, *this);
  }


  PRECOND(!s.dataType().stEither());
  bool const spatial = s.dataType().stSpatial();
  if (s.reportPosition() == RPInitial) {
    return new StaticWriter(s, *this);
  } else {
    PRECOND(s.reportPosition() == RPDynamic);
    if (spatial) {
      return new StackWriter(s, *this);
    } else {  // ns->tss
      return new NSTssWriter(s, *this);
    }
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
