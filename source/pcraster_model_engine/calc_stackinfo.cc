#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_STACKINFO
#include "calc_stackinfo.h"
#define INCLUDED_CALC_STACKINFO
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_REPORT
#include "calc_report.h"
#define INCLUDED_CALC_REPORT
#endif



/*!
  \file
  This file contains the implementation of the StackInfo class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class StackInfoPrivate
{
public:

  StackInfoPrivate()
  {
  }

  ~StackInfoPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC STACKINFO MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF STACKINFO MEMBERS
//------------------------------------------------------------------------------

calc::StackInfo::StackInfo():
    d_flushTssAtEachTimeStep(false)
{
}



calc::StackInfo::~StackInfo()
{
}

//! Assignment operator.
calc::StackInfo& calc::StackInfo::operator=(const StackInfo& rhs)
{
  if (this != &rhs) {
    d_report=rhs.d_report;
    d_vs=rhs.d_vs;
    d_stackName=rhs.d_stackName;
    d_flushTssAtEachTimeStep=rhs.d_flushTssAtEachTimeStep;
  }
  return *this;
}

//! Copy constructor.
calc::StackInfo::StackInfo(const StackInfo& rhs):
  GridStat(rhs),
  d_report(rhs.d_report),
  d_vs(rhs.d_vs),
  d_stackName(rhs.d_stackName),
  d_flushTssAtEachTimeStep(rhs.d_flushTssAtEachTimeStep)
{
}

//! set value of d_report
void calc::StackInfo::setReport(const Report* report)
{
  d_report=report;
}

//! set value of d_vs
void calc::StackInfo::setVs(VS vs)
{
  d_vs=vs;
}

//! set value of d_stackName
void calc::StackInfo::setStackName(const std::string& stackName)
{
  d_stackName=stackName;
}

//! set value of d_flushTssAtEachTimeStep
void calc::StackInfo::setFlushTssAtEachTimeStep(bool flushTssAtEachTimeStep)
{
  d_flushTssAtEachTimeStep=flushTssAtEachTimeStep;
}


//! get value of d_vs
VS calc::StackInfo::vs() const
{
  return d_vs;
}

//! get value of d_stackName
const std::string& calc::StackInfo::stackName() const
{
  return d_stackName;
}

//! get value of d_flushTssAtEachTimeStep
bool calc::StackInfo::flushTssAtEachTimeStep() const
{
  return d_flushTssAtEachTimeStep;
}

//! get value of d_report
const calc::Report* calc::StackInfo::report() const
{
  return d_report;
}

bool calc::StackInfo::reportTimeStep(size_t timeStep) const
{
  return report()->atInt(timeStep);
}

size_t calc::StackInfo::lastInt() const
{
   return report()->lastInt();
}
size_t calc::StackInfo::startInt() const
{
   return report()->startInt();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



