#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTSTAT
#include "calc_aststat.h"
#define INCLUDED_CALC_ASTSTAT
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif
#ifndef INCLUDED_CALC_REPORT
#include "calc_report.h"
#define INCLUDED_CALC_REPORT
#endif



/*!
  \file
  This file contains the implementation of the ASTStat class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ASTStatPrivate
{
public:

  ASTStatPrivate()
  {
  }

  ~ASTStatPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTSTAT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASTSTAT MEMBERS
//------------------------------------------------------------------------------

void calc::ASTStat::init()
{
  d_stat=0;
  d_reportParsed=false;
  d_reportInSitu=0;
}

calc::ASTStat::ASTStat()
{
  init();
}


calc::ASTStat::ASTStat(ASTNode *stat):
  ASTNode(*stat),
  d_stat(stat)
{
  init();
}


calc::ASTStat::~ASTStat()
{
  delete d_stat;
  delete d_reportInSitu;
}

//! Assignment operator.
calc::ASTStat& calc::ASTStat::operator=(const ASTStat& rhs)
{
  if (this != &rhs) {
    d_reportParsed=rhs.d_reportParsed;
    d_reportById  =rhs.d_reportById;

    delete d_reportInSitu;
    if (rhs.d_reportInSitu)
     d_reportInSitu = rhs.d_reportInSitu->createClone();

    delete d_stat;
    d_stat = rhs.d_stat->createClone();
  }
  return *this;
}

//! Copy constructor.
calc::ASTStat::ASTStat(const ASTStat& rhs):
    ASTNode(rhs),
    d_reportParsed(rhs.d_reportParsed),
    d_reportById(rhs.d_reportById),
    d_reportInSitu(0),
    d_stat(rhs.d_stat->createClone())
{
    if (rhs.d_reportInSitu)
     d_reportInSitu = rhs.d_reportInSitu->createClone();
}

void calc::ASTStat::accept(ASTVisitor& b)
{
  b.visitStat(this);
}

calc::ASTStat* calc::ASTStat::createClone() const
{
  return new ASTStat(*this);
}

//! set value of stat
void calc::ASTStat::transferStat(ASTNode* stat)
{
  delete d_stat;
  d_stat=stat;
  // if this is the 1st part
  if (!d_reportInSitu) // no prefix
    setPosition(stat->position());
}

//! get value of stat
calc::ASTNode* calc::ASTStat::stat() const
{
  return d_stat;
}

//! set value of reportParsed
void calc::ASTStat::setReportParsed(bool reportParsed)
{
  d_reportParsed=reportParsed;
}

//! set value of reportById
void calc::ASTStat::setReportById(const Id& reportById)
{
  d_reportById=reportById;
}

//! set value of reportInSitu
void calc::ASTStat::transferReportInSitu(Report * reportInSitu)
{
  PRECOND(reportInSitu);
  delete d_reportInSitu;
  d_reportInSitu=reportInSitu;
  // this is first part of statement
  setPosition(d_reportInSitu->position());
}

//! get value of reportParsed
bool calc::ASTStat::reportParsed() const
{
  return d_reportParsed;
}

//! get value of reportById
const calc::Id& calc::ASTStat::reportById() const
{
  return d_reportById;
}

//! get value of reportInSitu
calc::Report * calc::ASTStat::reportInSitu() const
{
  return d_reportInSitu;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



