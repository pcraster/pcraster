#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DISKWRITTENFIELD
#include "calc_diskwrittenfield.h"
#define INCLUDED_CALC_DISKWRITTENFIELD
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_IOSTRATEGY
#include "calc_iostrategy.h"
#define INCLUDED_CALC_IOSTRATEGY
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

/*!
  \file
  This file contains the implementation of the DiskWrittenField class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class DiskWrittenFieldPrivate
{
public:

  DiskWrittenFieldPrivate()
  {
  }

  ~DiskWrittenFieldPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DISKWRITTENFIELD MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DISKWRITTENFIELD MEMBERS
//------------------------------------------------------------------------------

namespace calc {

DiskWrittenField::DiskWrittenField(
                   const IOStrategy&  ios,
                   const std::string& fileName,
                   VS                 vs):
  d_ios(ios),
  d_fileName(fileName),
  d_vs(vs)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
DiskWrittenField::DiskWrittenField(
         DiskWrittenField const& rhs)

  : Base(rhs)

{
}
*/



DiskWrittenField::~DiskWrittenField()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
DiskWrittenField& DiskWrittenField::operator=(
         DiskWrittenField const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

OVS DiskWrittenField::ovs() const
{
  return d_vs;
}

DataValue* DiskWrittenField::load()
{
  return d_ios.createReadSpatial(d_fileName,d_vs);
}


} // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



