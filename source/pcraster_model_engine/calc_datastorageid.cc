#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DATASTORAGEID
#include "calc_datastorageid.h"
#define INCLUDED_CALC_DATASTORAGEID
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the DataStorageId class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class DataStorageIdPrivate
{
public:

  DataStorageIdPrivate()
  {
  }

  ~DataStorageIdPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASTORAGEID MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATASTORAGEID MEMBERS
//------------------------------------------------------------------------------

calc::DataStorageId::DataStorageId(const std::string& id):
  d_id(id)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::DataStorageId::DataStorageId(DataStorageId const& rhs)

  : Base(rhs)

{
}
*/



calc::DataStorageId::~DataStorageId()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::DataStorageId& calc::DataStorageId::operator=(DataStorageId const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! set value of d_id
void calc::DataStorageId::setId(const std::string& id)
{
  d_id=id;
}

//! get value of d_id
const std::string& calc::DataStorageId::id() const
{
  return d_id;
}

calc::OVS calc::DataStorageId::ovs() const
{
  return VS_STRING;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



