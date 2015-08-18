#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DATAVALUE
#include "calc_datavalue.h"
#define INCLUDED_CALC_DATAVALUE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the DataValue class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class DataValuePrivate
{
public:

  DataValuePrivate()
  {
  }

  ~DataValuePrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATAVALUE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATAVALUE MEMBERS
//------------------------------------------------------------------------------

calc::DataValue::DataValue():
  d_readOnlyReference(false),
  d_pcrmeManaged(true)
{
}

calc::DataValue::~DataValue()
{
  // outside PCRasterModelEngine, no one sets this to true
//  PRECOND(!d_readOnlyReference);
}

/* DEFAULT
//! Assignment operator.
calc::DataValue& calc::DataValue::operator=(const DataValue& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. DEFAULT
calc::DataValue::DataValue(const DataValue& rhs):
  Base(rhs)
{
}
*/

/*! load the object if needed
 *  This method can do anything the object deems neccessary be ready
 *  for execution.
 */
calc::DataValue* calc::DataValue::load()
{
  return this;
}

//! set value of d_readOnlyReference
void calc::DataValue::setReadOnlyReference(bool readOnlyReference)
{
  d_readOnlyReference=readOnlyReference;
}

//! get value of d_readOnlyReference
bool calc::DataValue::readOnlyReference() const
{
  return d_readOnlyReference;
}

//! set value of d_pcrmeManaged
void calc::DataValue::setPcrmeManaged(bool pcrmeManaged)
{
  d_pcrmeManaged=pcrmeManaged;
}

//! get value of d_pcrmeManaged
bool calc::DataValue::pcrmeManaged() const
{
  return d_pcrmeManaged;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

/*! delete conditionally
 */
void calc::deleteFromPcrme(const DataValue *dv) {
  if (!dv)
    return;
  if (dv->pcrmeManaged()) {
    if (!dv->readOnlyReference())
      delete dv;
  } else {
    // outside pcrme readOnlyReference is always false
    // so it can be deleted.
    DataValue *unmanage=const_cast<DataValue *>(dv);
    unmanage->setReadOnlyReference(false);
  }
}

//! always delete, even if readOnlyReference is true
void calc::deleteAlways(DataValue *dv) {
  if (dv) {
   PRECOND(dv->pcrmeManaged());
   dv->setReadOnlyReference(false);
   delete dv;
  }
}


