#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DATATYPECHANGER
#include "calc_datatypechanger.h"
#define INCLUDED_CALC_DATATYPECHANGER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif


/*!
  \file
  This file contains the implementation of the DataTypeChanger class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class DataTypeChangerPrivate
{
public:

  DataTypeChangerPrivate()
  {
  }

  ~DataTypeChangerPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATATYPECHANGER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATATYPECHANGER MEMBERS
//------------------------------------------------------------------------------

calc::DataTypeChanger::DataTypeChanger():
  d_nrChanges(0)
{}


/* NOT IMPLEMENTED
//! Copy constructor.
calc::DataTypeChanger::DataTypeChanger(DataTypeChanger const& rhs)

  : Base(rhs)

{
}
*/



calc::DataTypeChanger::~DataTypeChanger()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::DataTypeChanger& calc::DataTypeChanger::operator=(DataTypeChanger const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

void calc::DataTypeChanger::incr(size_t nrNewChanges)
{
  d_nrChanges+=nrNewChanges;
}

//! update the DataType, register changes
void calc::DataTypeChanger::update(
          DataType& toUpdate,
    const DataType& from)
{
  if (toUpdate!=from)
    d_nrChanges++;
  toUpdate=from;
}

void calc::DataTypeChanger::restrict(
          DataType& toUpdate,
    const DataType& restrict)
{
  DataType newValue(toUpdate);
  newValue.restrict(restrict);
  update(toUpdate,newValue);
}


//! get value of d_nrChanges
size_t calc::DataTypeChanger::nrChanges() const
{
  return d_nrChanges;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



