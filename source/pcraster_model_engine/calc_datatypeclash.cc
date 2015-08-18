#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DATATYPECLASH
#include "calc_datatypeclash.h"
#define INCLUDED_CALC_DATATYPECLASH
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif



/*!
  \file
  This file contains the implementation of the DataTypeClash class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class DataTypeClashPrivate
{
public:

  DataTypeClashPrivate()
  {
  }

  ~DataTypeClashPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATATYPECLASH MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATATYPECLASH MEMBERS
//------------------------------------------------------------------------------

calc::DataTypeClash::DataTypeClash()
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::DataTypeClash::DataTypeClash(DataTypeClash const& rhs)

  : Base(rhs)

{
}
*/



calc::DataTypeClash::~DataTypeClash()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::DataTypeClash& calc::DataTypeClash::operator=(DataTypeClash const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

calc::TableClash::TableClash(size_t hasSize, size_t mustHaveSize)
{
  std::ostringstream msg;
   msg << "used as table with " << hasSize <<
     " columns, but has "<< mustHaveSize <<" columns";
  d_message=msg.str();
}

calc::TableClash::TableClash(size_t offendingColNr,
                             const  VSClash& c)
{
  std::ostringstream msg;
   msg << "column '"<<offendingColNr
       << "' used as " << c.mustBeOneOf() <<
     " type, but has "<<  c.isOneOf()    <<" type";
  d_message=msg.str();
}

const std::string& calc::TableClash::message() const
{
  return d_message;
}

//! throw TableClash if nr of columns does not match
void calc::TableClash::checkNrOfColumns(size_t hasSize, size_t mustHaveSize)
{
  if (hasSize != mustHaveSize)
    throw TableClash(hasSize, mustHaveSize);
}

calc::MapStackClash::MapStackClash(const std::string& message):
  d_message(message)
{
}

const std::string& calc::MapStackClash::message() const
{
  return d_message;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



