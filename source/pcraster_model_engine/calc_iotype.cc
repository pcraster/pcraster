#include "stddefx.h"
#include "calc_iotype.h"



/*!
  \file
  This file contains the implementation of the IOType class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class IOTypePrivate
{
public:

  IOTypePrivate()
  {
  }

  ~IOTypePrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOTYPE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF IOTYPE MEMBERS
//------------------------------------------------------------------------------

calc::IOType::IOType(const Input&  input, const Output& output):
  d_input(input),
  d_output(output)
{
}

calc::IOType::IOType():
  d_input(Input::None),
  d_output(Output::Fixed)
{
}

/* DEFAULT
//  Copy constructor.
calc::IOType::IOType(IOType const& rhs)

  : Base(rhs)

{
}
*/



calc::IOType::~IOType()
{
}



/* DEFAULT
//  Assignment operator.
calc::IOType& calc::IOType::operator=(IOType const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! set value of d_input
void calc::IOType::setInput(const Input& input)
{
  d_input=input;
}

//! set value of d_output
void calc::IOType::setOutput(const Output& output)
{
  d_output=output;
}

//! get value of d_input
calc::IOType::Input calc::IOType::input() const
{
  return d_input;
}

//! get value of d_output
calc::IOType::Output calc::IOType::output() const
{
  return d_output;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
