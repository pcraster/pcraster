#include "stddefx.h"
#include "calc_parpcb.h"
#include "geo_rasterspace.h"
#include "calc_field.h"
#include "calc_cellunion.h"

#include <algorithm>

/*!
  \file
  This file contains the implementation of the ParPCB class.
*/


//------------------------------------------------------------------------------

/*
namespace calc {

class ParPCBPrivate
{
public:

  ParPCBPrivate()
  {
  }

  ~ParPCBPrivate()
  {
  }

};

} // namespace calc
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC PARPCB MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF PARPCB MEMBERS
//------------------------------------------------------------------------------

calc::ParPCB::ParPCB()
{
}

/*
//! Copy constructor.
calc::ParPCB::ParPCB(ParPCB const& rhs):
  d_input (rhs.d_input),
  d_output(rhs.d_output),
 // NEW/DELETE  d_field (rhs.d_field)
{
}
*/


calc::ParPCB::~ParPCB()
{
  deleteFromPcrme(d_field);
}

/*
//! Assignment operator.
calc::ParPCB& calc::ParPCB::operator=(ParPCB const& rhs)
{
  if (this != &rhs) {
     d_input =rhs.d_input;
     d_output=rhs.d_output;
 // NEW/DELETE!    d_field =rhs.d_field;
  }
  return *this;
}
*/

calc::Field *calc::ParPCB::releaseField()
{
  Field *f = d_field;
  d_field = nullptr;
  return f;
}

//! set value of d_input
void calc::ParPCB::setInput(const ASTPar *input)
{
  d_input = input;
}

//! set value of d_output
void calc::ParPCB::setOutput(const ASTPar *output)
{
  d_output = output;
}

//! set value of d_field
void calc::ParPCB::setField(Field *field)
{
  d_field = field;
}

//! get value of d_input
const calc::ASTPar *calc::ParPCB::input() const
{
  return d_input;
}

//! get value of d_output
const calc::ASTPar *calc::ParPCB::output() const
{
  return d_output;
}

//! get value of d_field
calc::Field *calc::ParPCB::field() const
{
  return d_field;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

void calc::execPCB(std::vector<ParPCB *> &data, const void *dllFunctionAddress)
{
  typedef void (*DllFunction)(CellPtr *data, size_t n);
  auto func = (DllFunction)dllFunctionAddress;

  size_t nrValues = 1;
  for (auto &i : data) {
    nrValues = std::max<>(nrValues, i->field()->nrValues());
  }

  std::vector<CellPtr> cp(data.size());

  // set up buffers, use src() equals dest() for output
  for (size_t c = 0; c < cp.size(); ++c) {
    cp[c].v = (void *)(data[c]->field()->src());
  }

  func(&cp[0], nrValues);
}
