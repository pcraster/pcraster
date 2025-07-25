#include "stddefx.h"
#include "calc_stackreader.h"
#include "com_pathinfo.h"
#include "calc_iofieldstrategy.h"



/*!
  \file
  This file contains the implementation of the StackReader class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC STACKREADER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF STACKREADER MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::StackReader::StackReader(
   IoFieldStrategy *strategy,
    const std::string& stackName):
    d_strategy(strategy),
    d_stackName(stackName)
{
}

//! dtor
calc::StackReader::~StackReader()
{
}

const std::string&
 calc::StackReader::stackName() const
{ return d_stackName; }

//! return name of stack item for timestep t
std::string calc::StackReader::itemName(size_t t) const
{
  return d_strategy->makeStackItemName(stackName(),t);
}

//! is there a file for timestep t
/*!
 * checks only for existence of the file (or directory), later
 * a check is done if it is a valid  statck item
 */
bool calc::StackReader::itemExists(size_t t) const
{
  return com::pathExists(itemName(t));
}

void calc::StackReader::checkClone(size_t t) const
{
  d_strategy->checkClone(itemName(t));
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
