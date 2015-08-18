#ifndef INCLUDED_DAL_CONSTANTDRIVER
#include "dal_ConstantDriver.h"
#define INCLUDED_DAL_CONSTANTDRIVER
#endif

// External headers.

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ConstantDriver class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CONSTANTDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CONSTANTDRIVER MEMBERS
//------------------------------------------------------------------------------

ConstantDriver::ConstantDriver(
         Format const& format)

  : Driver(format)

{
}



ConstantDriver::~ConstantDriver()
{
}



bool ConstantDriver::exists(
         std::string const& name) const
{
  return exists(name, DataSpace(), DataSpaceAddress());
}



Constant* ConstantDriver::open(
         std::string const& name) const
{
  return open(name, DataSpace(), DataSpaceAddress());
}



//! This function always returns an empty data space.
/*!
  \param     name
  \param     space
  \param     address
  \return    Empty data space.
*/
DataSpace ConstantDriver::dataSpace(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  return DataSpace();
}



Constant* ConstantDriver::read(
         std::string const& name) const
{
  return read(name, DataSpace(), DataSpaceAddress());
}



void ConstantDriver::read(
         void* cell,
         TypeId
#ifdef DEBUG_BUILD
           typeId
#endif
         ,
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  assert(typeId == TI_REAL4);
  pcr::setMV(static_cast<REAL4*>(cell));
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

