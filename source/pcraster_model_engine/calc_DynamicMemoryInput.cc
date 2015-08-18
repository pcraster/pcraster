#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DYNAMICMEMORYINPUT
#include "calc_DynamicMemoryInput.h"
#define INCLUDED_CALC_DYNAMICMEMORYINPUT
#endif

// External headers.
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif
#ifndef INCLUDED_CALC_DATATABLE
#include "calc_datatable.h"
#define INCLUDED_CALC_DATATABLE
#endif
#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif
#ifndef INCLUDED_CALC_IOSTRATEGY
#include "calc_iostrategy.h"
#define INCLUDED_CALC_IOSTRATEGY
#endif

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the DynamicMemoryInput class.
*/



namespace calc {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DYNAMICMEMORYINPUT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DYNAMICMEMORYINPUT MEMBERS
//------------------------------------------------------------------------------

DynamicMemoryInput::DynamicMemoryInput
   (size_t memoryIndexId,
    DataType const& dataType,
    DataTable const& dataTable,
    IOStrategy const& ioStrategy):
     d_memoryIndexId(memoryIndexId),
     d_dataType(dataType),
     d_dataTable(dataTable),
     d_ioStrategy(ioStrategy)
{
}



DynamicMemoryInput::~DynamicMemoryInput()
{
}

OVS DynamicMemoryInput::ovs() const
{
  return d_dataType.vs();
}

DataValue* DynamicMemoryInput::load()
{
  Field *f =d_dataType.stSpatial()
            ? d_ioStrategy.createSpatial(d_dataType.vs())
            : new NonSpatial(d_dataType.vs());

  f->beMemCpyDest(d_dataTable.memoryExchangeInputBuffer(d_memoryIndexId));

  return f;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace calc

