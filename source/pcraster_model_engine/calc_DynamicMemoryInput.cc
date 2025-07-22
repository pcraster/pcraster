#include "stddefx.h"
#include "calc_DynamicMemoryInput.h"
#include "calc_field.h"
#include "calc_datatype.h"
#include "calc_datatable.h"
#include "calc_nonspatial.h"
#include "calc_iostrategy.h"


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

