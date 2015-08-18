#include "ag_DataProperty.h"

// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_DataGuide.h"



/*!
  \file
  This file contains the implementation of the DataProperty class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATAPROPERTY MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATAPROPERTY MEMBERS
//------------------------------------------------------------------------------

ag::DataProperty::DataProperty()

  : d_enabled(true), d_selected(false)

{
}



ag::DataProperty::DataProperty(DataProperty const& dataProperty)

  : d_enabled(dataProperty.d_enabled), d_selected(dataProperty.d_selected)

{
}



/*
ag::DataProperty::DataProperty(const DataGuide& dataGuide)

  : d_data(new DataPropertyPrivate())

{
}
*/



ag::DataProperty::~DataProperty()
{
}



ag::DataProperty& ag::DataProperty::operator=(DataProperty const& rhs)
{
  if(this != &rhs) {
    d_enabled = rhs.d_enabled;
    d_selected = rhs.d_selected;
  }

  return *this;
}



void ag::DataProperty::setEnabled(bool enabled)
{
  d_enabled = enabled;
}



void ag::DataProperty::setSelected(bool selected)
{
  d_selected = selected;
}



bool ag::DataProperty::isEnabled() const
{
  return d_enabled;
}



bool ag::DataProperty::isSelected() const
{
  return d_selected;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



