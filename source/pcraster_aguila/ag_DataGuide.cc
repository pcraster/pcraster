#include "ag_DataGuide.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the DataGuide class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATAGUIDE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATAGUIDE MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \sa        geo::DataGuide()

  The data guide will be un selected and visible.
*/
ag::DataGuide::DataGuide()

  : geo::DataGuide()
    // d_selected(false), d_visible(true)

{
}



//! Constructor.
/*!
  \sa        geo::DataGuide(size_t, Address, DataType, CSF_VS)

  The data guide will be un selected and visible.
*/
ag::DataGuide::DataGuide(
         size_t index,
         Address address,
         geo::DataType type,
         CSF_VS valueScale)

  : geo::DataGuide(index, address, type, valueScale)
    // d_selected(false), d_visible(true)

{
}



//! Destructor.
/*!
*/
ag::DataGuide::~DataGuide()
{
}



//! Selects the data guide.
/*!
  \sa        unSelect(), setSelected(bool), isSelected()
*/
/*
void ag::DataGuide::select()
{
  d_selected = true;
}
*/



//! Un selects the data guide.
/*!
  \sa        select(), setSelected(bool), isSelected()
*/
/*
void ag::DataGuide::unSelect()
{
  d_selected = false;
}
*/



//! Sets the selection mode to \a select.
/*!
  \param     select Selection mode.
  \sa        select(), unSelect(), isSelected()
*/
/*
void ag::DataGuide::setSelected(bool select)
{
  d_selected = select;
}
*/



//! Sets the visibility to \a visible.
/*!
  \param     visible Visibility.
  \sa        isVisible()
*/
/*
void ag::DataGuide::setVisible(bool visible)
{
  d_visible = visible;
}
*/



//! Returns true if this data guide is selected.
/*!
  \return    true or false.
  \sa        select(), unSelect(), setSelected(bool)
*/
/*
bool ag::DataGuide::isSelected() const
{
  return d_selected;
}
*/



//! Returns the visibility.
/*!
  \return    true or false.
  \sa        setVisible(bool)
*/
/*
bool ag::DataGuide::isVisible() const
{
  return d_visible;
}
*/



//! Returns if *this equals \a aDataGuide.
/*!
  \param     aDataGuide Data guide to compare.
  \return    true if *this is equal to \a aDataGuide.
*/
bool ag::DataGuide::equals(const DataGuide& aDataGuide) const
{
  return static_cast<const geo::DataGuide&>(*this) ==
         static_cast<const geo::DataGuide&>(aDataGuide);
/*
         d_selected == aDataGuide.d_selected &&
         d_visible == aDataGuide.d_visible;
*/
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

/*!
  \fn        bool ag::operator==(const DataGuide& lhs, const DataGuide& rhs)
  \relates   DataGuide
  \brief     Equality operator.
  \return    true if \a lhs equals \a rhs.
*/
bool ag::operator==(const DataGuide& lhs, const DataGuide& rhs)
{
  return lhs.equals(rhs);
}



/*!
  \fn        bool ag::operator!=(const DataGuide& lhs, const DataGuide& rhs)
  \relates   DataGuide
  \brief     Inequality operator.
  \return    true if \a lhs does not equal \a rhs.
*/
bool ag::operator!=(const DataGuide& lhs, const DataGuide& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



