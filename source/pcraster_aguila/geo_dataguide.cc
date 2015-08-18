#include "geo_dataguide.h"



/*!
  \file
  This file contains the implementation of the DataGuide class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------



//! Default conststructor.
/*!
  This constructor creates an invalid DataGuide object. Use it only as a
  placeholder for variables you want to assign to:

  \code
  DataGuide foo() {

    DataGuide dg;

    if(a) {
      dg = createDataGuide(b);
    }
    else if(c) {
      dg = createDataGuide(d);
    }

    return dg;
  }
  \endcode
*/
geo::DataGuide::DataGuide()

  : d_index(), d_address(0), d_type(DT_INVALID), d_valueScale(VS_UNDEFINED)

{
}



//! Constructor.
/*!
  \param     index Index of data.
  \param     address Address of data.
  \param     type Data type.
  \param     valueScale Value scale.
*/
geo::DataGuide::DataGuide(
         size_t index,
         const void* address,
         DataType type,
         CSF_VS valueScale)

  : d_index(new com::RCSize_t(index)),
    d_address(address),
    d_type(type),
    d_valueScale(valueScale)

{
}



//! Destructor.
/*!
*/
geo::DataGuide::~DataGuide()
{
  // d_index is reference-counted and will be deleted automagically.
}



//! Sets the index to \a i.
/*!
  \param     index New index.
  \warning   Be sure about what you are doing. Setting an index should only
             be done by code close to the actual collection of data.
*/
void geo::DataGuide::setIndex(
         size_t index)
{
  d_index->setValue(index);
}



//! Returns the index.
/*!
  \return    Index.
*/
size_t geo::DataGuide::index() const
{
  return d_index->value();
}



//! Returns the address of the data.
/*!
  \return    Address.
*/
geo::DataGuide::Address geo::DataGuide::address() const
{
  return d_address;
}



//! Returns the data type.
/*!
  \return    Data type.
*/
geo::DataType geo::DataGuide::type() const
{
  return d_type;
}



//! Returns the value scale.
/*!
  \return    Value scale.
  \sa        isRangeData()
*/
CSF_VS geo::DataGuide::valueScale() const
{
  return d_valueScale;
}



//! Returns true if the guide is related to range data.
/*!
  \return    true or false.
  \sa        valueScale()
*/
bool geo::DataGuide::isRangeData() const
{
  return valueScale() == VS_SCALAR || valueScale() == VS_DIRECTION;
}



//! Returns if *this equals \a g.
/*!
  \param     dataGuide Data guide to compare.
  \return    true if *this is equal to \a dataGuide.
*/
bool geo::DataGuide::equals(
         DataGuide const& dataGuide) const
{
  return d_index->value() == dataGuide.d_index->value() &&
         d_address == dataGuide.d_address &&
         d_type == dataGuide.d_type && d_valueScale == dataGuide.d_valueScale;
}



//! Returns if *this is a valid object.
/*!
  \return    true if the object is valid.
  \warning   Use invalid objects only as placeholders for variables you assign
             valid DataGuide's to.

  Invalid DataGuide objects are created by the default constructor.
*/
bool geo::DataGuide::isValid() const
{
  return d_index && d_address && d_type != DT_INVALID;
    /* && d_valueScale != VS_UNDEFINED; */
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

/*!
  \fn        bool geo::operator==(const DataGuide& lhs, const DataGuide& rhs)
  \relates   DataGuide
  \brief     Equality operator.
  \return    true if \a lhs equals \a rhs.
*/
bool geo::operator==(const DataGuide& lhs, const DataGuide& rhs)
{
  return lhs.equals(rhs);
}



/*!
  \fn        bool geo::operator!=(const DataGuide& lhs, const DataGuide& rhs)
  \relates   DataGuide
  \brief     Inequality operator.
  \return    true if \a lhs does not equal \a rhs.
*/
bool geo::operator!=(const DataGuide& lhs, const DataGuide& rhs)
{
  return !lhs.equals(rhs);
}



bool geo::operator<(
         DataGuide const& lhs,
         DataGuide const& rhs)
{
  return lhs.address() < rhs.address();
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


