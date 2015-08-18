#include "ag_LegendBody.h"
#include <QApplication>



/*!
  \file
  This file contains the implementation of the LegendBody class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

int LegendBody::d_ticLength(2);

QSize LegendBody::d_keySize(20, 10);

QSize LegendBody::d_labelOffset(5, 5);



//! Returns the length of a tic.
/*!
  \return    Length of tic.
*/
int LegendBody::ticLength()
{
  return d_ticLength;
}



//! Returns the size of a key.
/*!
  \return    Size of key.
*/
QSize const& LegendBody::keySize()
{
  return d_keySize;
}



//! Returns the offset of the labels.
/*!
  \return    Offset of labels.
*/
QSize const& LegendBody::labelOffset()
{
  return d_labelOffset;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     parent Parent.
*/
LegendBody::LegendBody(
         ViewerType type,
         QWidget* parent)

  : QWidget(parent),
    d_type(type)

{
}



//! Destructor.
/*!
*/
LegendBody::~LegendBody()
{
}



ViewerType LegendBody::viewerType() const
{
  return d_type;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag
