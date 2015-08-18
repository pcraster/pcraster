#include "ag_MouseTarget.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the MouseTarget class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MOUSETARGET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MOUSETARGET MEMBERS
//------------------------------------------------------------------------------

//! ctor
ag::MouseTarget::MouseTarget()
{
}



//! dtor
ag::MouseTarget::~MouseTarget()
{
}



//! Initializes the object.
/*!
  After calling this function, moved() will return false.
*/
void ag::MouseTarget::initialize()
{
  d_press = QPoint();
  d_move = QPoint();
}



//! Sets the position of the mouse press.
/*!
  \param     pos Position of mouse press.
*/
void ag::MouseTarget::press(const QPoint& pos)
{
  d_press = pos;
  d_move = pos;
}



//! Sets position after mouse move.
/*!
  \param     pos Position of mouse after move.
*/
void ag::MouseTarget::move(const QPoint& pos)
{
  d_move = pos;
}



//! Returns true if the mouse has moved since the press.
/*!
  \return    true if the mouse has moved.
*/
bool ag::MouseTarget::moved() const
{
  return !(d_move - d_press).isNull();
}



//! Returns the position where the mouse was pressed.
/*!
  \return    Mouse position.
*/
const QPoint& ag::MouseTarget::pressPosition() const
{
  return d_press;
}



//! Returns the position where the mouse was moved to.
/*!
  \return    Mouse position.
*/
const QPoint& ag::MouseTarget::movePosition() const
{
  return d_move;
}



//! Returns the amount of mouse movement since the press.
/*!
  \return    Amount of mouse movement.
*/
QPoint ag::MouseTarget::movement() const
{
  return d_move - d_press;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



