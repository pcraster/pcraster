#ifndef INCLUDED_AG_MOUSETARGET
#define INCLUDED_AG_MOUSETARGET



// Library headers.
#include <QPoint>

// PCRaster library headers.

// Module headers.



namespace ag {
  // MouseTarget declarations.
}



namespace ag {



//! The MouseTarget class is for keeping track of mouse presses and movement.
/*!
  MouseTarget objects are for information about mouse movement. If you call
  press(const QPoint&) and move(const QPoint&) when a mouse button is pressed
  and/or when the mouse is moved while a mouse button is pressed.
*/
class MouseTarget
{

private:

  //! Mouse position after mouse press.
  QPoint           d_press;

  //! Mouse position after mouse move.
  QPoint           d_move;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MouseTarget         ();

  /* virtual */    ~MouseTarget        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  
  void             initialize          ();

  void             press               (const QPoint& pos);

  void             move                (const QPoint& pos);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             moved               () const;

  const QPoint&    pressPosition       () const;

  const QPoint&    movePosition        () const;

  QPoint           movement            () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
