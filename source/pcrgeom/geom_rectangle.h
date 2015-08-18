#ifndef INCLUDED_GEOM_RECTANGLE
#define INCLUDED_GEOM_RECTANGLE

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif

namespace geom {



/*!
  \class Rectangle
  \brief The Rectangle class is a template for rectangle classes.
  \sa    geom_Size

  To create and use a rectangle with size_t coordinates the Rectangle
  template needs to be instantiated with size_t as the argument:

  \code
    Rectangle<size_t> areaOnScreen;             // Construct the rectangle.
    areaOnScreen.setRectangle(0, 0, 50, 50);    // Set coordinates of rectangle.
    std::cout << areaOnScreen.getRight();       // Print the right coordinate.
  \endcode
*/
template<class ValueType>
class Rectangle
{

private:

  //! Upper left x-coordinate.
  ValueType        d_left;

  //! Upper left y-coordinate.
  ValueType        d_top;

  //! Lower right x-coordinate.
  ValueType        d_right;

  //! Lower right y-coordinate.
  ValueType        d_bottom;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


  /*!
    \brief   Default constructor.

    Constructs a rectangle with upper left and lower right coordinates
    (0,0) x (0, 0).
  */
                   Rectangle           ()
    : d_left(static_cast<ValueType>(0)), d_top(static_cast<ValueType>(0)),
      d_right(static_cast<ValueType>(0)), d_bottom(static_cast<ValueType>(0)) {}

  
  /*!
    \brief   Constructor.
    \param   upperLeftX The left coordinate.
    \param   upperLeftY The top coordinate.
    \param   lowerRightX The right coordinate.
    \param   lowerRightY The bottom coordinate.

    Constructs a rectangle with upper left and lower right coordinates
    (\a upperLeftX, \a upperLeftY) x (\a lowerRightX, \a lowerRightY).
  */
                   Rectangle           (ValueType upperLeftX,
                                        ValueType upperLeftY,
                                        ValueType lowerRightX,
                                        ValueType lowerRightY)
    : d_left(upperLeftX), d_top(upperLeftY),
      d_right(lowerRightX), d_bottom(lowerRightY) {}


  /*!
    \brief   Constructor.
    \param   width The width of the rectangle.
    \param   height The height of the rectangle.

    Constructs a rectangle with upper left and lower right coordinates
    (0, 0) x (\a width, \a height).
  */
                   Rectangle           (ValueType width,
                                        ValueType height)
    : d_left(static_cast<ValueType>(0)), d_top(static_cast<ValueType>(0)),
      d_right(width), d_bottom(height) {}


  /*!
    \brief   Destructor.
  */
                   ~Rectangle          () {}

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  /*!
    \brief   Moves the upper left coordinate of the rectangle to
             (\a upperLeftX, \a upperLeftY).
    \param   upperLeftX The new left coordinate.
    \param   upperLeftY The new top coordinate.
  */
  void             moveUpperLeft       (ValueType upperLeftX,
                                        ValueType upperLeftY)
  {
    d_right  -= d_left - upperLeftX;
    d_bottom -= d_top - upperLeftY;
    d_left    = upperLeftX;
    d_top     = upperLeftY;
  }

  /*!
    \brief   Sets the size to \a width x \a height.
    \param   width The new width.
    \param   height The new height.

    Only the lower right coordinates are adjusted.
  */
  void             setSize             (ValueType width,
                                        ValueType height)
  {
    d_right  = d_left + width;
    d_bottom = d_top + height;
  }

  /*!
    \brief   Sets the width to \a width.
    \param   width The new width.
    \sa      setHeight(), setSize()

    Only the right coordinate is adjusted.
  */
  void             setWidth            (ValueType width)
  {
    d_right = d_left + width;
  }

  /*!
    \brief   Sets the height to \a height.
    \param   height The new height.
    \sa      setWidth(), setSize()

    Only the bottom coordinate is adjusted.
  */
  void             setHeight           (ValueType height)
  {
    d_bottom = d_top + height;
  }

  /*!
    \brief   Sets the upper left coordinates to (\a upperLeftX, \a upperLeftY)
             and the size to \a width x \a height.
    \param   upperLeftX The new left coordinate.
    \param   upperLeftY The new top coordinate.
    \param   width      The new width.
    \param   height     The new height.
    \sa      moveUpperLeft(), setSize()
  */
  void             setRectangle        (ValueType upperLeftX,
                                        ValueType upperLeftY,
                                        ValueType width,
                                        ValueType height)
  {
    moveUpperLeft(upperLeftX, upperLeftY);
    setSize(width, height);
  }

  /*!
    \brief   Sets the coordinates to the coordinates of \a rectangle.
    \param   rectangle The rectangle to copy the properties from.
  */
  void             setRectangle        (const Rectangle &rectangle)
  {
    *this = rectangle;
  }

  /*!
    \brief   Sets the left coordinate to \a left.
    \param   left The new left coordinate.
    \sa      setTop(), setRight(), setBottom()
  */
  void             setLeft             (ValueType left)
  {
    d_left = left;
  }

  /*!
    \brief   Sets the right coordinate to \a right.
    \param   left The new right coordinate.
    \sa      setBottom(), setLeft(), setTop()
  */
  void             setRight            (ValueType right)
  {
    d_right = right;
  }

  /*!
    \brief   Sets the top coordinate to \a top.
    \param   left The new top coordinate.
    \sa      setRight(), setBottom(), setLeft()
  */
  void             setTop              (ValueType top)
  {
    d_top = top;
  }

  /*!
    \brief   Sets the bottom coordinate to \a bottom.
    \param   left The new bottom coordinate.
    \sa      setLeft(), setTop(), setRight()
  */
  void             setBottom           (ValueType bottom)
  {
    d_bottom = bottom;
  }

  /*!
    \brief   Translates by \a x, \a y.
    \param   x Translation in x direction.
    \param   y Translation in y direction.
    \warning Translation can result in range errors. Consider the ValueType
             of the rectangle and \a dx and \a dy.
    \sa      moveBy()
  */
  void             translate           (ValueType x,
                                        ValueType y)
  {
    d_left   -= x;
    d_top    -= y;
    d_right  -= x;
    d_bottom -= y;
  }

  /*!
    \brief   Moves by dx, dy.
    \param   dx Move in x direction.
    \param   dy Move in y direction.
    \warning moveBy can result in range errors. Consider the ValueType of the
             rectangle and \a dx and \a dy.
    \sa      translate()
  */
  void             moveBy              (ValueType dx,
                                        ValueType dy)
  {
    d_left   += dx;
    d_right  += dx;
    d_top    += dy;
    d_bottom += dy;
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------


  /*!
    \brief   Returns the width.
    \return  The width.
    \sa      height()
  */
  ValueType        width               () const
    { return com::absolute(d_right - d_left); }

  /*!
    \brief   Returns the height.
    \return  The height.
    \sa      width()
  */
  ValueType        height              () const
    { return com::absolute(d_bottom - d_top); }

  /*!
    \brief   Returns the left coordinate.
    \return  The left coordinate.
    \sa      top(), right(), bottom()
  */
  ValueType        left                () const
    { return d_left; }

  /*!
    \brief   Returns the right coordinate.
    \return  The right coordinate.
    \sa      bottom(), left(), top()
  */
  ValueType        right               () const
    { return d_right; }

  /*!
    \brief   Returns the top coordinate.
    \return  The top coordinate.
    \sa      right(), bottom(), left()
  */
  ValueType        top                 () const
    { return d_top; }

  /*!
    \brief   Returns the bottom coordinate.
    \return  The bottom coordinate.
    \sa      left(), top(), right()
  */
  ValueType        bottom              () const
    { return d_bottom; }

};



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

template<class ValueType> bool operator==(const Rectangle<ValueType> &lhs,
                                          const Rectangle<ValueType> &rhs)
{
  return ((lhs.left() == rhs.left()) && (lhs.top() == rhs.top()) &&
          (lhs.right() == rhs.right()) && (lhs.bottom() == rhs.bottom()));
}

} // Namespace geom.

#endif

