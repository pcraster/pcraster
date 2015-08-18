#ifndef INCLUDED_GEOM_SIZE
#define INCLUDED_GEOM_SIZE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



/*!
  \class geom_Size
  \brief The geom_Size class is a template for size classes.
  \sa    geom::Rectangle

  The difference between the geom_Size and geom::Rectangle template is the fact
  that the geom::Rectangles have coordinates while geom_Sizes don't. The
  geom_Sizes class only keep track of the width and height of an object.

  To create and use a size with int coordinates the geom_Size template needs
  to be instantiated with int as the argument:

  \code
    geom_Size<int> sizeOfMap;                      // Construct the size-object.
    sizeOfMap.setWidth(40);                        // Set the width.
    sizeOfMap.setHeight(5);                        // Set the height.
    cout << sizeOfMap.getWidth();                  // Print the width.
  \endcode
*/
template<class ValueType>
class geom_Size
{

private:

  //! Width.
  ValueType        d_width;

  //! Height.
  ValueType        d_height;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


  /*!
    \brief   Default constructor.

    Constructs a size with width and height 0 x 0.
  */
                   geom_Size          ()
    : d_width(static_cast<ValueType>(0)), d_height(static_cast<ValueType>(0)) {}

  /*!
    \brief   Constructor.
    \param   width The width.
    \param   height The height.

    Constructs a size with width and height \a width x \a height.
  */
                   geom_Size          (ValueType width,
                                       ValueType height)
    : d_width(width), d_height(height) {}

  /*!
    \brief   Destructor.
  */
                   ~geom_Size         () {}

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  /*!
    \brief   Sets the width to \a width.
    \param   width The new width.
    \sa      setHeight()
  */
  void             setWidth           (ValueType width)
  {
    d_width = width;
  }

  /*!
    \brief   Sets the height to \a height.
    \param   height The new height.
    \sa      setWidth()
  */
  void             setHeight          (ValueType height)
  {
    d_height = height;
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  /*!
    \brief   Returns the width.
    \return  The width.
    \sa      getHeight()
  */
  ValueType        getWidth           () const
  {
    return d_width;
  }

  /*!
    \brief   Returns the height.
    \return  The height.
    \sa      getWidth()
  */
  ValueType        getHeight          () const
  {
    return d_height;
  }

  /*!
    \brief   Returns the width.
    \return  The width.
    \sa      height()
  */
  ValueType        width              () const
  {
    return d_width;
  }

  /*!
    \brief   Returns the height.
    \return  The height.
    \sa      width()
  */
  ValueType        height             () const
  {
    return d_height;
  }

};

#endif
