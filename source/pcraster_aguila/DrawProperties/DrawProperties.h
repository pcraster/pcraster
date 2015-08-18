#ifndef INCLUDED_DRAWPROPERTIES
#define INCLUDED_DRAWPROPERTIES



// External headers.
#ifndef INCLUDED_QBRUSH
#include <QBrush>
#define INCLUDED_QBRUSH
#endif

#ifndef INCLUDED_QPEN
#include <QPen>
#define INCLUDED_QPEN
#endif

// Project headers.

// Module headers.
#ifndef INCLUDED_PALETTE
#include "Palette.h"
#define INCLUDED_PALETTE
#endif



namespace ag {
  // DrawProperties declarations.
}



namespace ag {

//! Base class for draw properties classes.
/*!
  This class contains the common properties for more specialized draw
  properties classes.

  Usage of the properties:
  - Paint a shape:
    - Pen for outline.
    - Brush to fill.
  - Line in multi-line plot:
    - Attribute pen.
  - Select color based on attribute values:
    - Color from palette.

  \sa        .
*/
class DrawProperties
{

  friend class DrawPropertiesTest;

private:

  //! Pen with which to draw lines and outlines of shapes.
  QPen             _pen;

  //! Pen with which to draw lines if they need to stand out.
  QPen             _attributePen;

  //! Brush which defines the fill pattern of shapes.
  QBrush           _brush;

  //! Range of colors to use for coloring different attribute values.
  Palette          _palette;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DrawProperties      ();

                   DrawProperties      (QPen const& pen,
                                        QPen const& attributePen,
                                        QBrush const& brush,
                                        Palette const& palette);

  virtual          ~DrawProperties     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setPen              (QPen const& pen);

  void             setAttributePen     (QPen const& pen);

  void             setBrush            (QBrush const& brush);

  void             setPalette          (Palette const& palette);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  QPen const&      pen                 () const;

  QPen const&      attributePen        () const;

  QBrush const&    brush               () const;

  Palette const&   palette             () const;

  QColor const&    color               (size_t i) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

#endif
