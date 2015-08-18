#include "qtw_PaletteBar.h"
#include <cassert>
#include <QPainter>
#include "com_rawpalette.h"
#include "qt_ColourLib.h"



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

qtw::PaletteBar::PaletteBar(QWidget *p)

  : QWidget(p),
    d_palette(0), d_outline(false)

{
}



qtw::PaletteBar::PaletteBar(const com::RawPalette *pal,
                            QWidget *p)

  : QWidget(p),
    d_palette(pal), d_outline(false)

{
  assert(pal);

  // Convert the rgb values from the palette to QColors.
  com::RawPalette::const_iterator it;
  for(it = d_palette->begin(); it != d_palette->end(); it++)
    d_colours.push_back(qt::RgbTupleToQColor(*it, d_palette->max()));
}



qtw::PaletteBar::~PaletteBar()
{
}



void qtw::PaletteBar::setPalette(const com::RawPalette *p)
{
  assert(p);

  d_palette = p;

  // Erase current colours.
  d_colours.erase(d_colours.begin(), d_colours.end());

  // Convert the rgb values from the palette to QColors.
  com::RawPalette::const_iterator it;
  for(it = d_palette->begin(); it != d_palette->end(); it++)
    d_colours.push_back(qt::RgbTupleToQColor(*it, d_palette->max()));
}



void qtw::PaletteBar::paintEvent(QPaintEvent *)
{
  // Calculate width of rectangle to draw.
  // Draw filled rectangle.

  QPainter p(this);

  if(d_colours.size() > 0 && width() > 0 && height() > 0)
  {
    size_t xl, xr; // Left and right of rectangle to draw.
    size_t w;      // Width of rectangle to draw.
    REAL8 s;       // Scale: number of pixels available per colour.

    s  = static_cast<double>(width()) / d_colours.size();

    xl = 0;
    for(size_t i = 1; i <= d_colours.size(); i++)
    {
      xr = qRound(s * i);
      w  = (xr - xl) + 1;
      p.fillRect(xl, 0, w, height(), d_colours[i - 1]);
      xl = xr;
    }
  }

  if(d_outline)
  {
    p.setPen(Qt::black);
    p.drawRect(0, 0, width(), height());
  }
}



void qtw::PaletteBar::mousePressEvent(QMouseEvent *e)
{
  Q_EMIT mousePressed(this, e);
}



const com::RawPalette *qtw::PaletteBar::palette() const
{
  return d_palette;
}


void qtw::PaletteBar::setOutline(bool s)
{
  d_outline = s;
}



bool qtw::PaletteBar::outline() const
{
  return d_outline;
}



QSize qtw::PaletteBar::sizeHint() const
{
  return d_palette ? QSize(d_palette->nrColours(), 20) : QSize();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


