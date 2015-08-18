#include "com_rawpalette.h"
#include <cassert>
#include <vector>
#include "csftypes.h"

// #ifndef INCLUDED_COM_BASICTABLE
// #include "com_basictable.h"
// #define INCLUDED_COM_BASICTABLE
// #endif

// #ifndef INCLUDED_COM_EXCEPTION
// #include "com_exception.h"
// #define INCLUDED_COM_EXCEPTION
// #endif
#include "com_b2rmpalette.h"
#include "com_b2wpalette.h"
#include "com_b2wmpalette.h"
#include "com_b2ypalette.h"
#include "com_b2ympalette.h"
#include "com_boolpalette.h"
#include "com_classpalette.h"
#include "com_contpalette.h"
#include "com_g2rmpalette.h"
#include "com_graydirectpalette.h"
#include "com_netscapepalette.h"
#include "com_p2rmpalette.h"
#include "com_r2gpalette.h"
#include "com_r2gmpalette.h"
#include "com_r2ppalette.h"
#include "com_r2pmpalette.h"
#include "com_rainbowpalette.h"
#include "com_w2bpalette.h"
#include "com_w2bmpalette.h"
#include "com_y2bpalette.h"
#include "com_y2bmpalette.h"



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

const com::RawPalette* com::RawPalette::COLOURCLASSPALETTE()
{
  static RawPalette palette(classPalette, classPaletteSize, classColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::COLOURBOOLPALETTE()
{
  static RawPalette palette(boolPalette, boolPaletteSize, boolColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::B2WPALETTE()
{
  static RawPalette palette(b2wPalette, b2wPaletteSize, b2wColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::B2WMPALETTE()
{
  static RawPalette palette(b2wmPalette, b2wmPaletteSize, b2wmColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::W2BPALETTE()
{
  static RawPalette palette(w2bPalette, w2bPaletteSize, w2bColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::COLOURCONTPALETTE()
{
  static RawPalette palette(contPalette, contPaletteSize, contColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::RAINBOWPALETTE()
{
  static RawPalette palette(rainbowPalette, rainbowPaletteSize,
              rainbowColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::R2PPALETTE()
{
  static RawPalette palette(r2pPalette, r2pPaletteSize, r2pColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::R2PMPALETTE()
{
  static RawPalette palette(r2pmPalette, r2pmPaletteSize, r2pmColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::P2RMPALETTE()
{
  static RawPalette palette(p2rmPalette, p2rmPaletteSize, p2rmColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::B2YPALETTE()
{
  static RawPalette palette(b2yPalette, b2yPaletteSize, b2yColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::Y2BPALETTE()
{
  static RawPalette palette(y2bPalette, y2bPaletteSize, y2bColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::Y2BMPALETTE()
{
  static RawPalette palette(y2bmPalette, y2bmPaletteSize, y2bmColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::GRAYDIRECTPALETTE()
{
  static RawPalette palette(grayDirectPalette, grayDirectPaletteSize,
              grayDirectColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::W2BMPALETTE()
{
  static RawPalette palette(w2bmPalette, w2bmPaletteSize, w2bmColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::R2GPALETTE()
{
  static RawPalette palette(r2gPalette, r2gPaletteSize, r2gColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::R2GMPALETTE()
{
  static RawPalette palette(r2gmPalette, r2gmPaletteSize, r2gmColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::G2RMPALETTE()
{
  static RawPalette palette(g2rmPalette, g2rmPaletteSize, g2rmColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::B2RMPALETTE()
{
  static RawPalette palette(b2rmPalette, b2rmPaletteSize, b2rmColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::B2YMPALETTE()
{
  static RawPalette palette(b2ymPalette, b2ymPaletteSize, b2ymColourMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::netscapePalette()
{
  static RawPalette palette(netscapePaletteColours, netscapePaletteSize,
              netscapePaletteMaxVal);
  return &palette;
}



const com::RawPalette* com::RawPalette::booleanPalette()
{
  return COLOURBOOLPALETTE();
}



const com::RawPalette* com::RawPalette::nominalPalette()
{
  return COLOURCLASSPALETTE();
}



const com::RawPalette* com::RawPalette::ordinalPalette()
{
  return RAINBOWPALETTE();
}



const com::RawPalette* com::RawPalette::scalarPalette()
{
  return RAINBOWPALETTE();
}



const com::RawPalette* com::RawPalette::directionalPalette()
{
  return W2BMPALETTE();
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

com::RawPalette::RawPalette()

  : d_max(0)

{
}



/*!
  \param     p One dimensional array with rgb tuples.
  \param     n The number of rgb tuples in \t. This should be at most the
             number of values in \a t / 3.
  \param     max Maximum value of the palette.

  The \a n rgb tuples from palette \a p will be copied.
*/
com::RawPalette::RawPalette(const UINT2 *p, size_t n, UINT2 max)

  : d_tuples(n), d_max(max)

{
  const UINT2 *t = p;
  for(iterator it = begin(); it != end(); it++, t += 3)
    (*it).setRgb(*t, *(t + 1), *(t + 2));
}



/*!
  \param     p Two dimensional array with rgb tuples.
  \param     n The number of rgb tuples in \t. This should be at most the
             number of values in \a t.
  \param     max Maximum value of the palette.

  The \a n rgb tuples from palette \a p will be copied.
*/
com::RawPalette::RawPalette(const UINT2 p[][3], size_t n, UINT2 max)

  : d_tuples(n), d_max(max)

{
  const UINT2 (*t)[3] = p;   // Array of pointers to an array of rgb tuples.
  for(iterator it = begin(); it != end(); it++, t++)
    (*it).setRgb(**t, *(*t + 1), *(*t + 2));
}



com::RawPalette::~RawPalette()
{
}



bool com::RawPalette::equals(RawPalette const& rhs) const
{
  return d_tuples == rhs.d_tuples && d_max == rhs.d_max;
}



void com::RawPalette::resize(size_t n)
{
  d_tuples.resize(n);
}



void com::RawPalette::setMaximum(UINT2 m)
{
  d_max = m;
}



void com::RawPalette::insert(iterator pos, const RgbTuple &t)
{
  d_tuples.insert(pos, t);
}



com::RawPalette::const_iterator com::RawPalette::begin() const
{
  return d_tuples.begin();
}



com::RawPalette::iterator com::RawPalette::begin()
{
  return d_tuples.begin();
}



com::RawPalette::const_iterator com::RawPalette::end() const
{
  return d_tuples.end();
}



com::RawPalette::iterator com::RawPalette::end()
{
  return d_tuples.end();
}



bool com::RawPalette::empty() const
{
  return d_tuples.empty();
}



size_t com::RawPalette::nrColours() const
{
  return d_tuples.size();
}



UINT2 com::RawPalette::max() const
{
  return d_max;
}



com::RgbTuple const&  com::RawPalette::colour(size_t i) const
{
  assert(i < d_tuples.size());

  return d_tuples[i];
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------

bool com::operator==(RawPalette const& lhs, RawPalette const& rhs)
{
  return lhs.equals(rhs);
}



bool com::operator!=(RawPalette const& lhs, RawPalette const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------

/* read palette
 *  \throws BadStreamFormat if error in format 
 */
// std::istream &com::operator>>(std::istream &s, com::RawPalette &p)
// {
//   // Create a basic table.
//   BasicTable t;
// 
//   // Fill it with values from the stream.
//   s >> t;
// 
//   // Check the format of the table.
//   if(t.nrCols() < 3)
//     throw BadStreamFormat("palette should contain at least three columns");
// 
//   p.resize(t.nrRecs());
//   p.setMaximum(255);
// 
//   // Put the rgb values in the palette.
//   BasicTable::const_iterator rit = t.begin(0);
//   BasicTable::const_iterator git = t.begin(1);
//   BasicTable::const_iterator bit = t.begin(2);
//   RawPalette::iterator it;
// 
//   UINT2 r, g, b;
// 
//   for(it = p.begin(); it != p.end(); it++)
//   {
//     r = static_cast<UINT2>(*(rit++));
//     g = static_cast<UINT2>(*(git++));
//     b = static_cast<UINT2>(*(bit++));
//     if(r > 255 || g > 255 || b > 255)
//       throw BadStreamFormat("colour value should be in range [0,255]");
// 
//     (*it).setRgb(r, g, b);
//   }
// 
//   return s;
// }



//! Returns the default palette for data with value scale vs.
/*!
  \param     vs Value scale.
  \return    Palette.
*/
const com::RawPalette* com::defaultPalette(CSF_VS vs)
{
  if(vs == VS_BOOLEAN)
    return RawPalette::booleanPalette();
  else if(vs == VS_NOMINAL)
    return RawPalette::nominalPalette();
  else if(vs == VS_ORDINAL)
    return RawPalette::ordinalPalette();
  else if(vs == VS_SCALAR)
    return RawPalette::scalarPalette();
  else if(vs == VS_DIRECTION)
    return RawPalette::directionalPalette();
  else if(vs == VS_LDD)
    return RawPalette::nominalPalette();
  else
    assert(false);
    return 0;                // Never reached.
}



std::vector<const com::RawPalette*> com::classPalettes()
{
  std::vector<const RawPalette*> palettes;
  palettes.push_back(RawPalette::COLOURCLASSPALETTE());
  palettes.push_back(RawPalette::COLOURBOOLPALETTE());
  palettes.push_back(RawPalette::netscapePalette());
  return palettes;
}



std::vector<const com::RawPalette*> com::rangePalettes()
{
  std::vector<const com::RawPalette*> palettes;
  palettes.push_back(RawPalette::B2WPALETTE());
  palettes.push_back(RawPalette::B2WMPALETTE());
  palettes.push_back(RawPalette::W2BPALETTE());
  palettes.push_back(RawPalette::COLOURCONTPALETTE());
  palettes.push_back(RawPalette::RAINBOWPALETTE());
  palettes.push_back(RawPalette::R2PPALETTE());
  palettes.push_back(RawPalette::R2PMPALETTE());
  palettes.push_back(RawPalette::P2RMPALETTE());
  palettes.push_back(RawPalette::B2YPALETTE());
  palettes.push_back(RawPalette::Y2BPALETTE());
  palettes.push_back(RawPalette::Y2BMPALETTE());

  palettes.push_back(RawPalette::GRAYDIRECTPALETTE());
  palettes.push_back(RawPalette::W2BMPALETTE());
  palettes.push_back(RawPalette::R2GPALETTE());
  palettes.push_back(RawPalette::R2GMPALETTE());
  palettes.push_back(RawPalette::G2RMPALETTE());
  palettes.push_back(RawPalette::B2RMPALETTE());
  palettes.push_back(RawPalette::B2YMPALETTE());

  return palettes;
}



/*!
  \param     p Palette to convert.
  \param     max New maximum rgb value.
  \param     a Array to hold the new, scaled values.
*/
void com::convert(const RawPalette &p, UINT2 max, UINT2 *a)
{
#ifdef DEBUG_DEVELOP
  assert(p.max() > 0);
#endif

  UINT2 *c = a;              // Current colour code.

  for(com::RawPalette::const_iterator it = p.begin(); it != p.end(); it++)
  {
    *c++ = (*it).red() * max / p.max();
    *c++ = (*it).green() * max / p.max();
    *c++ = (*it).blue() * max / p.max();
  }
}



void com::write(std::ostream &s, const RawPalette &p)
{
  for(RawPalette::const_iterator it = p.begin(); it != p.end(); it++)
  {
    s << (*it).red() << ' ' << (*it).green() << ' '
      << (*it).blue() << std::endl;
  }
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


