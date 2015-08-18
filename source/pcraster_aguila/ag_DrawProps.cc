#include "ag_DrawProps.h"
#include <cassert>
#include "com_rawpalette.h"
#include "qt_ColourLib.h"



/*!
  \file
  This file contains the implementation of the DrawProps class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

size_t ag::DrawProps::_freeColourId = 0;



com::RawPalette const* ag::DrawProps::_datasetColours =
         com::RawPalette::nominalPalette();


//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     p Palette.
*/
ag::DrawProps::DrawProps(
         std::string const& title,
         com::RawPalette const* p)

  : _title(title),
    _palette(p),
    _nrClasses(0)

{
  assert(p);

  _freeColourId = _freeColourId % _datasetColours->nrColours();
  _colour = qt::RgbTupleToQColor(_datasetColours->colour(_freeColourId),
         _datasetColours->max());
  ++_freeColourId;
}



ag::DrawProps::DrawProps(
         DrawProps const& properties)

  : _title(properties._title),
    _palette(properties._palette),
    _nrClasses(properties._nrClasses),
    _colours(properties._colours),
    _labels(properties._labels)

{
}



//! Destructor.
/*!
  The palette is for use only and is not deleted here.
*/
ag::DrawProps::~DrawProps()
{
}



ag::DrawProps& ag::DrawProps::operator=(DrawProps const& rhs)
{
  if(this != &rhs) {
    _title = rhs._title;
    _palette = rhs._palette;
    _nrClasses = rhs._nrClasses;
    _colours = rhs._colours;
    _labels = rhs._labels;
  }

  return *this;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
bool ag::DrawProps::equals(DrawProps const& rhs) const
{
  return _title == rhs._title &&
         *_palette == *rhs._palette &&
         _nrClasses == rhs._nrClasses &&
         _colours == rhs._colours &&
         _labels == rhs._labels;
}



void ag::DrawProps::setPalette(const com::RawPalette* palette)
{
  _palette = palette;
  reMapColours();
}



//! Returns the title.
/*!
  \return    Title.
*/
const std::string& ag::DrawProps::title() const
{
  return _title;
}



//! Returns the palette.
/*!
  \return    Palette.
*/
const com::RawPalette* ag::DrawProps::palette() const
{
  assert(_palette);
  return _palette;
}



size_t ag::DrawProps::nrClasses() const
{
  return _nrClasses;
}



QColor const& ag::DrawProps::colour() const
{
  return _colour;
}



//! Returns the colour for class with index \a i.
/*!
  \param     i Class index [0, nrclasses).
  \return    Colour.
*/
const QColor& ag::DrawProps::colourByIndex(
         size_t i) const
{
  assert(i < _colours.size());
  return _colours[i];
}



//! Returns the label for class with index \a i.
/*!
  \param     i Class index [0, nrclasses).
  \return    Label.
*/
const std::string& ag::DrawProps::label(size_t i) const
{
  assert(i < _labels.size());
  return _labels[i];
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

bool ag::operator==(DrawProps const& lhs, DrawProps const& rhs)
{
  return lhs.equals(rhs);
}



bool ag::operator!=(DrawProps const& lhs, DrawProps const& rhs)
{
  return !lhs.equals(rhs);
}



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


