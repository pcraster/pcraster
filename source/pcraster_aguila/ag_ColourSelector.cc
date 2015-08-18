#include "ag_ColourSelector.h"



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Returns a selection of \a nrClasses colours from \a palette.
/*!
  \param     palette Palette with colours to choose from.
  \param     nrClasses Number of colours to choose.
  \return    Selection of colours.
  \warning   .

  Distance between the colours in terms of number of colours is constant.
*/
std::vector<QColor> mapEqualInterval(
         com::RawPalette const& palette,
         size_t nrClasses)
{
  assert(!palette.empty());

  std::vector<QColor> result(nrClasses);

  if(nrClasses > 0) {
    if(nrClasses == 1) {
      result[0] = qt::RgbTupleToQColor(*(palette.begin()), palette.max());
    }
    else {
      REAL8 s = static_cast<REAL8>(palette.nrColours() - 1) / (nrClasses - 1);
      com::RawPalette::const_iterator pos;

      for(size_t i = 0; i < nrClasses; i++) {
        pos = palette.begin() + qRound(i * s);
        result[i] = qt::RgbTupleToQColor(*pos, palette.max());
      }
    }
  }

  return result;
}



std::vector<QColor> mapSequential(
         com::RawPalette const& palette,
         size_t selectionSize)
{
  assert(!palette.empty());

  std::vector<QColor> result(selectionSize);

  if(selectionSize > 0) {
    com::RawPalette::const_iterator pos;
    size_t nrColours = palette.nrColours();

    for(size_t i = 0; i < selectionSize; i++) {
      pos = palette.begin() + i % nrColours;
      result[i] = qt::RgbTupleToQColor(*pos, palette.max());
    }
  }

  return result;
}



//------------------------------------------------------------------------------
// INSTANTIATION OF TEMPLATES
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

} // namespace ag
