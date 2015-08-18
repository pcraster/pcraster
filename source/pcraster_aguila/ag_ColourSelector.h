#ifndef INCLUDED_QT_COLOURSELECTOR
#define INCLUDED_QT_COLOURSELECTOR

#include <cassert>
#include <vector>
#include <QColor>
#include "qt_ColourLib.h"
#include "com_rawpalette.h"



namespace ag {

std::vector<QColor> mapEqualInterval   (com::RawPalette const& palette,
                                        size_t nrClasses);

std::vector<QColor> mapSequential      (com::RawPalette const& palette,
                                        size_t nrClasses);

template<typename T>
std::vector<QColor> mapLoop            (com::RawPalette const& palette,
                                        std::vector<T> const& classes);

template<typename T>
std::vector<QColor> mapLoop            (com::RawPalette const& palette,
                                        size_t selectionSize,
                                        std::vector<T> const& classes);



template<typename T>
std::vector<QColor> mapLoop(
         com::RawPalette const& palette,
         size_t selectionSize,
         std::vector<T> const& classes)
{
  assert(!palette.empty());

  std::vector<QColor> result(selectionSize);

  if(selectionSize > 0) {
    com::RawPalette::const_iterator pos;
    size_t nrColours = palette.nrColours();

    for(size_t i = 0; i < selectionSize; i++) {
      pos = palette.begin() + static_cast<int>(classes[i]) % nrColours;
      result[i] = qt::RgbTupleToQColor(*pos, palette.max());
    }
  }

  return result;
}



template<typename T>
std::vector<QColor> mapLoop(
         com::RawPalette const& palette,
         std::vector<T> const& classes)
{
  assert(!palette.empty());

  std::vector<QColor> result(classes.size());

  if(palette.nrColours() > 0) {
    com::RawPalette::const_iterator pos;
    size_t nrColours = palette.nrColours();

    for(size_t i = 0; i < classes.size(); i++) {
      pos = palette.begin() + static_cast<int>(classes[i]) % nrColours;
      result[i] = qt::RgbTupleToQColor(*pos, palette.max());
    }
  }

  return result;
}

} // namespace ag

#endif
