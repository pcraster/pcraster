#include "qt_ColourLib.h"
#include <cassert>
#include "com_rgbtuple.h"



/*!
  \file
  brief

  more elaborated
*/



QColor qt::RgbTupleToQColor(const com::RgbTuple &t, UINT2 max)
{
  assert(max > 0);

  return QColor(t.red() * 255 / max,
                t.green() * 255 / max,
                t.blue() * 255 / max);
}

