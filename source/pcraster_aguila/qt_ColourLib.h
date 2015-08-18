#ifndef INCLUDED_QT_COLOURLIB
#define INCLUDED_QT_COLOURLIB



#include <QColor>
#include "csftypes.h"



namespace com {
  class RgbTuple;
}



//       1         2         3         4         5         6         7         8

namespace qt {

  //! Creates a QColor object from a com::RgbTuple object.
  QColor           RgbTupleToQColor    (const com::RgbTuple &t,
                                        UINT2 max);

} // namespace qt

#endif
