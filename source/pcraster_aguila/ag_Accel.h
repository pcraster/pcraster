#ifndef INCLUDED_AG_ACCEL
#define INCLUDED_AG_ACCEL



#include <Qt>
#include "qt_Accel.h"



namespace ag {

  enum AppAccel
  {
    Animate = qt::Modifier + static_cast<int>(Qt::Key_A)
  };

} // namespace ag

#endif
