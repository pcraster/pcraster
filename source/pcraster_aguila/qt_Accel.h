#ifndef INCLUDED_QT_ACCEL
#define INCLUDED_QT_ACCEL



#include <Qt>



namespace qt {

  enum AppAccel
  {
    Modifier       = Qt::ALT,

    // File menu.
    New            = Modifier + Qt::Key_N,
    Open           = Modifier + Qt::Key_O,
    Save           = Modifier + Qt::Key_S,
    Print          = Modifier + Qt::Key_P,
    Close          = Modifier + Qt::Key_W,
    Exit           = Modifier + Qt::Key_Q,

    // Help menu.
    WhatsThis      = Qt::SHIFT + Qt::Key_F1,

    MenuBar        = Modifier + Qt::Key_B
  };

} // namespace qt

#endif
