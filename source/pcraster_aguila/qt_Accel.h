#ifndef INCLUDED_QT_ACCEL
#define INCLUDED_QT_ACCEL

#include <QtGlobal>
#include <Qt>

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
  #include <QKeyCombination>
#endif



namespace qt {


#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
  enum AppAccel
  {
    // File menu.
    New            = QKeyCombination(Qt::ALT, Qt::Key_N).toCombined(),
    Open           = QKeyCombination(Qt::ALT, Qt::Key_O).toCombined(),
    Save           = QKeyCombination(Qt::ALT, Qt::Key_S).toCombined(),
    Print          = QKeyCombination(Qt::ALT, Qt::Key_P).toCombined(),
    Close          = QKeyCombination(Qt::ALT, Qt::Key_W).toCombined(),
    Exit           = QKeyCombination(Qt::ALT, Qt::Key_Q).toCombined(),

    // Help menu.
    WhatsThis      = QKeyCombination(Qt::SHIFT | Qt::Key_F1).toCombined(),

    MenuBar        = QKeyCombination(Qt::ALT, Qt::Key_B).toCombined()
  };
#else
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
#endif

} // namespace qt

#endif
