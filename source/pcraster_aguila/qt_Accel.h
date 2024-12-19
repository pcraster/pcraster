#ifndef INCLUDED_QT_ACCEL
#define INCLUDED_QT_ACCEL

#ifndef INCLUDED_QTGLOBAL
#include <QtGlobal>
#define INCLUDED_QTGLOBAL
#endif

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
  #ifndef INCLUDED_QTKEYCOMBINATION
  #include <QKeyCombination>
  #define INCLUDED_QTKEYCOMBINATION
  #endif
#else
  #ifndef INCLUDED_QT
  #include <Qt>
  #define INCLUDED_QT
  #endif
#endif




namespace qt {


#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
  enum AppAccel
  {
    Modifier       = Qt::ALT,

    // File menu.
    New            = QKeyCombination(Modifier, Qt::Key_N).toCombined(),
    Open           = QKeyCombination(Modifier, Qt::Key_O).toCombined(),
    Save           = QKeyCombination(Modifier, Qt::Key_S).toCombined(),
    Print          = QKeyCombination(Modifier, Qt::Key_P).toCombined(),
    Close          = QKeyCombination(Modifier, Qt::Key_W).toCombined(),
    Exit           = QKeyCombination(Modifier, Qt::Key_Q).toCombined(),

    // Help menu.
    WhatsThis      = QKeyCombination(Qt::SHIFT | Qt::Key_F1).toCombined(),

    MenuBar        = QKeyCombination(Modifier, Qt::Key_B).toCombined()
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
