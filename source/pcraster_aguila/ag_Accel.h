#ifndef INCLUDED_AG_ACCEL
#define INCLUDED_AG_ACCEL


#include <QtGlobal>
#include <Qt>

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
  #include <QKeyCombination>
#endif



namespace ag {

  enum AppAccel
  {
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    Animate = QKeyCombination(Qt::ALT, Qt::Key_A).toCombined(),
#else
    Animate = qt::Modifier + static_cast<int>(Qt::Key_A)
#endif
  };

} // namespace ag

#endif
