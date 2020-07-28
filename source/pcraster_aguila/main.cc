#include <cassert>
#include <QApplication>
#include <QtGlobal>
#include "ag_Aguila.h"



int main(int argc,
         char **argv)
{
  int status = 1;

  try {
#if QT_VERSION < QT_VERSION_CHECK(5, 8, 0)
    // Make sure we don't get dithered colours when running on MS-Windows.
    QApplication::setColorSpec(QApplication::CustomColor);
#endif

    ag::Aguila app(argc, argv);
    status = app.run();
  }
  catch(...) {
    assert(false);
  }

  return status;
}
