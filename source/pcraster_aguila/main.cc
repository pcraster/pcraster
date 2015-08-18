#include <cassert>
#include <QApplication>
#include "ag_Aguila.h"



int main(int argc,
         char **argv)
{
  int status = 1;

  try {
    // Make sure we don't get dithered colours when running on MS-Windows.
    QApplication::setColorSpec(QApplication::CustomColor);

    ag::Aguila app(argc, argv);
    status = app.run();
  }
  catch(...) {
    assert(false);
  }

  return status;
}
