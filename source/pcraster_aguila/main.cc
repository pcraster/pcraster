#include "ag_Aguila.h"

#include <QApplication>

#include <cassert>

int main(int argc, char **argv)
{
  int status = 1;

  try {
    ag::Aguila app(argc, argv);
    status = app.run();
  } catch (...) {
    assert(false);
  }

  return status;
}
