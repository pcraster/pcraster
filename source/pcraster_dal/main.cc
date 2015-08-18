#ifndef INCLUDED_DAL_APPLICATION
#include "dal_Application.h"
#define INCLUDED_DAL_APPLICATION
#endif



int main(int argc,
         char **argv)
{
  int status = 1;

  try {
    dal::Application application(argc, argv);
    status = application.run();
  }
  catch(...) {
    assert(false);
  }

  return status;
}
