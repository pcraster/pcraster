#include "qt_New.h"
#include <stdexcept>
#include <memory>        // bad_alloc -> should be in stdexcept (?)
#include <QMessageBox>



/*!
  \warning For this function to work a QApplication object must be created
           first (for the dialog). 

  Out-of-memory-handling function which operator new can call. The next
  code fragment shows how to install it:

  \code
#include <new>

    QApplication application(argc, argv);
    set_new_handler(qt::outOfMemoryHandler);

    try
    {
      // your code here...
      // allocate lots of memory...
    }
    catch(std::bad_alloc &exception)
    {
      // clean up...
    }
  \endcode
*/
void qt::outOfMemoryHandler()
{
  switch(QMessageBox::critical(0, "",
               "Your computer is unable to satisfy\n"
               "a request for memory. Close other\n"
               "applications to free memory and\n"
               "try again or exit.\n",
               QMessageBox::Retry | QMessageBox::Default,
               QMessageBox::Abort))
  {
    case QMessageBox::Retry: {    // Try again; maybe there's more memory now.
      break;
    }
    case QMessageBox::Abort: {    // We can't help anymore.
      std::set_new_handler(0);
      break;
    }
    default: {
      break;
    }
  }
}


