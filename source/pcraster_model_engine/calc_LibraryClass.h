#ifndef INCLUDED_CALC_LIBRARYCLASS
#define INCLUDED_CALC_LIBRARYCLASS

#include "dev_QtClient.h"
#include "dev_GDalClient.h"
#include "dal_Client.h"
#include "pcrxsd_library.h"
#include "dev_FilesystemUtils.h"

#include <QCoreApplication>

#include <string>


namespace calc {
 struct LibraryClassNoQt:
    public dev::GDalClient,
    public dal::Client,
     public pcrxsd::Library
  {
    LibraryClassNoQt(
         const std::string& appName)
      : dev::GDalClient(),
        dal::Client(dev::prefix(appName), true, false),
        pcrxsd::Library()
    {
      assert(dev::GDalClient::isInitialized());
      assert(dal::Client::isInitialized());
    }
  };

  //! used in main.cc of pcrcalc
  //  when used in calc_test.cc we get a segfault
  struct LibraryClass: public LibraryClassNoQt,
                   public dev::QtClient<QCoreApplication>
  {
    LibraryClass(
         int argc,
         char** argv)
      : LibraryClassNoQt(argv[0]),
        dev::QtClient<QCoreApplication>(argc, argv)
        {
          assert(dev::QtClient<QCoreApplication>::isInitialized());
        }
  };
}

#endif
