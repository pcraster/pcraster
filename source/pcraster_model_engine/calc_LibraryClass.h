#ifndef INCLUDED_CALC_LIBRARYCLASS
#define INCLUDED_CALC_LIBRARYCLASS

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_QCOREAPPLICATION
#include <QCoreApplication>
#define INCLUDED_QCOREAPPLICATION
#endif
#ifndef INCLUDED_DEV_QTCLIENT
#include "dev_QtClient.h"
#define INCLUDED_DEV_QTCLIENT
#endif
#ifndef INCLUDED_DEV_GDALCLIENT
#include "dev_GDalClient.h"
#define INCLUDED_DEV_GDALCLIENT
#endif
#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif
#ifndef INCLUDED_PCRXSD_LIBRARY
#include "pcrxsd_library.h"
#define INCLUDED_PCRXSD_LIBRARY
#endif
#ifndef INCLUDED_DEV_FILESYSTEMUTILS
#include "dev_FilesystemUtils.h"
#define INCLUDED_DEV_FILESYSTEMUTILS
#endif


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
