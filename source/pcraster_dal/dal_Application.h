#ifndef INCLUDED_DAL_APPLICATION
#define INCLUDED_DAL_APPLICATION



// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_QCOREAPPLICATION
#include <QCoreApplication>
#define INCLUDED_QCOREAPPLICATION
#endif

// PCRaster library headers.
#ifndef INCLUDED_DEV_COMMANDLINEAPPLICATION
#include "dev_CommandLineApplication.h"
#define INCLUDED_DEV_COMMANDLINEAPPLICATION
#endif

#ifndef INCLUDED_DEV_GDALCLIENT
#include "dev_GDalClient.h"
#define INCLUDED_DEV_GDALCLIENT
#endif

#ifndef INCLUDED_DEV_QTCLIENT
#include "dev_QtClient.h"
#define INCLUDED_DEV_QTCLIENT
#endif

// Module headers.
#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif

#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif



namespace dal {
  // Application declarations.
  class Dataset;
  class Driver;
}



namespace dal {

namespace po = boost::program_options;



//! Application class for command line tool which uses the library.
/*!
  Support command modes like cvs/svn/git do:
  - dal info/driver/dataset ...  (info about drivers, datasets)
  - dal convert/translate ... (convert datasets, inclusief van en naar stdin/stdout)
  - dal list/discover ... (search for datasets and list them)

  dal [dal-options] command [command-options-and-arguments]

  dal-options:

  commands: driver, info

  driver-options-and-arguments: info of drivers.
    <none>: print information of all drivers
    --datasettype type: print information of all type drivers
    --driver driver: print information of driver
    name of driver: print information of specific driver

  info-options-and-arguments:
    --scenarios, --samples --timesteps: configuration of dataspace
    name of dataset

  \todo case of user input
  \todo handle not existing drivers
  \todo handle not existing data sets
  \todo Print interpretation of band values in dataset.
*/
class Application: public dev::GDalClient,
                   public dev::QtClient<QCoreApplication>,
                   public dal::Client,
                   public dev::CommandLineApplication
{

  friend class ApplicationTest;

private:

  //! Type for functions which handle a command.
  typedef void (Application::*CommandHandler) (po::variables_map const&);

  std::map<std::string, CommandHandler> _commandHandlers;

  void             testDatasetType     (std::string const& name) const;

  void             handleCommandRequest(std::string const& command,
                                        po::variables_map const& variables);

  void             handleDriverCommandRequest(
                                        po::variables_map const& variables);

  void             handleDatasetCommandRequest(
                                        po::variables_map const& variables);

  void             handleResampleCommandRequest(
                                        po::variables_map const& variables);

  void             showDriverInfoByDataset(
                                        std::string const& name) const;

  void             showDriverInfoByDatasetType(
                                        std::string const& name) const;

  void             showDriverInfoByName(std::string const& name="") const;

  void             showDriverInfo      (Driver const& driver) const;

  void             showDatasetInfo     (std::string const& name,
                                        DataSpace const& space=DataSpace()) const;

  void             showDatasetInfo     (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        Dataset const& dataset,
                                        Driver const& driver) const;

  template<class T>
  void             showDatasetInfo     (T const& dataset) const;

  // void             showDatasetInfo     (Raster const& raster) const;

  // void             showDatasetInfo     (Table const& table) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Application         (int argc,
                                        char** argv);

  /* virtual */    ~Application        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  int              run                 ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
