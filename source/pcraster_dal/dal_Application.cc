#ifndef INCLUDED_DAL_APPLICATION
#include "dal_Application.h"
#define INCLUDED_DAL_APPLICATION
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM
#include <boost/filesystem.hpp>
#define INCLUDED_BOOST_FILESYSTEM
#endif

#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

#include <boost/shared_ptr.hpp>

// PCRaster library headers.
#ifndef INCLUDED_DEV_FILESYSTEMUTILS
#include "dev_FilesystemUtils.h"
#define INCLUDED_DEV_FILESYSTEMUTILS
#endif

// Module headers.
#ifndef INCLUDED_DAL_DAL
#include "dal_Dal.h"
#define INCLUDED_DAL_DAL
#endif

#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_FEATURELAYER
#include "dal_FeatureLayer.h"
#define INCLUDED_DAL_FEATURELAYER
#endif

#ifndef INCLUDED_DAL_LIBRARY
#include "dal_Library.h"
#define INCLUDED_DAL_LIBRARY
#endif

#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



/*!
  \file
  This file contains the implementation of the Application class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC APPLICATION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF APPLICATION MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     argc Argument count.
  \param     argv Argument vector.
*/
Application::Application(
         int argc,
         char** argv)

  : dev::GDalClient(),
    dev::QtClient<QCoreApplication>(argc, argv),
    dal::Client(dev::prefix(argv[0]), true),
    dev::CommandLineApplication(argc, argv)

{
  assert(dev::GDalClient::isInitialized());
  assert(dev::QtClient<QCoreApplication>::isInitialized());
  assert(dal::Client::isInitialized());

  genericOptions().add_options()
         ;

  hiddenOptions().add_options()
         ("command", po::value<std::string>())
         ("arguments", po::value< std::vector<std::string> >())
         ;

  // TODO Add overload which takes all semantic options, including a notifier
  //      which can handle the subsequent arguments.
  // TODO Somehow make sure that everything after the command option is passed
  //      to the notifier, instead of being parsed by the CommandLineApplication
  //      base class.
  // TODO Check http://www.boost.org/doc/libs/1_42_0/doc/html/program_options/howto.html#id1420587
  addPositionalOption("command", 1, "driver | dataset | resample");
  addPositionalOption("arguments", -1, "arguments");

  _commandHandlers["driver"] = &Application::handleDriverCommandRequest;
  _commandHandlers["dataset"] = &Application::handleDatasetCommandRequest;
  _commandHandlers["resample"] = &Application::handleResampleCommandRequest;

  // TODO unrecognized options are stored.
  // http://www.boost.org/doc/libs/1_34_0/doc/html/program_options/howto.html#id1593176
  commandLineParser().allow_unregistered();
}



//! Destructor.
/*!
*/
Application::~Application()
{
}



int Application::run()
{
  int result = EXIT_FAILURE;

  try {
    result = parseCommandLine();
    assert(result == EXIT_SUCCESS);

    if(programOptions().empty()) {
      usage(std::cerr);
      result = EXIT_FAILURE;
    }
    else if(!programOptions().count("help") &&
         !programOptions().count("version")) {
      assert(programOptions().count("command"));
      handleCommandRequest(programOptions()["command"].as<std::string>(),
         programOptions());
      result = EXIT_SUCCESS;
    }
  }
  catch(dal::Exception const& exception) {
    showError(exception.message());
  }
  catch(std::exception const& exception) {
    showError(exception.what());
  }

  return result;
}



void Application::handleCommandRequest(
         std::string const& command,
         po::variables_map const& variables)
{
  if(_commandHandlers.find(command) == _commandHandlers.end()) {
    std::string commands = " ";

    for(std::map<std::string, CommandHandler>::const_iterator it =
         _commandHandlers.begin(); it != _commandHandlers.end(); ++it) {
      commands += " " + (*it).first;
    }

    throw Exception((boost::format(
         "Command %1%: Unknown\n"
         "Valid commands are:\n"
         "%2%")
         % command
         % commands).str());
  }

  (this->*_commandHandlers[command])(variables);
}



void Application::handleDriverCommandRequest(
         po::variables_map const& variables)
{
  assert(variables.size() > 0);

  // dalapp driver
  if(variables.size() == 1) {
    assert(variables.count("command") == 1);
    showDriverInfoByName();
  }
  // dalapp driver --datasettype raster
  else if(variables.count("datasettype")) {
    assert(!variables.count("dataset"));
    assert(!variables.count("argument"));
    showDriverInfoByDatasetType(variables["datasettype"].as<std::string>());
  }
  // dalapp driver --dataset dem.pcrmap
  else if(variables.count("dataset")) {
    assert(!variables.count("datasettype"));
    assert(!variables.count("argument"));
    showDriverInfoByDataset(variables["dataset"].as<std::string>());
  }
  // dalapp driver csf
  else if(variables.count("argument")) {
    assert(!variables.count("dataset"));
    assert(!variables.count("datasettype"));
    showDriverInfoByName(variables["argument"].as<std::string>());
  }
}



void Application::handleDatasetCommandRequest(
         po::variables_map const& variables)
{
  if(!variables.count("arguments")) {
    throw Exception("Name of dataset is missing from command");
  }
  else {
    BOOST_FOREACH(std::string const& name,
         variables["arguments"].as< std::vector<std::string> >()) {
      showDatasetInfo(name);
    }
  }
}



void Application::handleResampleCommandRequest(
         po::variables_map const& /* variables */)
{
  // Parse subsequent command line options.

  // Determine aggregation algorithm.

  // dal resample --algorithm <algorithm> <clone> <source> <destination>
  //   algorithm -- aggregation algorithm: sum | min | max | average
  //   clone -- properties of target raster
  //   source -- input raster
  //   destination -- output raster


  // Based on algorithm, create an object which does the actual aggregation,
  // given the required information. There are two classed of algorithms with
  // different requirements:
  // - sum, average: % area, value
  //   Proportional source values used.
  // - min, max: % area, nrDestinationCells, value
  //   Source values used once, by destination cell in which the largest part
  //   of the cell lies.
  //
  // Each aggregator gets an algorithm to do the actual calculation.
  //   dal::resample::min
  //   dal::resample::max
  //   dal::resample::sum
  //   dal::resample::average
  //
  // The overall loop is handled by the dal::resample::Resample template.
  //
  // dal::resample::Resample<Aggregator, average, true> resampler;
  // dal::resample::Resample<Aggregator, min, false> resampler;
  // resampler.calculate(source, destination);




}



void Application::testDatasetType(
         std::string const& name) const
{
  if(!isDatasetType(name)) {
    std::string types = " ";

    for(size_t i = 0; i < NR_DATASET_TYPES; ++i) {
      types += " " + datasetTypeToString(DatasetType(i));
    }

    throw Exception((boost::format("Dataset type %1%: Unknown\n"
         "Valid dataset types are:\n"
         "%2%")
         % name
         % types).str());
  }
}



void Application::showDriverInfoByDatasetType(
         std::string const& name) const
{
  testDatasetType(name);
  DatasetType type = stringToDatasetType(name);

  BOOST_FOREACH(Driver* driver, Client::dal().drivers()) {
    if(driver->datasetType() == type) {
      showDriverInfo(*driver);
    }
  }
}



void Application::showDriverInfoByDataset(
         std::string const& name) const
{
  boost::shared_ptr<Dataset> dataset;
  Driver* driver;
  boost::tie(dataset, driver) = Client::dal().open(name);

  if(dataset) {
    assert(driver);
    showDriverInfo(*driver);
  }
}



void Application::showDriverInfoByName(
         std::string const& name) const
{
  // TODO throw exception when no driver exists with name, show options.

  BOOST_FOREACH(Driver* driver, Client::dal().drivers()) {
    if(name.empty() || driver->name() == name) {
      showDriverInfo(*driver);
    }
  }
}



void Application::showDriverInfo(
         Driver const& driver) const
{
  std::cout <<
  (
    boost::format
    (
      "----> Driver %1% <-----\n"
      "name        : %2%\n"
      "dataset type: %3%\n"
    )
    % driver.name()
    % driver.description()
    % datasetTypeToString(driver.datasetType())
  ).str();
}



template<>
void Application::showDatasetInfo(
         Raster const& raster) const
{
  /*
  data_type   scalar
  projection  yb2t
  angle(deg)  0
  min_val     79.9256
  max_val     111.614
  version     2
  file_id     0
  native      y
  attr_tab    n
  */

  std::cout <<
  (
    boost::format
    (
      "rows        : %1%\n"
      "columns     : %2%\n"
      "cell size   : %3%\n"
      "north       : %4%\n"
      "west        : %5%\n"
      "value types : %6%\n"
    )
    % raster.nrRows()
    % raster.nrCols()
    % raster.cellSize()
    % raster.north()
    % raster.west()
    % typeIdToString(raster.typeId())
  ).str();

  // Optional / extra stuff.
  std::string filenameConvention = "<not relevant or unknown>";

  if(raster.properties().hasValue(DAL_FILENAME_CONVENTION)) {
    filenameConvention = filenameConventionToString(
         raster.properties().value<FilenameConvention>(DAL_FILENAME_CONVENTION));
  }

  std::string legendDescription = "<no legend set>\n";

  if(raster.properties().hasValue(DAL_LEGEND)) {
    Table const& legend(raster.properties().value<Table>(DAL_LEGEND));
    legendDescription = legend.title().empty()
         ? "<no title set>"
         : legend.title();
    legendDescription += "\n";

    for(size_t i = 0; i < legend.nrRecs(); ++i) {
      legendDescription += (boost::format("              %1%  %2%\n")
         % legend.col<INT4>(0)[i]
         % legend.col<std::string>(1)[i]
         ).str();
    }
  }

  std::cout <<
  (
    boost::format
    (
      "filename convention: %1%\n"
      "legend      : %2%"
    )
    % filenameConvention
    % legendDescription
  ).str();

  // TODO more optional stuff: valuescale(?)
}



template<>
void Application::showDatasetInfo(
         FeatureLayer const& layer) const
{
  std::cout <<
  (
    boost::format
    (
      "west        : %1%\n"
      "north       : %2%\n"
      "east        : %3%\n"
      "south       : %4%\n"
      "value type  : %5%\n"
    )
    % layer.dimensions().west()
    % layer.dimensions().north()
    % layer.dimensions().east()
    % layer.dimensions().south()
    % typeIdToString(layer.typeId())
  ).str();

  // for(size_t i = 0; i < layer.nrAttributes(); ++i) {
  //   std::cout << boost::format("%1%%|3t|%2%%|20t|%3%\n")
  //        % (i + 1)
  //        % layer.name(i)
  //        % typeIdToString(layer.typeId(i))
  //        ;
  // }
}



template<>
void Application::showDatasetInfo(
         Table const& table) const
{
  std::cout <<
  (
    boost::format
    (
      "columns     : %1%\n"
    )
    % table.nrCols()
  ).str();
}



void Application::showDatasetInfo(
         std::string const& name,
         DataSpace const& space) const
{
#ifdef DEBUG_DEVELOP
  Client::dal().setDebugging(true);
#endif

  DataSpaceQueryResult result;
  Driver* driver;
  boost::tie(result, driver) = Client::dal().search(name, space,
         SearchThisSpaceOnly, SearchForAllItems);

  if(!result) {
    // TODO throw exception, dataset not found.
    std::cout << name << " not found" << std::endl;
  }
  else {
    assert(driver);
    boost::shared_ptr<Dataset> dataset(driver->open(result));
    assert(dataset);
    showDatasetInfo(name, result.space(), result.address(), *dataset, *driver);
  }
}



void Application::showDatasetInfo(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         Dataset const& dataset,
         Driver const& driver) const
{
  std::cout <<
  (
    boost::format
    (
      "----> Dataset %1% <-----\n"
      "data space  : %2%\n"
      "first item  : %3%\n"
      "driver      : %4%\n"
      "datatype    : %5%\n"
    )
    % dataSpaceToString(space)
    % dataSpaceAddressToString(space, address)
    % name
    % driver.name()
    % datasetTypeToString(dataset.type())
  ).str();

  switch(dataset.type()) {
    case RASTER: {
      showDatasetInfo(dynamic_cast<Raster const&>(dataset));
      break;
    }
    case FEATURE: {
      showDatasetInfo(dynamic_cast<FeatureLayer const&>(dataset));
      break;
    }
    case TABLE: {
      showDatasetInfo(dynamic_cast<Table const&>(dataset));
      break;
    }
    case MATRIX: {
      assert(false);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal
