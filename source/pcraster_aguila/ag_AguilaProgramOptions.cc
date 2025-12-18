#include "ag_AguilaProgramOptions.h"

// Library headers.
#include <boost/algorithm/string.hpp>
#include <boost/spirit/include/classic.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/ini_parser.hpp>
#include <clipp.h>

// PCRaster library headers.
#include "pcrxsd_dominput.h"
#include "AguilaXSD.h"
#include "dal_DataSpace.h"
#include "dal_FilesystemUtils.h"
#include "dal_StackInfo.h"
#include "com_exception.h"

// Module headers.
#include "ag_XMLViewItems.h"

#include <any>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <unordered_map>

/*!
  \file
  This file contains the implementation of the AguilaProgramOptions class.
*/


namespace ag
{

typedef std::vector<std::string> VecOfStr;
typedef std::unordered_map<std::string, std::any> variables_map;

namespace detail
{


void expand(std::vector<std::string> &values)
{
  std::vector<std::string> splittedValues;

  for (size_t i = 0; i < values.size(); ++i) {
    splittedValues.clear();
    boost::split(splittedValues, values[i], boost::is_any_of(" "), boost::token_compress_on);
    values.erase(values.begin() + i);
    values.insert(values.begin() + i, splittedValues.begin(), splittedValues.end());
    i += splittedValues.size() - 1;
  }
}

static std::vector<pcrxml::StringSet>
viewPlusSyntaxToStringSet(std::vector<std::string> const &viewValues)
{
  typedef std::vector<std::string> SV;
  std::vector<SV> r;
  r = AguilaProgramOptions::viewPlusSyntaxToViewCtor(viewValues);
  std::vector<pcrxml::StringSet> s;
  for (auto &v : r) {
    s.push_back(pcrxml::StringSet());
    for (auto &i : v) {
      s.back().item().push_back(i);
    }
  }
  return s;
}

// create StringSet from copy hence no const& argument in
static std::vector<pcrxml::StringSet> toStringSet(std::vector<std::string> t)
{
  // TODO: why do we need this. Splitting by space doesn't work for
  // TODO: names that contain spaces, eg: Program\ Files.
  // expand(t);
  for (auto &i : t) {
    boost::trim(i);
  }

  return viewPlusSyntaxToStringSet(t);
}

template <class Set> struct Items {
  Set set;

  template <typename E> void operator()(E v)
  {
    set.item().push_back(v);
  }
};

//! T is std::string,float or size_t
template <typename T> struct SetParser {
  typedef typename pcrxsd::RangeSetTypeTrait<T> RS;
  static typename RS::Set set(std::string const &value);
};

//! T is float or size_t
template <typename T> struct SetRangeParser : public SetParser<T> {
  typedef typename pcrxsd::RangeSetTypeTrait<T> RS;
  static typename RS::Range range(std::string const &value);

  static typename RS::RangeOrSet rangeOrSet(std::string const &str)
  {
    typename RS::RangeOrSet f;
    if (str[0] == '{') {
      f.set(SetParser<T>::set(str));
    } else if (str[0] == '[') {
      f.range(range(str));
    } else {
      throw std::invalid_argument(("Value " + str + ": Not a valid set or range").c_str());
    }
    return f;
  }
};

template <> pcrxml::FloatSet SetParser<float>::set(std::string const &value)
{
  namespace sp = boost::spirit::classic;

  std::vector<float> v;

  if (!sp::parse(value.c_str(),
                 sp::ch_p('{') >>
                     sp::list_p.direct(
                         sp::real_parser<float, sp::real_parser_policies<float>>()[sp::push_back_a(v)],
                         sp::ch_p(',')) >>
                     sp::ch_p('}'),
                 sp::space_p)
           .full) {
    throw std::invalid_argument(("value " + value + " is not a valid set").c_str());
  }

  Items<pcrxml::FloatSet> const result = std::for_each(v.begin(), v.end(), Items<pcrxml::FloatSet>());

  return result.set;
}

template <> pcrxml::FloatRange SetRangeParser<float>::range(std::string const &value)
{
  namespace sp = boost::spirit::classic;

  std::vector<float> v;

  if (!sp::parse(value.c_str(),
                 sp::ch_p('[') >>
                     sp::list_p.direct(
                         sp::real_parser<float, sp::real_parser_policies<float>>()[sp::push_back_a(v)],
                         sp::ch_p(',')) >>
                     sp::ch_p(']'),
                 sp::space_p)
           .full) {
    throw std::invalid_argument(("value " + value + " is not a valid range").c_str());
  }

  if (v.size() == 2) {
    // Default increment is 1.
    v.push_back(1.0);
  }

  if (v.size() != 3) {
    throw std::invalid_argument(("range " + value + " must have two or three values").c_str());
  }

  if (v[0] > v[1]) {
    std::swap(v[0], v[1]);
  }

  assert(v.size() == 3);
  return pcrxml::FloatRange(v[0], v[1], v[2]);
}

template <> pcrxml::OneBasedIntegerSet SetParser<size_t>::set(std::string const &value)
{
  namespace sp = boost::spirit::classic;

  std::vector<size_t> v;

  if (!sp::parse(value.c_str(),
                 sp::ch_p('{') >> sp::list_p.direct(sp::uint_p[sp::push_back_a(v)], sp::ch_p(',')) >>
                     sp::ch_p('}'),
                 sp::space_p)
           .full) {
    throw std::invalid_argument(("value " + value + " is not a valid set").c_str());
  }

  Items<pcrxml::OneBasedIntegerSet> const result =
      std::for_each(v.begin(), v.end(), Items<pcrxml::OneBasedIntegerSet>());

  return result.set;
}

template <> pcrxml::OneBasedIntegerRange SetRangeParser<size_t>::range(std::string const &value)
{
  namespace sp = boost::spirit::classic;

  std::vector<size_t> v;

  if (!sp::parse(value.c_str(),
                 sp::ch_p('[') >> sp::list_p.direct(sp::uint_p[sp::push_back_a(v)], sp::ch_p(',')) >>
                     sp::ch_p(']'),
                 sp::space_p)
           .full) {
    throw std::invalid_argument(("value " + value + " is not a valid range").c_str());
  }

  if (v.size() == 2) {
    // increment
    v.push_back(1);
  }
  if (v.size() != 3) {
    throw std::invalid_argument(("range " + value + " must have two or three values").c_str());
  }
  if (v[0] > v[1]) {
    std::swap(v[0], v[1]);
  }
  pcrxml::OneBasedIntegerRange const result(v[0], v[1], v[2]);
  return result;
}

//! value has syntax "{ string1, string2, stringN }"
template <> pcrxml::StringSet SetParser<std::string>::set(std::string const &v)
{
  std::string value(v);
  boost::trim(value);
  if (!(!value.empty() && value[0] == '{' && value[value.size() - 1] == '}')) {
    throw std::invalid_argument(("value " + value + " is not a valid set of strings").c_str());
  }

  value = value.erase(0, 1);
  value = value.erase(value.size() - 1, 1);

  std::vector<std::string> s;
  boost::split(s, value, boost::is_any_of(","));
  pcrxml::StringSet result;
  for (auto &i : s) {
    boost::trim(i);
    result.item().push_back(i);
  }

  return result;
}

struct DataSpaceFromXML : public pcrxml::DataSpace {
  size_t elementCount{0};

  DataSpaceFromXML(variables_map &v, size_t stackStepStart, size_t stackStepEnd)
  {
    if (v.count("scenarios") != 0u) {
      auto s = std::any_cast<VecOfStr>(v["scenarios"]);

      for (auto &i : s) {
        if (i[0] == '=') {
          i.erase(0, 1);
        }
        scenarios().push_back(SetParser<std::string>::set(i));
      }
      elementCount += scenarios().size();
    }
    if (v.count("quantiles") != 0u) {
      auto s = std::any_cast<VecOfStr>(v["quantiles"]);
      for (auto &i : s) {
        if (i[0] == '=') {
          i.erase(0, 1);
        }
        quantiles().push_back(SetRangeParser<float>::rangeOrSet(i));
      }
      elementCount += quantiles().size();
    }

    if (v.count("timesteps") != 0u) {
      auto s = std::any_cast<VecOfStr>(v["timesteps"]);
      for (auto &i : s) {
        if (i[0] == '=') {
          i.erase(0, 1);
        }
        pcrxml::OneBasedIntegerRangeOrSet obirs(SetRangeParser<size_t>::rangeOrSet(i));
        timesteps().push_back(pcrxml::Timesteps());
        if (obirs.range().present()) {
          timesteps().back().range(obirs.range().get());
        }
        if (obirs.set().present()) {
          timesteps().back().set(obirs.set().get());
        }
        // ALSO MERGE
      }
      elementCount += timesteps().size();
    }
    if (stackStepEnd != 0) {
      pcrxml::OneBasedIntegerRange const range(stackStepStart, stackStepEnd, 1);
      timesteps().push_back(pcrxml::Timesteps());
      timesteps().back().range(range);
      elementCount += 1;
    }
  }
};

class ViewsFromXML : public pcrxml::VisualisationGroup::view_sequence
{
  //! modifies vs for stack names and record the range of stackSteps
  void fixStackNameSyntaxAndRecordTimesteps(pcrxml::StringSet &vs)
  {
    std::tuple<std::string, dal::DataSpace> tuple;
    std::string name;

    for (auto &i : vs.item()) {
      name = i;
      // std::cout << "-> " << name << std::endl;
      // name = dal::fixPathname(vs.item()[i]);
      tuple = dal::oldStackName2NameSpaceTuple(name);
      dal::DataSpace const &space(std::get<1>(tuple));

      if (!space.hasTime()) {
        // Data source name is not in old stack format, reset to the
        // original name.
        name = i;
      } else {
        dal::Dimension dimension(space.dimension(dal::Time));
        stackStepStart = std::min<>(dimension.value<size_t>(0), stackStepStart);
        stackStepEnd = std::max<>(dimension.value<size_t>(1), stackStepEnd);
        name = std::get<0>(tuple);
      }

      i = name;
    }
  }

public:
  size_t elementCount{0};
  size_t stackStepStart;
  size_t stackStepEnd{0};

  ViewsFromXML(variables_map &variables)

      : stackStepStart(std::string::npos)

  {
    std::vector<std::string> optionNames;
    optionNames.push_back("mapView");
#ifdef AGUILA_WITH_OPENGL
    optionNames.push_back("drapeView");
#endif
    optionNames.push_back("timeGraphView");
    optionNames.push_back("probabilityGraphView");
    optionNames.push_back("valueOnly");
    optionNames.push_back("defaultView");

    for (auto &optionName : optionNames) {

      if (variables.count(optionName) != 0u) {
        std::vector<pcrxml::StringSet> stringSets(
            toStringSet(std::any_cast<VecOfStr>(variables[optionName])));

        for (size_t i = 0; i < stringSets.size(); ++i) {
          elementCount += stringSets.size();

          fixStackNameSyntaxAndRecordTimesteps(stringSets[i]);

          pcrxml::AguilaView view;
          XMLViewItems::setItems(view, optionName, stringSets[i]);
          push_back(view);
        }
      }
    }
  }
};
}  // namespace detail

//------------------------------------------------------------------------------
// DEFINITION OF STATIC AGUILAPROGRAMOPTIONS MEMBERS
//------------------------------------------------------------------------------

//! dissect "+" syntax of cmdLine
/*!
 * \returns per view (top vector) a vector of strings
 */
std::vector<std::vector<std::string>>
ag::AguilaProgramOptions::viewPlusSyntaxToViewCtor(std::vector<std::string> const &viewValues)
{
  // + is a string on its own
  // if viewValues is   a + b c + d + e f
  // then r should become
  // r[0][0] = a
  // r[0][1] = b
  // r[1][0] = c
  // r[1][1] = d
  // r[1][2] = e
  // r[2][0] = f
  typedef std::vector<std::string> SV;
  std::vector<SV> r;

  for (auto it = viewValues.begin(); it != viewValues.end(); ++it) {

    r.push_back(SV());
    r.back().push_back(*it);

    while (it + 1 != viewValues.end() && *(it + 1) == "+" && it + 2 != viewValues.end()) {
      it += 2;
      r.back().push_back(*it);
    }
  }
  return r;
}

//------------------------------------------------------------------------------
// DEFINITION OF AGUILAPROGRAMOPTIONS MEMBERS
//------------------------------------------------------------------------------

AguilaProgramOptions::AguilaProgramOptions(int argc, char **argv)

    : d_configuration(new pcrxml::Aguila(pcrxml::VisualisationGroup()))

{
  d_configuration->multiView(pcrxml::NrRowsNrCols(1, 1));
  obtainProgramOptions(argc, argv);
}

AguilaProgramOptions::~AguilaProgramOptions()
{
  delete d_configuration;
}

//! view has multiple subview's
bool AguilaProgramOptions::hasMultiView() const
{
  if (!d_configuration->multiView().present()) {
    return false;
  }
  return pcrxsd::fundamentalBaseCast<size_t>(d_configuration->multiView()->nrRows()) == 1 ||
         pcrxsd::fundamentalBaseCast<size_t>(d_configuration->multiView()->nrCols()) == 1;
}

//! get value of d_license
bool AguilaProgramOptions::license() const
{
  return d_license;
}

//! get value of d_version
bool AguilaProgramOptions::version() const
{
  return d_version;
}

//! get value of d_help
std::string const &AguilaProgramOptions::help() const
{
  return d_help;
}

//! get value of d_lockFileName
std::string const &AguilaProgramOptions::lockFileName() const
{
  return d_lockFileName;
}

//! get value of d_configuration
pcrxml::Aguila const &AguilaProgramOptions::configuration() const
{
  assert(d_configuration);
  return *d_configuration;
}

template <class T> std::ostream &operator<<(std::ostream &stream, std::vector<T> const &values)
{
  std::copy(values.begin(), values.end(), std::ostream_iterator<T>(std::cout, ", "));
  return stream;
}

template <class T> std::ostream &operator<<(std::ostream &stream, std::set<T> const &values)
{
  std::copy(values.begin(), values.end(), std::ostream_iterator<T>(std::cout, ", "));
  return stream;
}

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Global options, animation interval, loop animation, ...
  \todo      Bug: xml option will discard other options by breaking
             out of obtainProgramOptions. If not, we get errors or
             obtainProgramOptions never returns!
*/
void AguilaProgramOptions::obtainProgramOptions(int argc, char **argv)
{
  bool show_help = false;
  std::string config_filename;
  std::string xml_filename;
  VecOfStr unrecognised;
  VecOfStr mapView;
  VecOfStr drapeView;
#ifdef DEBUG_DEVELOP
  VecOfStr testVisualisation;
#endif
  VecOfStr timeGraphView;
  VecOfStr probabilityGraphView;
  VecOfStr valueOnly;

  auto genericOptions =
      "Command line options:" %
      ((clipp::option("-f", "--config") & clipp::values("config_filename", config_filename))
           .doc("read configuration from file"),
       (clipp::option("-f=", "--config=") & clipp::values("config_filename=", config_filename)),
       (clipp::option("-x", "--xml") & clipp::values("xml_filename", xml_filename))
           .doc("read configuration from XML file"),
       (clipp::option("-x=", "--xml=") & clipp::values("xml_filename=", xml_filename)),
       (clipp::option("-h", "--help").set(show_help).doc("show usage information")),
       clipp::option("--license").set(d_license).doc("show license information"),
       clipp::option("-v", "--version").set(d_version).doc("show version information"),
       (clipp::repeatable(clipp::option("-2", "--mapView") & clipp::values("mapView", mapView)))
           .doc("show data in 2D visualisation(s)"),
       (clipp::repeatable(clipp::option("-2=", "--mapView=") & clipp::values("mapView", mapView))),
#ifdef AGUILA_WITH_OPENGL
       (clipp::repeatable(clipp::option("-3", "--drapeView") & clipp::values("drapeView", drapeView)))
           .doc("show data in 3D visualisation(s)"),
       (clipp::repeatable(clipp::option("-3=", "--drapeView=") & clipp::values("drapeView", drapeView))),
#endif
#ifdef DEBUG_DEVELOP
       (clipp::repeatable(clipp::option("--testVisualisation") &
                          clipp::value("testVisualisation", testVisualisation)))
           .doc("show data in test visualisation(s)"),
#endif
       (clipp::repeatable(clipp::option("-t", "--timeGraphView") &
                          clipp::values("timeGraphView", timeGraphView)))
           .doc("show data in time series visualisation(s)"),
       (clipp::repeatable(clipp::option("-t=", "--timeGraphView=") &
                          clipp::values("timeGraphView", timeGraphView))),
       (clipp::repeatable(clipp::option("-p", "--probabilityGraphView") &
                          clipp::values("probabilityGraphView", probabilityGraphView)))
           .doc("show data in probability distribution visualisation(s)"),
       (clipp::repeatable(clipp::option("-p=", "--probabilityGraphView=") &
                          clipp::values("probabilityGraphView", probabilityGraphView))),
       (clipp::repeatable(clipp::option("--valueOnly") & clipp::values("valueOnly", valueOnly)))
           .doc("show data only in value matrix"),
       (clipp::repeatable(clipp::option("--valueOnly=") & clipp::values("valueOnly", valueOnly))));

  VecOfStr scenarios;
  VecOfStr timesteps;
  VecOfStr quantiles;
  std::string lock;
  std::string multi;
  std::string cursorValueMonitorFile;
  std::string fileToGetCursorValue;

  auto configOptions =
      "Command line and configuration file options:" %
      ((clipp::repeatable(clipp::option("-n", "--scenarios") & clipp::value("scenarios", scenarios)))
           .doc("scenarios available for data"),
       (clipp::repeatable(clipp::option("-n=", "--scenarios=") & clipp::value("scenarios", scenarios))),
       (clipp::repeatable(clipp::option("-s", "--timesteps") & clipp::value("timesteps", timesteps)))
           .doc("time steps available for data"),
       (clipp::repeatable(clipp::option("-s=", "--timesteps=") & clipp::value("timesteps", timesteps))),
       (clipp::repeatable(clipp::option("-q", "--quantiles") & clipp::value("quantiles", quantiles)))
           .doc("quantiles available for data"),
       (clipp::repeatable(clipp::option("-q=", "--quantiles=") & clipp::value("quantiles", quantiles))),
       (clipp::option("-l", "--lock") & clipp::value("lock", lock)).doc("create lock file"),
       (clipp::option("-l=", "--lock=") & clipp::value("lock", lock)),
       (clipp::option("-m", "--multi") & clipp::value("multi", multi)).doc("multiple views per window"),
       (clipp::option("-m=", "--multi=") & clipp::value("multi", multi)),
       (clipp::option("--cursorValueMonitorFile") &
        clipp::value("cursorValueMonitorFile", cursorValueMonitorFile))
           .doc("enable Save to cursor value monitor file"),
       (clipp::option("--cursorValueMonitorFile=") &
        clipp::value("cursorValueMonitorFile", cursorValueMonitorFile)),
       (clipp::option("--fileToGetCursorValue") &
        clipp::value("fileToGetCursorValue", fileToGetCursorValue))
           .doc("enable Get from cursor file"),
       (clipp::option("--fileToGetCursorValue=") &
        clipp::value("fileToGetCursorValue", fileToGetCursorValue)));

  // All positional options should be translated into defaultView options.
  VecOfStr defaultView;

  auto cli = (genericOptions, configOptions, clipp::opt_values("defaultView", defaultView),
              clipp::any_other(unrecognised));

  auto result = clipp::parse(argc, argv, cli);

  //   clipp::debug::print(std::cout, result);

  if (show_help) {

    auto fmt = clipp::doc_formatting{}.paragraph_spacing(0).first_column(0).doc_column(8);

    std::filesystem::path const path(argv[0]);
    std::ostringstream stream;
    stream << "<pre>" << path.filename() << " [options] defaultViews\nAllowed options:\n\n";
    stream << clipp::documentation(genericOptions, fmt).str();
    stream << "\n\n";
    stream << clipp::documentation(configOptions, fmt).str();
    stream << "</pre>";
    d_help = stream.str();
    return;
  }

  if (d_license) {
    return;
  }

  if (d_version) {
    return;
  }

  if (result && unrecognised.empty()) {

    variables_map variables;

    if (!config_filename.empty()) {
      d_configFileName = config_filename;
      boost::trim(d_configFileName);
      std::filesystem::path const path(d_configFileName);
      dal::testFileCanBeOpenedForReading(path);
      std::ifstream const stream(path.string().c_str());

      using boost::property_tree::ptree;
      ptree pt;

      read_ini(path.string().c_str(), pt);

      for (auto &key : pt) {
        if (key.first == "defaultView") {
          defaultView.emplace_back(key.second.get_value<std::string>());
        } else if (key.first == "scenarios") {
          scenarios.emplace_back(key.second.get_value<std::string>());
        } else if (key.first == "timesteps") {
          timesteps.emplace_back(key.second.get_value<std::string>());
        } else if (key.first == "quantiles") {
          quantiles.emplace_back(key.second.get_value<std::string>());
        } else if (key.first == "lock") {
          lock = key.second.get_value<std::string>();
        } else if (key.first == "multi") {
          multi = key.second.get_value<std::string>();
        } else if (key.first == "cursorValueMonitorFile") {
          cursorValueMonitorFile = key.second.get_value<std::string>();
        } else if (key.first == "fileToGetCursorValue") {
          fileToGetCursorValue = key.second.get_value<std::string>();
        } else {
          std::stringstream msg{};
          msg << "unrecognised option '" << key.first << "'\n";
          msg << "Use -h or --help for usage information";
          com::Exception const exception(msg.str());
          throw exception;
        }
      }
    }

    if (!xml_filename.empty()) {
      d_configFileName = xml_filename;
      boost::trim(d_configFileName);
      std::filesystem::path const path(d_configFileName);
      dal::testFileCanBeOpenedForReading(path);

      delete d_configuration;

      pcrxsd::DOMInput d(pcrxsd::DOMInput::CompiledIn);
      d.setValidate(true);
      d.setFile(path.string().c_str());

      try {
        d_configuration = pcrxml::aguila(*d.document()).release();
      } catch (pcrxsd::Exception const &e) {
        throw com::Exception(e.msg());
      }

      // bug see todo
      // ignore all other, by return here
      // if not, this method seem to hang/loop!!!!!
      return;
    }

    if (!lock.empty()) {
      d_lockFileName = lock;
      boost::trim(d_lockFileName);
    }

    if (!multi.empty()) {
      boost::trim(multi);

      using namespace boost::spirit::classic;

      size_t nrRows = 0;
      size_t nrCols = 0;
      // parse nrRows x nrCols
      com::Exception const error("Multi view layout '" + multi + "': Not valid");

      if (!parse(multi.c_str(), uint_p[assign_a(nrRows)] >> "x" >> uint_p[assign_a(nrCols)]).full) {
        throw error;
      }

      if (nrRows == 0 || nrCols == 0) {
        throw error;
      }

      d_configuration->multiView(pcrxml::NrRowsNrCols(nrRows, nrCols));
    }

    if (!cursorValueMonitorFile.empty()) {
      boost::trim(cursorValueMonitorFile);
      d_configuration->visualisationGroup().cursorValueMonitorFile(cursorValueMonitorFile);
    }

    if (!fileToGetCursorValue.empty()) {
      boost::trim(fileToGetCursorValue);
      d_configuration->visualisationGroup().fileToGetCursorValue(fileToGetCursorValue);
    }

    variables["defaultView"] = defaultView;
    variables["timesteps"] = timesteps;
    variables["scenarios"] = scenarios;
    variables["quantiles"] = quantiles;
    variables["mapView"] = mapView;
    variables["timeGraphView"] = timeGraphView;
    variables["probabilityGraphView"] = probabilityGraphView;
    variables["valueOnly"] = valueOnly;
#ifdef AGUILA_WITH_OPENGL
    variables["drapeView"] = drapeView;
#endif
#ifdef DEBUG_DEVELOP
    variables["testVisualisation"] = testVisualisation;
#endif

    detail::ViewsFromXML const xmlViews(variables);

    if (xmlViews.elementCount != 0u) {
      d_configuration->visualisationGroup().view(xmlViews);
    }

    detail::DataSpaceFromXML const xmlDataSpace(variables, xmlViews.stackStepStart,
                                                xmlViews.stackStepEnd);

    if (xmlDataSpace.elementCount != 0u) {
      d_configuration->visualisationGroup().searchSpace(xmlDataSpace);
    }
  } else {
    std::stringstream msg{};
    msg << "unrecognised option '" << unrecognised[0] << "'\n";
    msg << "Use -h or --help for usage information";
    com::Exception const exception(msg.str());
    throw exception;
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


}  // namespace ag
