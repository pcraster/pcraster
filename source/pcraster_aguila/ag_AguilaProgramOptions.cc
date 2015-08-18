#include "ag_AguilaProgramOptions.h"

// Library headers.
#include <fstream>
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/format.hpp>
#include <boost/spirit/include/classic.hpp>
#include <boost/filesystem/path.hpp>

// PCRaster library headers.
#include "pcrxsd_dominput.h"
#include "AguilaXSD.h"
#include "dal_DataSpace.h"
#include "dal_FilesystemUtils.h"
#include "dal_StackInfo.h"
#include "com_exception.h"

// Module headers.
#include "ag_XMLViewItems.h"



/*!
  \file
  This file contains the implementation of the AguilaProgramOptions class.
*/



namespace ag {

typedef std::vector<std::string> VecOfStr;

namespace detail {


  void expand(
           std::vector<std::string>& values)
  {
    std::vector<std::string> splittedValues;

    for(size_t i = 0; i < values.size(); ++i) {
      splittedValues.clear();
      boost::split(splittedValues, values[i], boost::is_any_of(" "),
           boost::token_compress_on);
      values.erase(values.begin() + i);
      values.insert(values.begin() + i, splittedValues.begin(),
           splittedValues.end());
      i += splittedValues.size() - 1;
    }
  }

 static std::vector<pcrxml::StringSet>
     viewPlusSyntaxToStringSet(std::vector<std::string> const& viewValues)
  {
    typedef std::vector<std::string> SV;
    std::vector<SV> r;
    r = AguilaProgramOptions::viewPlusSyntaxToViewCtor(viewValues);
    std::vector<pcrxml::StringSet> s;
    for(size_t v=0; v < r.size(); ++v) {
       s.push_back(pcrxml::StringSet());
       for(size_t i=0; i < r[v].size(); ++i)
         s.back().item().push_back(r[v][i]);
    }
    return s;
  }


  // create StringSet from copy hence no const& argument in
  static std::vector<pcrxml::StringSet> toStringSet(
      std::vector<std::string> t)
  {
    // TODO: why do we need this. Splitting by space doesn't work for
    // TODO: names that contain spaces, eg: Program\ Files.
    // expand(t);
    for(size_t i = 0; i < t.size(); ++i) {
      boost::trim(t[i]);
    }

    return viewPlusSyntaxToStringSet(t);
  }


  template<
    class Set >
   struct Items {
     Set set;
     template<typename E>
     void operator()(E v) {
        set.item().push_back(v);
      }
   };

  //! T is std::string,float or size_t
  template<typename T>
  struct SetParser {
     typedef typename pcrxsd::RangeSetTypeTrait<T> RS;
     static  typename RS::Set set(std::string const& value);
  };

  //! T is float or size_t
  template<typename T>
  struct SetRangeParser : public SetParser<T> {
     typedef typename pcrxsd::RangeSetTypeTrait<T> RS;
     static  typename RS::Range range(std::string const& value);
     static  typename RS::RangeOrSet rangeOrSet(std::string const& str)
    {
      typename RS::RangeOrSet f;
      if(str[0] == '{') {
        f.set(SetParser<T>::set(str));
      } else if(str[0] == '[') {
        f.range(range(str));
      } else {
        throw std::invalid_argument((
               "Value " + str + ": Not a valid set or range").c_str());
      }
      return f;
    }
  };

  template<>
  pcrxml::FloatSet SetParser<float>::set(std::string const& value)
  {
    namespace sp = boost::spirit::classic;

    std::vector<float> v;

    if(!sp::parse(value.c_str(),
           sp::ch_p('{') >>
           sp::list_p.direct(sp::real_parser<float, sp::real_parser_policies<float> >()[sp::push_back_a(v)], sp::ch_p(',')) >>
           sp::ch_p('}'),
           sp::space_p).full) {
      throw std::invalid_argument((
           "value " + value + " is not a valid set").c_str());
    }

    Items<pcrxml::FloatSet> result =
    std::for_each(v.begin(),v.end(),Items<pcrxml::FloatSet>());

    return result.set;
  }

  template<>
  pcrxml::FloatRange SetRangeParser<float>::range(std::string const& value)
  {
    namespace sp = boost::spirit::classic;

    std::vector<float> v;

    if(!sp::parse(value.c_str(),
           sp::ch_p('[') >>
           sp::list_p.direct(sp::real_parser<float, sp::real_parser_policies<float> >()[sp::push_back_a(v)], sp::ch_p(',')) >>
           sp::ch_p(']'),
           sp::space_p).full) {
      throw std::invalid_argument((
           "value " + value + " is not a valid range").c_str());
    }

    if(v.size() == 2) {
      // Default increment is 1.
      v.push_back(1.0);
    }

    if(v.size() != 3) {
      throw std::invalid_argument((
           "range " + value + " must have two or three values").c_str());
    }

    if(v[0] > v[1]) {
      std::swap(v[0], v[1]);
    }

    assert(v.size() == 3);
    return pcrxml::FloatRange(v[0],v[1],v[2]);
  }

  template<>
  pcrxml::OneBasedIntegerSet SetParser<size_t>::set(std::string const& value)
  {
    namespace sp = boost::spirit::classic;

    std::vector<size_t> v;

    if(!sp::parse(value.c_str(),
           sp::ch_p('{') >>
           sp::list_p.direct(sp::uint_p[sp::push_back_a(v)], sp::ch_p(',')) >>
           sp::ch_p('}'),
           sp::space_p).full) {
      throw std::invalid_argument((
           "value " + value + " is not a valid set").c_str());
    }

    Items<pcrxml::OneBasedIntegerSet> result =
    std::for_each(v.begin(),v.end(),Items<pcrxml::OneBasedIntegerSet>());

    return result.set;
  }


  template<>
  pcrxml::OneBasedIntegerRange SetRangeParser<size_t>::range(std::string const& value)
  {
    namespace sp = boost::spirit::classic;

    std::vector<size_t> v;

    if(!sp::parse(value.c_str(),
           sp::ch_p('[') >>
           sp::list_p.direct(sp::uint_p[sp::push_back_a(v)], sp::ch_p(',')) >>
           sp::ch_p(']'),
           sp::space_p).full) {
      throw std::invalid_argument((
           "value " + value + " is not a valid range").c_str());
    }

    if(v.size() == 2) {
      // increment
      v.push_back(1); 
    }
    if(v.size()!=3) {
      throw std::invalid_argument((
           "range " + value + " must have two or three values").c_str());
    }
    if(v[0] > v[1]) {
      std::swap(v[0], v[1]);
    }
    pcrxml::OneBasedIntegerRange result(v[0],v[1],v[2]);
    return result;
  }

  //! value has syntax "{ string1, string2, stringN }"
  template<>
  pcrxml::StringSet SetParser<std::string>::set(std::string const& v)
  {
    std::string value(v);
    boost::trim(value);
    if(!(!value.empty() && value[0] == '{' && value[value.size() - 1] == '}')) {
      throw std::invalid_argument((
           "value " + value + " is not a valid set of strings").c_str());
    }

    value = value.erase(0, 1);
    value = value.erase(value.size() - 1, 1);

    std::vector<std::string> s;
    boost::split(s, value, boost::is_any_of(","));
    pcrxml::StringSet result;
    for(size_t i=0; i < s.size(); ++i) {
     boost::trim(s[i]);
     result.item().push_back(s[i]);
    }

    return result;
  }

  struct DataSpaceFromXML : public pcrxml::DataSpace {
     size_t elementCount;
     DataSpaceFromXML(
         boost::program_options::variables_map  const& v,
         size_t stackStepStart,
         size_t stackStepEnd):
      elementCount(0)
     {
      if(v.count("scenarios")) {
        VecOfStr s = v["scenarios"].as<VecOfStr>();
        for(size_t i = 0; i < s.size(); ++i)
          scenarios().push_back(SetParser<std::string>::set(s[i]));
        elementCount+=scenarios().size();
      }
      if(v.count("quantiles")) {
        VecOfStr s = v["quantiles"].as<VecOfStr>();
        for(size_t i = 0; i < s.size(); ++i)
          quantiles().push_back(SetRangeParser<float>::rangeOrSet(s[i]));
        elementCount+=quantiles().size();
      }

      if(v.count("timesteps")) {
        VecOfStr s = v["timesteps"].as<VecOfStr>();
        for(size_t i = 0; i < s.size(); ++i) {
          pcrxml::OneBasedIntegerRangeOrSet obirs(SetRangeParser<size_t>::rangeOrSet(s[i]));
          timesteps().push_back(pcrxml::Timesteps());
          if (obirs.range().present())
            timesteps().back().range(obirs.range().get());
          if (obirs.set().present())
            timesteps().back().set(obirs.set().get());
          // ALSO MERGE
        }
        elementCount+=timesteps().size();
      }
      if (stackStepEnd != 0) {
       pcrxml::OneBasedIntegerRange range(stackStepStart, stackStepEnd,1);
       timesteps().push_back(pcrxml::Timesteps());
       timesteps().back().range(range);
       elementCount+=1;
      }
    }
   };

  class ViewsFromXML: public pcrxml::VisualisationGroup::view_sequence
  {
    //! modifies vs for stack names and record the range of stackSteps
    void fixStackNameSyntaxAndRecordTimesteps(pcrxml::StringSet& vs) {
      boost::tuple<std::string, dal::DataSpace> tuple;
      std::string name;

      for(size_t i=0; i < vs.item().size(); ++i) {
        name = vs.item()[i];
        // std::cout << "-> " << name << std::endl;
        // name = dal::fixPathname(vs.item()[i]);
        tuple = dal::oldStackName2NameSpaceTuple(name);
        dal::DataSpace const& space(boost::get<1>(tuple));

        if(!space.hasTime()) {
          // Data source name is not in old stack format, reset to the
          // original name.
          name = vs.item()[i];
        }
        else {
          dal::Dimension dimension(space.dimension(dal::Time));
          stackStepStart = std::min<>(
              dimension.value<size_t>(0), stackStepStart);
          stackStepEnd = std::max<>(
              dimension.value<size_t>(1), stackStepEnd);
          name = boost::get<0>(tuple);
        }

        vs.item()[i] = name;
      }
    }

    public:
     size_t elementCount;
     size_t stackStepStart;
     size_t stackStepEnd;

    ViewsFromXML(
         boost::program_options::variables_map const& variables)

      : elementCount(0),
        stackStepStart(std::string::npos),
        stackStepEnd(0)

    {
      std::vector<std::string> optionNames;
      optionNames.push_back("mapView");
      optionNames.push_back("drapeView");
      optionNames.push_back("timeGraphView");
      optionNames.push_back("probabilityGraphView");
      optionNames.push_back("valueOnly");
      optionNames.push_back("defaultView");

      for(std::vector<std::string>::const_iterator it = optionNames.begin();
              it != optionNames.end(); ++it) {

        if(variables.count(*it)) {
          std::vector<pcrxml::StringSet> stringSets(
              toStringSet(variables[*it].as<VecOfStr>()));

          for(size_t i = 0; i < stringSets.size(); ++i) {
            elementCount += stringSets.size();

            fixStackNameSyntaxAndRecordTimesteps(stringSets[i]);

            pcrxml::AguilaView view;
            XMLViewItems::setItems(view, *it, stringSets[i]);
            push_back(view);
          }
        }
      }
    }
  };
} // detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC AGUILAPROGRAMOPTIONS MEMBERS
//------------------------------------------------------------------------------

//! dissect "+" syntax of cmdLine
/*!
 * \returns per view (top vector) a vector of strings
 */
std::vector<std::vector<std::string> >
 ag::AguilaProgramOptions::viewPlusSyntaxToViewCtor(std::vector<std::string> const& viewValues)
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

  for(SV::const_iterator it = viewValues.begin();
      it != viewValues.end(); ++it) {

    r.push_back(SV());
    r.back().push_back(*it);

    while(it + 1 != viewValues.end() &&
            *(it + 1) == "+" && it + 2 != viewValues.end()) {
        it += 2;
        r.back().push_back(*it);
      }
  }
  return r;
}




//------------------------------------------------------------------------------
// DEFINITION OF AGUILAPROGRAMOPTIONS MEMBERS
//------------------------------------------------------------------------------

AguilaProgramOptions::AguilaProgramOptions(
         int argc,
         char **argv)

  : d_license(false),
    d_version(false),
    d_configuration(new pcrxml::Aguila(pcrxml::VisualisationGroup()))

{
  d_configuration->multiView(pcrxml::NrRowsNrCols(1, 1));
  obtainProgramOptions(argc, argv);
}



AguilaProgramOptions::~AguilaProgramOptions()
{
  delete d_configuration;
}



//! view has multiple subview's
bool AguilaProgramOptions::hasMultiView() const {
 if (!d_configuration->multiView().present())
  return false;
 return
   pcrxsd::fundamentalBaseCast<size_t>(d_configuration->multiView()->nrRows())==1
  ||
  pcrxsd::fundamentalBaseCast<size_t>(d_configuration->multiView()->nrCols())==1;
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
std::string const& AguilaProgramOptions::help() const
{
  return d_help;
}



//! get value of d_lockFileName
std::string const& AguilaProgramOptions::lockFileName() const
{
  return d_lockFileName;
}



//! get value of d_configuration
pcrxml::Aguila const& AguilaProgramOptions::configuration() const
{
  assert(d_configuration);
  return *d_configuration;
}



template<class T>
std::ostream& operator<<(std::ostream& stream, std::vector<T> const& values)
{
  std::copy(values.begin(), values.end(),
         std::ostream_iterator<T>(std::cout, ", "));
  return stream;
}



template<class T>
std::ostream& operator<<(std::ostream& stream, std::set<T> const& values)
{
  std::copy(values.begin(), values.end(),
         std::ostream_iterator<T>(std::cout, ", "));
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
void AguilaProgramOptions::obtainProgramOptions(
         int argc,
         char **argv)
{
  namespace po = boost::program_options;

  try {
    po::options_description genericOptions("Command line options");
    genericOptions.add_options()
      // TODO make config and xml mutually exclusive ?????
      ("config,f", po::value<std::string>(), "read configuration from file")
      ("xml,x", po::value<std::string>(), "read configuration from XML file")
      ("help,h", "show usage information")
      ("license", "show license information")
      ("version,v", "show version information")
      ("mapView,2", po::value<VecOfStr>()->composing()->multitoken(),
         "show data in 2D visualisation(s)")
      ("drapeView,3", po::value<VecOfStr>()->composing()->multitoken(),
         "show data in 3D visualisation(s)")
#ifdef DEBUG_DEVELOP
      ("testVisualisation",
         po::value<VecOfStr>()->composing(),
         "show data in test visualisation(s)")
#endif
      ("timeGraphView,t", po::value<VecOfStr>()->composing()->multitoken(),
         "show data in time series visualisation(s)")
      ("probabilityGraphView,p", po::value<VecOfStr>()->composing()->multitoken(),
         "show data in probability distribution visualisation(s)")
      ("valueOnly", po::value<VecOfStr>()->composing()->multitoken(),
         "show data only in value matrix")
      ;

    po::options_description configOptions(
         "Command line and configuration file options");
    configOptions.add_options()
      ("scenarios,n",
         po::value<VecOfStr>()->composing(),
         "scenarios available for data")
      ("timesteps,s",
         po::value<VecOfStr>()->composing(),
         "time steps available for data")
      ("quantiles,q",
         po::value<VecOfStr>()->composing(),
         "quantiles available for data")
      ("lock,l",
         po::value<std::string>(),
         "create lock file")
      ("multi,m",
         po::value<std::string>(),
         "multiple views per window")
      ("cursorValueMonitorFile",
         po::value<std::string>(),
         "enable Save to cursor value monitor file")
      ("fileToGetCursorValue",
         po::value<std::string>(),
         "enable Get from cursor file")
      ;

    // All positional options should be translated into defaultView options.
    po::positional_options_description positionalOptions;
    positionalOptions.add("defaultView", -1);

    // Options allowed both on command line and in config file, but hidden for
    // the user.
    po::options_description hiddenOptions("Hidden options");
    hiddenOptions.add_options()
      ("defaultView", po::value<VecOfStr>()->composing()->multitoken(),
         "show data in default view type")
      ;

    po::options_description cmdLineOptions;
    cmdLineOptions.add(genericOptions).add(configOptions).add(hiddenOptions);

    po::options_description configFileOptions;
    configFileOptions.add(configOptions).add(hiddenOptions);

    po::options_description visibleOptions("Allowed options");
    visibleOptions.add(genericOptions).add(configOptions);

    // namespace cls = po::command_line_style;
    // int style = (
    //        cls::allow_short
    //      | cls::case_insensitive     // TODO seems not to work
    //      | cls::allow_dash_for_short
    //      | cls::short_allow_adjacent
    //      | cls::short_allow_next
    //      | cls::allow_long
    //      | cls::long_allow_adjacent
    //      | cls::long_allow_next
    //      | cls::allow_sticky
    //      | cls::allow_guessing
    //      );

    po::variables_map variables;
    po::store(po::command_line_parser(argc, argv). // style(style).
           options(cmdLineOptions).positional(positionalOptions).run(),
           variables);
    po::notify(variables);

    if(variables.count("config")) {
      d_configFileName = variables["config"].as<std::string>();
      boost::trim(d_configFileName);
      boost::filesystem::path path(d_configFileName);
      dal::testFileCanBeOpenedForReading(path);
      std::ifstream stream(path.string().c_str());
      po::store(parse_config_file(stream, configFileOptions), variables);
      po::notify(variables);
    }

    if(variables.count("xml")) {
      d_configFileName = variables["xml"].as<std::string>();
      boost::trim(d_configFileName);
      boost::filesystem::path path(d_configFileName);
      dal::testFileCanBeOpenedForReading(path);

      delete d_configuration;

      pcrxsd::DOMInput d(pcrxsd::DOMInput::CompiledIn);
      d.setValidate(true);
      d.setFile(path.string().c_str());

      try {
        d_configuration = pcrxml::aguila(*d.document()).release();
      }
      catch(pcrxsd::Exception const& e) {
        throw com::Exception(e.msg());
      }

      // bug see todo
      // ignore all other, by return here
      // if not, this method seem to hang/loop!!!!!
      return;
    }

    if(variables.count("help")) {
      boost::filesystem::path path(argv[0]);
      std::ostringstream stream;
      stream << "<pre>" << path.filename() << " [options] defaultViews\n" << visibleOptions << "</pre>";
      d_help=stream.str();
      return;
    }

    d_license = variables.count("license") > 0;

    if(variables.count("version")) {
      d_version=true;
      return;
    }

    if(variables.count("lock")) {
      d_lockFileName = variables["lock"].as<std::string>();
      boost::trim(d_lockFileName);
    }

    if(variables.count("multi")) {
      std::string fmt;
      fmt = variables["multi"].as<std::string>();
      boost::trim(fmt);

      using namespace boost::spirit::classic;

      size_t nrRows=0, nrCols=0;
      // parse nrRows x nrCols
      com::Exception error("Multi view layout '" + fmt + "': Not valid");

      if(!parse(fmt.c_str(),
        uint_p[assign_a(nrRows)] >> "x" >> uint_p[assign_a(nrCols)]).full) {
        throw error;
      }

      if (nrRows == 0 || nrCols == 0) {
        throw error;
      }

      d_configuration->multiView(pcrxml::NrRowsNrCols(nrRows, nrCols));
    }

    if(variables.count("cursorValueMonitorFile")) {
      std::string fileName =
         variables["cursorValueMonitorFile"].as<std::string>();
      boost::trim(fileName);
      d_configuration->visualisationGroup().cursorValueMonitorFile(fileName);
    }

    if(variables.count("fileToGetCursorValue")) {
      std::string fileName =
         variables["fileToGetCursorValue"].as<std::string>();
      boost::trim(fileName);
      d_configuration->visualisationGroup().fileToGetCursorValue(fileName);
    }

    detail::ViewsFromXML xmlViews(variables);

    if (xmlViews.elementCount) {
      d_configuration->visualisationGroup().view(xmlViews);
    }

    detail::DataSpaceFromXML xmlDataSpace(variables,
         xmlViews.stackStepStart, xmlViews.stackStepEnd);

    if(xmlDataSpace.elementCount) {
      d_configuration->visualisationGroup().searchSpace(xmlDataSpace);
    }
  }
  catch(boost::program_options::error const& error) {
    com::Exception exception(error.what());
    exception.append("Use -h or --help for usage information");
    throw exception;
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag
