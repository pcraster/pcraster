#include "stddefx.h"
#include "calc_xmlscriptclientinterface.h"
#include "PCRasterXSD.h"
#include "pcrxsd_dominput.h"
#include "calc_stringparser.h"
#include "calc_astnodelist.h"
#include "calc_astscript.h"
#include "calc_astsymboltable.h"
#include "calc_stattable.h"
#include "calc_astpar.h"
#include "calc_code.h"
#include "calc_datatypeclash.h"
#include "calc_positionname.h"

/*!
  \file
  This file contains the implementation of the XMLScriptClientInterface class.
*/


namespace calc
{
namespace detail
{
typedef std::pair<std::string, StatTable *> NameStatTablePair;
typedef std::map<std::string, StatTable *> NameStatTableMap;

/*! parse the model parts
   */
class ParseModelParts
{
private:
  Code *d_code;
  NameStatTableMap d_nameStatTableMap;

public:
  ParseModelParts() : d_code(new Code(new ASTNodeList()))
  {
  }

  ~ParseModelParts()
  {
    delete d_code;
  }

  void parseTextModel(std::string const &textModel)
  {
    // Code, initial/dynamic only, timer, bindings are
    //  simply ignored
    delete d_code;
    d_code = StringParser::createCode(pcrxsd::toString<>(textModel));
  }

  void parseTextStatistics(pcrxml::TextStatistics const &s)
  {

    std::string const name(pcrxsd::toString(s.name()));
    if (d_nameStatTableMap.count(name)) {
      std::ostringstream is;
      is << "redefinition of textStatistics with name '" << name << "'" << '\n';
      throw com::Exception(is.str());
    }

    std::vector<StatTable::InputMap> maps;
    for (pcrxml::TextStatisticsSubject const &i : s.textStatisticsSubject()) {
      StatTable::InputMap in;
      in.d_name = pcrxsd::toString(i.fieldRef());
      in.d_field = new ASTPar(pcrxsd::toString(i.fieldRef()));
      if (i.intervalRef())
        in.d_intervals = new ASTPar(pcrxsd::toString(i.intervalRef()));
      maps.push_back(in);
    }
    if (maps.size() == 1) {
      maps.push_back(StatTable::InputMap());
    }
    auto *st = new StatTable(name, maps[0], maps[1]);
    d_code->transferPushBack(st);
    d_nameStatTableMap.insert(std::make_pair(name, st));
  }

  Code *releaseCode()
  {
    Code *c = d_code;
    d_code = nullptr;
    return c;
  }

  NameStatTableMap const &nameStatTableMap() const
  {
    return d_nameStatTableMap;
  }
};

//! update ASTSymbolTable for definitions
class XMLUpdateSymbolTable
{
private:
  ASTSymbolTable &d_table;
  NameStatTableMap const &d_nameStatTableMap;

public:
  XMLUpdateSymbolTable(ASTSymbolTable &table, NameStatTableMap const &nameStatTableMap)
      : d_table(table), d_nameStatTableMap(nameStatTableMap)
  {
    for (NameStatTablePair const i : d_nameStatTableMap) {
      std::string const name(i.first);
      if (d_table.count(name)) {
        // force a type error on existing
        ASTSymbolInfo &sym(d_table[i.second->id()]);
        try {
          sym.dataType().restrict(VS_STATISTICS);
        } catch (const VSClash &v) {
          // PRINT_VAR(name);
          std::string const msg = sym.vsClashError(v, "set");
          sym.throwSym(PositionName("xml"), msg);
        }
      } else {
        // insert as string
        d_table[i.second->id()].dataType() = DataType(VS_STATISTICS);
      }
    }
  }

  XMLUpdateSymbolTable(const XMLUpdateSymbolTable &other) = delete;

  XMLUpdateSymbolTable &operator=(const XMLUpdateSymbolTable &other) = delete;

  //! only update the symbol if found in the model
  void updateUsedSymbols(pcrxml::Definition const &d)
  {
    auto si = d_table.find(d.name());
    if (si == d_table.end())
      return;  // not found

    ASTSymbolInfo &i(si->second);
    i.setDefinition(d);

    auto s = d_nameStatTableMap.find(d.name());
    if (s != d_nameStatTableMap.end()) {
      // TODO now always report that is not conform scriptOutput meaning
      s->second->setIdBinding(i);
    }
  }
};
}  // namespace detail

//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC XMLSCRIPTCLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF XMLSCRIPTCLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------

XMLScriptClientInterface::XMLScriptClientInterface(const std::string &scriptFileOrContents, bool asFile)
    : ClientInterface(scriptFileOrContents, asFile)
{
}

/* NOT IMPLEMENTED
//! Copy constructor.
XMLScriptClientInterface::XMLScriptClientInterface(
         XMLScriptClientInterface const& rhs)

  : Base(rhs)

{
}
*/


XMLScriptClientInterface::~XMLScriptClientInterface()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
XMLScriptClientInterface& XMLScriptClientInterface::operator=(
         XMLScriptClientInterface const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/

void XMLScriptClientInterface::parse()
{
  pcrxsd::DOMInput d(pcrxsd::DOMInput::CompiledIn);
  d.setValidate(true);
  if (d_asFile) {
    d.setFile(d_scriptFileOrContents);
  } else {
    d.setString(d_scriptFileOrContents);
  }
  d_xml.reset(pcrxml::script(*d.document()).release());
}

ASTScript *XMLScriptClientInterface::createScriptAndAnalyzeNoContext()
{
  std::unique_ptr<ASTScript> script(new ASTScript());
  script->setReportOnlyForXMLScriptOutput(true);

  try {
    parse();
  } catch (pcrxsd::Exception const &e) {
    std::ostringstream is;
    is << e.msg() << '\n';
    throw com::Exception(is.str());
  } catch (xml_schema::exception const &e) {
    std::ostringstream is;
    is << e << '\n';
    throw com::Exception(is.str());
  }

  detail::ParseModelParts pmp;

  if (d_xml->model()) {
    pmp.parseTextModel(pcrxsd::toString<>(d_xml->model()->textModel()));
  }
  for (pcrxml::TextStatistics const &s : d_xml->textStatistics()) {
    pmp.parseTextStatistics(s);
  }

  // fixed PCRaster Bugzilla #77
  // setSettingsFromXML must be done before transferCode
  // because symbol used for areaMap must be found
  script->setSettingsFromXML(*d_xml);

  // ASTNodeList *p(new ASTNodeList());
  // p->transferPushBack(pmp.releaseCode());
  // script->transferCode(p);

  script->transferCode(pmp.releaseCode());

  if (d_xml->timer()) {
    Timer t;
    t.setStartInt(d_xml->timer()->integer().start());
    if (d_xml->timer()->integer().end().present())
      t.setLastInt(d_xml->timer()->integer().end().get());
    script->setExternalTimer(t);
  }

  // pick up the symbols used
  script->analyzeNoContext();

  // annotate the symbols used
  detail::XMLUpdateSymbolTable ust(script->symbols(), pmp.nameStatTableMap());
  for (pcrxml::Definition const &d : d_xml->definition()) {
    ust.updateUsedSymbols(d);
  }

  return script.release();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


}  // namespace calc
