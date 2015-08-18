#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_XMLSCRIPTCLIENTINTERFACE
#include "calc_xmlscriptclientinterface.h"
#define INCLUDED_CALC_XMLSCRIPTCLIENTINTERFACE
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif
// PCRaster library headers.
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif
#ifndef INCLUDED_PCRXSD_DOMINPUT
#include "pcrxsd_dominput.h"
#define INCLUDED_PCRXSD_DOMINPUT
#endif
// Module headers.
#ifndef INCLUDED_CALC_STRINGPARSER
#include "calc_stringparser.h"
#define INCLUDED_CALC_STRINGPARSER
#endif
#ifndef INCLUDED_CALC_ASTNODELIST
#include "calc_astnodelist.h"
#define INCLUDED_CALC_ASTNODELIST
#endif
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h"
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif
#ifndef INCLUDED_CALC_STATTABLE
#include "calc_stattable.h"
#define INCLUDED_CALC_STATTABLE
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_CODE
#include "calc_code.h"
#define INCLUDED_CALC_CODE
#endif
#ifndef INCLUDED_CALC_DATATYPECLASH
#include "calc_datatypeclash.h"
#define INCLUDED_CALC_DATATYPECLASH
#endif
#ifndef INCLUDED_CALC_POSITIONNAME
#include "calc_positionname.h"
#define INCLUDED_CALC_POSITIONNAME
#endif

/*!
  \file
  This file contains the implementation of the XMLScriptClientInterface class.
*/


namespace calc {
 namespace detail {
  typedef std::pair<std::string, StatTable *> NameStatTablePair;
  typedef std::map<std::string, StatTable *> NameStatTableMap;

  /*! parse the model parts
   */
  class ParseModelParts {
   private:
     Code* d_code;
     NameStatTableMap d_nameStatTableMap;
   public:
     ParseModelParts():
       d_code(new Code(new ASTNodeList()))
     { }
     ~ParseModelParts() {
      delete d_code;
     }

     void parseTextModel(std::string const& textModel) {
      // Code, initial/dynamic only, timer, bindings are
      //  simply ignored
      delete d_code;
      d_code = StringParser::createCode(pcrxsd::toString<>(textModel));
     }

     void parseTextStatistics(pcrxml::TextStatistics const& s ) {

       std::string name(pcrxsd::toString(s.name()));
       if (d_nameStatTableMap.count(name)) {
          std::ostringstream is;
          is << "redefinition of textStatistics with name '"<<
                name << "'" << std::endl;
          throw com::Exception(is.str());
       }

       std::vector<StatTable::InputMap> maps;
       BOOST_FOREACH(pcrxml::TextStatisticsSubject const& i,
                    s.textStatisticsSubject())
       {
          StatTable::InputMap in;
          in.d_name = pcrxsd::toString(i.fieldRef());
          in.d_field= new ASTPar(pcrxsd::toString(i.fieldRef()));
          if (i.intervalRef())
            in.d_intervals=new ASTPar(pcrxsd::toString(i.intervalRef()));
          maps.push_back(in);
       }
       if (maps.size()==1) {
          maps.push_back(StatTable::InputMap());
       }
       StatTable *st = new StatTable(name, maps[0],maps[1]);
       d_code->transferPushBack(st);
       d_nameStatTableMap.insert(std::make_pair(name,st));
     }

     Code* releaseCode() {
      Code* c=d_code;
      d_code=0;
      return c;
     }

     NameStatTableMap const& nameStatTableMap() const {
      return d_nameStatTableMap;
     }
  };

  //! update ASTSymbolTable for definitions
  class XMLUpdateSymbolTable:
    public boost::noncopyable
  {
  private:
    ASTSymbolTable& d_table;
    NameStatTableMap const& d_nameStatTableMap;
  public:
    XMLUpdateSymbolTable(ASTSymbolTable& table,
                         NameStatTableMap const& nameStatTableMap):
      d_table(table),
      d_nameStatTableMap(nameStatTableMap)
    {
      BOOST_FOREACH(NameStatTablePair i, d_nameStatTableMap)
      {
        std::string name(i.first);
        if (d_table.count(name)) {
          // force a type error on existing
          ASTSymbolInfo& sym(d_table[i.second->id()]);
          try {
           sym.dataType().restrict(VS_STATISTICS);
          } catch(const VSClash& v) {
           // PRINT_VAR(name);
           std::string msg= sym.vsClashError(v,"set");
           sym.throwSym(PositionName("xml"),msg);
          }
        } else {
          // insert as string
         d_table[i.second->id()].dataType()=DataType(VS_STATISTICS);
        }
      }
    }

    //! only update the symbol if found in the model
    void updateUsedSymbols(pcrxml::Definition const& d) {
      ASTSymbolTable::iterator si=d_table.find(d.name());
      if (si == d_table.end())
        return; // not found

      ASTSymbolInfo& i(si->second);
      i.setDefinition(d);

      NameStatTableMap::const_iterator s = d_nameStatTableMap.find(d.name());
      if (s != d_nameStatTableMap.end()) {
        // TODO now always report that is not conform scriptOutput meaning
        s->second->setIdBinding(i);
      }

    }
   };
  }

//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC XMLSCRIPTCLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF XMLSCRIPTCLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------

XMLScriptClientInterface::XMLScriptClientInterface(
    const std::string& scriptFileOrContents,
    bool asFile):
     ClientInterface(scriptFileOrContents,asFile)
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

ASTScript* XMLScriptClientInterface::createScriptAndAnalyzeNoContext()
{
  std::auto_ptr<ASTScript> script(new ASTScript());
  script->setReportOnlyForXMLScriptOutput(true);

  try {
    parse();
  } catch (pcrxsd::Exception const& e) {
    std::ostringstream is;
    is << e.msg() << std::endl;
    throw com::Exception(is.str());
  } catch (xml_schema::exception const& e) {
    std::ostringstream is;
    is << e << std::endl;
    throw com::Exception(is.str());
  }

  detail::ParseModelParts pmp;

  if (d_xml->model()) {
     pmp.parseTextModel(pcrxsd::toString<>(d_xml->model()->textModel()));
  }
  BOOST_FOREACH(pcrxml::TextStatistics const& s, d_xml->textStatistics())
  {
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
  BOOST_FOREACH(pcrxml::Definition const& d, d_xml->definition())
  {
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



} // namespace calc

