#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_WLDELFTHABITAT
#include "calc_wldelfthabitat.h"
#define INCLUDED_CALC_WLDELFTHABITAT
#endif

// #define XML_TRACE_LOG(x)  (x)
#define XML_TRACE_LOG(x)

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_STRCONV
#include "com_strconv.h"
#define INCLUDED_COM_STRCONV
#endif
#ifndef INCLUDED_COM_INTERVAL
#include "com_interval.h"
#define INCLUDED_COM_INTERVAL
#endif
#ifndef INCLUDED_COM_CLONE
#include "com_clone.h"
#define INCLUDED_COM_CLONE
#endif
#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif
#ifndef INCLUDED_PCRXML_DOMALGORITHM
#include "pcrxml_domalgorithm.h"
#define INCLUDED_PCRXML_DOMALGORITHM
#endif
#ifndef INCLUDED_PCRXML_DOM
#include "pcrxml_dom.h"
#define INCLUDED_PCRXML_DOM
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif
#ifndef INCLUDED_APPARGS
#include "appargs.h" // appIOstrategy
#define INCLUDED_APPARGS
#endif
// Module headers.
#ifndef INCLUDED_CALC_LOOKUPTABLE
#include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif
#ifndef INCLUDED_CALC_LOOKUPLINEAR
#include "calc_lookuplinear.h"
#define INCLUDED_CALC_LOOKUPLINEAR
#endif
#ifndef INCLUDED_CALC_STATTABLE
#include "calc_stattable.h"
#define INCLUDED_CALC_STATTABLE
#endif



/*!
  \file
  This file contains the implementation of the WlDelftHabitat class.
  First version was the JSX stuff
*/



//------------------------------------------------------------------------------


namespace calc {

  class RuleName2Tag: public std::map<std::string, std::string> {
   public:
    void add(const std::string& key, const std::string& value) {
     insert(std::make_pair(key,value));
    }
  };


  static std::string expectAttr(
      const QDomElement& e,
      const QString& attrName) {
    QString value=e.attribute(attrName);
    if (value == QString::null) {
      std::ostringstream os;
      os << "expect element '"
         << std::string(e.tagName().toLatin1())
         << "' to have attribute '" 
         << std::string(attrName.toLatin1()) << "'";
      throw com::BadStreamFormat(os.str());
    }
    return std::string(value.toLatin1());
  }
  /*! return atttribute value of attribute valueOf of first
   * matching descendant element with tagName \a tag
   */
  static std::string value(
      const QDomElement& e)
  {
    return expectAttr(e,"value");
  }
  /*! return atttribute value of attribute \a attrName of 
   * first sibling of the first matching descendant element with 
   * tagName \a subTagName
   * <pre>
   *  <tree>
   *    .....
   *    <subTagName> <!-- match -->
   *     <subTagName attrName="thisReturnValue"/> <!-- subMatch -->
   *    </subTagName>
   *    ....
   *  </tree>
   * </pre>
   */
  static std::string attrOfSubMatch(
      const QDomElement& tree,
      const QString& subTagName,
      const QString& attrName)
  {
    QDomElement match(pcrxml::firstMatchByTagName(tree,subTagName));
    if (match.isNull()) {
      std::ostringstream os;
      os << "expect element '"
         << std::string(tree.tagName().toLatin1()) << "' to include element '"
         << std::string(subTagName.toLatin1()) << "'";
      throw com::BadStreamFormat(os.str());
    }
    QDomElement subMatch(pcrxml::firstMatchByTagName(match,subTagName));
    if (subMatch.isNull()) {
      std::ostringstream os;
      os << "expect element '"
         << std::string(tree.tagName().toLatin1())
         << "' to include element '"
         << std::string(subTagName.toLatin1()) << "'";
      throw com::BadStreamFormat(os.str());
    }
    return expectAttr(subMatch,attrName);
  }

  /*! return atttribute value of attribute valueOf of first
   * matching descendant element with tagName \a tag
   */
  static std::string valueOfSubMatch(
      const QDomElement& e,
      const QString& tagName)
  {
    return attrOfSubMatch(e, tagName,"value");
  }

  static double numericValueOfSubMatch(
      const QDomElement& e,
      const QString& tagName)
  {
    try {
      return com::fromString<double>(valueOfSubMatch(e,tagName));
    } catch(const std::range_error& /* re */) {
        std::ostringstream os;
        os << "expect element '"
           << std::string(e.tagName().toLatin1())
           << "' to have numeric attribute value";
        throw com::BadStreamFormat(os.str());
    }
  }

  /*! return atttribute value of attribute value of only
   *  sub element with an identical tag as e
   */
  static std::string subValue(
      const QDomElement& e)
  {
    QDomElement de(pcrxml::firstMatchByTagName(e,e.tagName()));
    if (de.isNull() || !de.hasAttribute("value")) {
      std::ostringstream os;
      os << "expect element '"
         << std::string(e.tagName().toLatin1())
         << "' to include element '"
         << std::string(e.tagName().toLatin1())
         << "' with an attribute named value";
      throw com::BadStreamFormat(os.str());
    }
    return expectAttr(de,"value");
  }

 //! transform Java-JSX xml into something easier (NOT USED)
 /*! \sa http://www.csse.monash.edu.au/~bren/JSX/tech.html
  *  -todo?
  *    how to expand <alias-ref alias="267"/> stuff without
  *     getting into a loop
  */
  class JsxTransform {
    std::set<QString> d_toObjName;
  public:
    JsxTransform() {
      d_toObjName.insert("java.lang.String");
      d_toObjName.insert("java.util.Vector");
    }
    void operator()(QDomElement e) {
      if (d_toObjName.find(e.tagName())!= d_toObjName.end()) {
        // replace element name by value of obj-name
        const char *on("obj-name");
        if (!e.hasAttribute(on))
          throw com::Exception("unexpected format");
        e.setTagName(e.attribute(on));
        e.removeAttribute(on);
      }
      // shorten names from java objects:
      //   nl.wldelft.habitat.Parameter --> Parameter
      //  std::vector<std::string> n(com::split(e.tagName().latin1(),'.'));
      //  if (n.size() > 1)
      //    e.setTagName(n.back().c_str());
    };
  };

  /* find in tree on first match basis an element named \a tag
   * and return a list of its sibling elements
   * \returns
   *   list of element, 0 size if tag not matched or no sibling
   *   elements
   */
  static std::vector<QDomElement> findTagGetSiblings(
     const QDomElement& tree,
     const QString& tag)
  {
   QDomElement parent(pcrxml::firstMatchByTagName(tree,tag));
   if (parent.isNull())
     return std::vector<QDomElement>(); // empty vector
   return pcrxml::childElements(parent);
  }
  /* find in tree on first match basis an element named \a tag
   * and return the first sibling element
   * \returns
   *   the sibling element of element with tag \a tag
   */
  static QDomElement findTagGet1stSibling(
     const QDomElement& tree,
     const QString& tag)
  {
   std::vector<QDomElement> l(findTagGetSiblings(tree,tag));
   if (l.empty())
     return QDomElement();
   return l[0];
  }
  static QDomElement expectTagGet1stSibling(
     const QDomElement& tree,
     const QString& tag)
  {
   QDomElement e(findTagGet1stSibling(tree,tag));
   if (e.isNull()) {
      std::ostringstream os;
      os << "element '"
         << std::string(tag.toLatin1())
         << "' has no sibling";
      throw com::BadStreamFormat(os.str());
   }
   return e;
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF STATIC WLDELFTHABITAT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF WLDELFTHABITAT MEMBERS
//------------------------------------------------------------------------------

/*!
 \todo
   foutmelding als dynamic model met BIL format
   zorg dat calc::IoBandFieldStrategy::createStackReader nooit
   wordt aangeroepen (en  calc::IoBandFieldStrategy::makeStackItemName)
   Of zelfs refactoren met een hasDynamicFieldsCapacity
 */
calc::WlDelftHabitat::WlDelftHabitat(const com::PathName& xmlFile):
     d_xmlFile(xmlFile)
{
  setPositionName(xmlFile.toString());
  appIOstrategy=APP_IO_BANDMAP;
}

calc::WlDelftHabitat::~WlDelftHabitat()
{
}

void calc::WlDelftHabitat::parseParameter(
  const QDomElement&  de)
{
  std::string identifier=expectAttr(de,"identifier");

  QDomElement bestand=pcrxml::firstMatchByTagName(de,"BESTAND");

  // WAS Parameter p(expectAttr(de,"name"),!bestand.isNull());
  Parameter p(valueOfSubMatch(de,"VARIABELE"),!bestand.isNull());

  XML_TRACE_LOG(std::cout << p.d_name << " id:" << identifier);
  if (!bestand.isNull()) {
    std::string fileName(subValue(bestand));
    if (fileName.empty())
      throw com::BadStreamFormat("BESTAND has empty value attribute");
    addBinding(p.d_name,subValue(bestand));
    XML_TRACE_LOG(std::cout << " binding:" << subValue(bestand));
  }
  XML_TRACE_LOG(std::cout << "\n");

  // when p is configured completely, insert copy in table
  d_identifier2Name.insert(std::make_pair(identifier,p));

  d_alreadyDefinedSymbols.insert(p.d_name);
}

void calc::WlDelftHabitat::parseModel(
  const QDomElement&  de)
{
   XML_TRACE_LOG(
     std::cout << "Begin Model:" << expectAttr(de,"name") << "\n");
   parseRules(de,"Rule");
   parseRules(de,"UITVOERFUNCTIE");
}

void calc::WlDelftHabitat::parseRules(
  const QDomElement&  model,
  const std::string& tagName)
{
  RuleName2Tag ruleName2Tag;
   ruleName2Tag.add("Univariaat"      ,"UnivarFunc");   // single sibling has value
   ruleName2Tag.add("Multivariaat"    ,"MultivarFunc"); // single sibling has value
   ruleName2Tag.add("Klassen"         ,"KlassenTabel"); // <KlassenTabel>+ ==> (Variable,Factor)
   ruleName2Tag.add("Gebroken lineair","GebrokenLineair");   // <GebrokenLineair>+ ==> (X,Y)

   std::vector<QDomElement> r(findTagGetSiblings(model,tagName.c_str()));
   for (size_t i=0; i < r.size(); i++) {
    QDomElement soortKennis(expectTagGet1stSibling(r[i],"SoortKennis"));
    RuleName2Tag::const_iterator skR(ruleName2Tag.find(value(soortKennis)));
    if (skR == ruleName2Tag.end()) {
      std::ostringstream os;
      os << "SoortKennis unknown value :'" << value(soortKennis) << "'";
      throw com::BadStreamFormat(os.str());
    }
    if (skR->first == "Univariaat" || skR->first == "Multivariaat") {
      addExpr(r[i],skR->second);
    } else {
      if (skR->first == "Klassen") {
        addKlassen(r[i]);
      } else {
        PRECOND(skR->first == "Gebroken lineair");
        addLookuplinear(r[i]);
      }
    }
   }
}

void calc::WlDelftHabitat::parseXml()
{
  try { //  catch xml processing errors

  pcrxml::Document doc(d_xmlFile);
  QDomElement  de(doc.documentElement());
  POSTCOND(de.tagName() == "Root");

  struct ParseHandler {
    typedef void (WlDelftHabitat::*Parse)(const QDomElement&  de);
    const char *d_elementName;
    Parse       d_parse;
    bool parse(
        WlDelftHabitat *w,
        const QDomElement& de) {
     std::vector<QDomElement> cmds(findTagGetSiblings(de,d_elementName));
     for (size_t i=0; i < cmds.size(); i++)
       (w->*d_parse)(cmds[i]);
     return !cmds.empty();
    }
  };
  ParseHandler parseCommands[]= {
    { "Parameter", &WlDelftHabitat::parseParameter },
    { "Model", &WlDelftHabitat::parseModel },
    { "Ecotoop", &WlDelftHabitat::parseEcotoop },
    { "Statistics", &WlDelftHabitat::parseStatistics }
  };

  parseCommands[0].parse(this,de);
  evaluateBindings();

  bool cmdsFound=false;
  for(size_t i=1;i<ARRAY_SIZE(parseCommands); ++i)
    cmdsFound |= parseCommands[i].parse(this,de);

  if (!cmdsFound)
      throw com::BadStreamFormat("no command element (like Model) found");
 } catch( const com::BadStreamFormat& bsf) {
    throw com::FileFormatError(d_xmlFile,bsf.messages());
 }
}

const calc::WlDelftHabitat::Parameter&
 calc::WlDelftHabitat::parameter(
    const std::string& reference) const
{
  Identifier2Name::const_iterator rn=d_identifier2Name.find(reference);
  if (rn==d_identifier2Name.end()) {
      std::ostringstream os;
      os << "reference :'" << reference << "' not found";
      throw com::BadStreamFormat(os.str());
  }
  return rn->second;
}

const calc::WlDelftHabitat::Parameter&
 calc::WlDelftHabitat::parameter(const QDomElement& reference) const
{
  return parameter(expectAttr(reference,"reference"));
}

const calc::WlDelftHabitat::Parameter&
 calc::WlDelftHabitat::output(
    const QDomElement& rule) const
{
  return(parameter(attrOfSubMatch(rule,"UITVOER","reference")));
}


const calc::WlDelftHabitat::Parameter&
 calc::WlDelftHabitat::input(
    const QDomElement& rule) const
{
  return(parameter(attrOfSubMatch(rule,"INVOER","reference")));
}

void calc::WlDelftHabitat::addAssignment(
    const Parameter&   assignTo,
    const std::string& expr)
{
  XML_TRACE_LOG(std::cout << assignTo.d_name << " = " << expr << "\n");
  addFieldAssignment(assignTo.d_name, addFieldExpr(expr), assignTo.d_report);
}

void calc::WlDelftHabitat::addExpr(
    const QDomElement& rule,
    const std::string& tagWithExpr)
{
  XML_TRACE_LOG(std::cout << "Begin " << tagWithExpr << "\n");
  // inputs are implicit in text string
  addAssignment(output(rule), valueOfSubMatch(rule,tagWithExpr.c_str()));
  XML_TRACE_LOG(std::cout << "End " << tagWithExpr << "\n");
}

namespace calc {

static LookupRecord parseRecord(
    const QDomElement& re,
    const std::string& keyName,
    const std::string& resultName)
{
  std::vector<QDomElement> keys(findTagGetSiblings(re,keyName.c_str()));
  LookupRecord::Key key;
  try {
   for(size_t i=0; i < keys.size(); i++) {
    key.push_back(
        com::createIntervalFromLookupTableKey<double>(
          value(keys[i])));
    XML_TRACE_LOG(std::cout << value(keys[i]) << " ");
   }
  LookupRecord lr(key,numericValueOfSubMatch(re,resultName.c_str()));
  XML_TRACE_LOG(std::cout << "-> " << numericValueOfSubMatch(re,resultName) << "\n");
  LookupRecord::deleteKey(key);
  return lr;
 } catch (...) {
  LookupRecord::deleteKey(key);
  throw;
 }
}

static std::vector<QDomElement> nonEmptyContainer(
    const QDomElement& containedIn,
    const std::string& containerName)
{
  // get all records
  std::vector<QDomElement> recs(findTagGetSiblings(containedIn,containerName.c_str()));
  if (recs.empty()) {
     std::ostringstream msg;
     msg << "empty " << containerName << " is not allowed";
     throw com::BadStreamFormat(msg.str());
  }
  return recs;
}

static void parseTable(
    LookupTable::Records& lr,
    const QDomElement& containedIn,
    const std::string& containerName,
    const std::string& keyName,
    const std::string& resultName)
{
  std::vector<QDomElement> recs(nonEmptyContainer(containedIn,containerName));
  for(size_t i=0; i < recs.size(); i++)
    try {
     lr.push_back(parseRecord(recs[i],keyName,resultName));
    } catch (com::BadIntervalFormat& e) {
       std::ostringstream msg;
       msg << "On parsing " << containerName << ":";
       e.prepend(msg.str());
       throw;
    }
}

static void parseIntervals(
   std::vector<const com::IntervalF *>& iv,
    const QDomElement& containedIn,
    const std::string& containerName)
{
  // get all records
  std::vector<QDomElement> recs(nonEmptyContainer(containedIn,containerName));
  for(size_t i=0; i < recs.size(); i++)
    try {
     iv.push_back(
        com::createIntervalFromLookupTableKey<float>(valueOfSubMatch(recs[i],"VARIABELE")));
    } catch (com::BadIntervalFormat& e) {
       std::ostringstream msg;
       msg << "On parsing " << containerName << ":";
       e.prepend(msg.str());
       throw;
    }
}

static LookupTable* fillTable(
  LookupTable *tab,
  const LookupTable::Records& records,
  VS                          keyVs)  // all keys are equal
{
  PRECOND(!records.empty());
  std::vector<VS> readKeys(records[0].nrKeys(),keyVs);
  tab->setRecords(records,readKeys);
  return tab;
}

}
  
/*! \brief generate an unique id
 *  only works if al external parameters names are already known
 *  a priori
 */
std::string calc::WlDelftHabitat::generatedName(
    const std::string& nameSuggestion)
{
  // create new name
  std::string newName(nameSuggestion+"_Generated");
  if (d_alreadyDefinedSymbols.count(newName)) {
    d_alreadyDefinedSymbols.setPrefix(newName);
    newName=d_alreadyDefinedSymbols.generate();
  }
  d_alreadyDefinedSymbols.insert(newName);
  return newName;
}


/*! add table with an invented table name
 */
std::string calc::WlDelftHabitat::addInlineTable(
    const std::string& nameSuggestion,
          LookupTable  *tab)
{
  const std::string tableName(generatedName(nameSuggestion));
  addLookupTable(tableName,tab);
  return tableName;
}

void calc::WlDelftHabitat::addRuleTableExpr(
    const QDomElement& rule,
    const std::string& function,
          LookupTable  *tab)
{
  std::string tableName=
    addInlineTable(output(rule).d_name,tab);
  std::ostringstream expr;
  expr << function << "("<<tableName<<","<<input(rule).d_name<<")";
  addAssignment(output(rule), expr.str());
}

//! returned pointer only for testing, ptr managed by this
calc::StatTable *calc::WlDelftHabitat::addStatistics(
    const std::string&   subjectName)
{
  StatTable *st=new StatTable(addFieldExpr(subjectName));
  // add intervals/etc
  addStatement(st);
  return st;
}

void calc::WlDelftHabitat::addKlassen(
    const QDomElement& rule)
{
  XML_TRACE_LOG(std::cout << "Begin Klassen\n");
  LookupTable::Records lr;
  parseTable(lr,rule,"KlassenTabel","VARIABELE","FACTOR");
  addRuleTableExpr(rule,"lookupscalar",
      fillTable(new LookupTable(VS_S),lr,VS_N));
  XML_TRACE_LOG(std::cout << "End Klassen\n");
}

void calc::WlDelftHabitat::addLookuplinear(
    const QDomElement& rule)
{
  XML_TRACE_LOG(std::cout << "Begin GebrokenLineair\n");
  LookupTable::Records lr;
  parseTable(lr,rule,"GebrokenLineair","X","Y");
  addRuleTableExpr(rule,"lookuplinear",
      fillTable(new LookupLinear(VS_S),lr,VS_S));
  XML_TRACE_LOG(std::cout << "End GebrokenLineair\n");
}

/*
 * \todo
 *  add test #keys = #ECOTOOPVARIA
 */
void calc::WlDelftHabitat::parseEcotoop(
  const QDomElement&  ecotoop)
{
  XML_TRACE_LOG(std::cout << "Begin Ecotoop\n");

  // create  cmd
  // ECOTOOPUITVOER <-
  //   Expr(lookupnominal(TABLE, ECOTOOPVARIA[0],..,ECOTOOPVARIA[n]))

  //  ECOTOOPUITVOER
  QDomElement outEl=expectTagGet1stSibling(ecotoop,"ECOTOOPUITVOER");
  const Parameter& output = parameter(outEl);

  std::string expr("lookupnominal(");

  // TABLE
  LookupTable::Records lr;
  parseTable(lr,ecotoop,"ECOTOOPCLASS","WAARDEN","VARIABELE");

  std::string tableName= addInlineTable(output.d_name,
                              fillTable(new LookupTable(VS_N),lr,VS_S));
  expr += tableName + ",";

  // ECOTOOPVARIA[0],..,ECOTOOPVARIA[n]
  std::vector<QDomElement> inputData(findTagGetSiblings(ecotoop,"ECOTOOPVARIA"));
  if (inputData.size() != lr[0].nrKeys())
     throw com::BadStreamFormat(
         "number of ECOTOOPVARIA and WAARDEN elements not identical");

  std::vector<std::string> arg;
  for(size_t v=0;v<inputData.size(); v++)
    arg.push_back(parameter(inputData[v]).d_name);
  expr += com::join(arg,",")+ ")";

  addAssignment(output, expr);
  XML_TRACE_LOG(std::cout << "End Ecotoop\n");

}

void calc::WlDelftHabitat::parseStatistics(const QDomElement&  de)
{
  XML_TRACE_LOG(std::cout << "Begin Statistics\n");

  QDomElement e;

  // Required
  e=expectTagGet1stSibling(de,"OnderwerpKaarten");
  const Parameter& onderwerp = parameter(e);

  e=findTagGet1stSibling(de,"ZoomGebruik");
  StatTable *s;
  if (e.isNull() || value(e) != "true" ) { // Optional
   s=addStatistics(onderwerp.d_name);
  }  else {
    std::ostringstream maskCmd;
    std::string maskedMap(generatedName(onderwerp.d_name));
    maskCmd << maskedMap << " = "
            << " if( ycoordinate(1) >="<< numericValueOfSubMatch(de,"ZoomYmin")
            << " and ycoordinate(1) <="<< numericValueOfSubMatch(de,"ZoomYmax")
            << " and xcoordinate(1) >="<< numericValueOfSubMatch(de,"ZoomXmin")
            << " and xcoordinate(1) <="<< numericValueOfSubMatch(de,"ZoomXmax")
            << " then " << onderwerp.d_name << ")";

    XML_TRACE_LOG(std::cout << "Zoom " << maskCmd.str() << "\n");
    addStatement(maskCmd.str(),false);
    s=addStatistics(maskedMap);
  }

  s->setSubjectName(onderwerp.d_name);
  s->setResultTable(valueOfSubMatch(de,"UitvoerTabel"));

  XML_TRACE_LOG(std::cout << "UitvoerTabel: " << s->resultTable() << "\n");

  e=findTagGet1stSibling(de,"IndelingsKaarten");
  if (!e.isNull()) { // Optional
    const Parameter& c = parameter(e);
    s->setCross(addFieldExpr(c.d_name));
    s->setCrossName(c.d_name);
  }

  struct IntervalParser {
    std::vector<const com::IntervalF *> d_iv;
    IntervalParser(const QDomElement& de, const char *name) {
      QDomElement e=pcrxml::firstMatchByTagName(de,name);
      if (!e.isNull())
         parseIntervals(d_iv,de,name);
    }
    ~IntervalParser() {
      com::deleteCloneContainer(d_iv);
    }
    bool parsed() const { return !d_iv.empty(); }
  };
  { // Optional
    IntervalParser ip(de,"OnderwerpKlassen");
    if (ip.parsed())
     s->setSubjectIntervals(ip.d_iv);
  }
  { // Optional
    IntervalParser ip(de,"IndelingsKlassen");
    if (ip.parsed())
     s->setCrossIntervals(ip.d_iv);
  }

  XML_TRACE_LOG(std::cout << "End Statistics\n");
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
