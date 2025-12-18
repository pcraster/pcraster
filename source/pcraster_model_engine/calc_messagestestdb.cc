#include "stddefx.h"
#include "calc_messagestestdb.h"
#include "com_pathname.h"
#include "com_strlib.h"
#include "pcrxml_document.h"
#include "pcrxml_domalgorithm.h"
#include "pcrxml_dom.h"
#include "com_exception.h"
#include "com_file.h"
#include "calc_stringparser.h"

#include <functional>
#include <iostream>
#include <map>

/*!
  \file
  This file contains the implementation of the MessagesTestDB class.
*/


//------------------------------------------------------------------------------

namespace calc
{

std::unique_ptr<MessagesTestDB> MessagesTestDB::d_instance;

namespace detail
{
struct AddError {
  std::string reason;
  std::string text;
};

class DBCol
{
  const std::string d_name;

public:
  DBCol(const std::string &name) : d_name(name)
  {
  }

  const std::string &name() const
  {
    return d_name;
  }

  std::string toString(const std::string &id, const QDomElement &e) const
  {
#ifdef WIN32
    // Somehow Qt(?!) adds an additional CR in multiline elements
    std::string s = std::string(pcrxml::textOnlyContents(e).toLatin1().replace("\r", ""));
#else
    std::string s = std::string(pcrxml::textOnlyContents(e).toLatin1());
#endif
    com::removeFrontEndSpace(s);
    if (s.empty()) {
      detail::AddError ae;
      ae.reason = "EMPTY id:" + id;
      ae.text = s;
      throw ae;
    }
    return s;
  }
};
}  // namespace detail

class MessagesTestDBPrivate :
    //                       id,test
    public std::map<std::string, QDomElement>
{
  detail::DBCol d_dbMsg;
  detail::DBCol d_dbModel;
  detail::DBCol d_dbAst;

  QDomElement d_root;


  mutable std::ostringstream d_dump;

  void writeDump(const std::string &id, const std::string &reason) const
  {
    d_dump << "id: " << id << " " << reason << '\n';

    // write into dumpmsg.txt
    com::write(d_dump.str(), "dumpmsg.txt");
    d_dump.clear();

    std::cerr << "./dumpmsg.txt:1:1:" << reason << '\n';
  }

  // std::vector<QDomElement> c(pcrxml::childElements(e));
  // PRECOND(c.size()==1);

  void addTest(const std::string &id, const QDomElement &e)
  {
    PRECOND(e.tagName() == "test");
    std::string const idS = attrStr(e, "id");
    PRECOND(!idS.empty());

    auto notYetPresent = find(id);
    if (notYetPresent != end()) {
      detail::AddError ae;
      ae.reason = "DUPLICATE";
      ae.text = id;
      throw ae;
    }
    insert(std::make_pair(id, e));
  }

  static void writeResFile(const std::string &id, const std::string &result)
  {
    com::PathName pn(id);
    std::string contents(result);
    if (contents.size() != 0u) {
      // reading in trims last end-of-line needed
      if (contents[contents.size() - 1] != '\n') {
        contents += "\n";
      }
    }
    pn.setExtension("res");
    com::write(contents, pn);
  }

public:
  void operator()(const QDomElement &e)
  {
    if (e.tagName() != "test") {
      return;  // skip makefile
    }
    std::string const id = attrStr(e, "id");
    PRECOND(!id.empty());


    try {
      std::vector<QDomElement> const c(pcrxml::childElements(e));
      addTest(id, e);
      // check non-emptyness now
      for (auto &i : c) {
        if (i.tagName() == QString(d_dbMsg.name().c_str())) {
          std::string const result = d_dbMsg.toString(id, i);
          if (i.hasAttribute("resFile")) {
            writeResFile(id, result);
          }
        }
        if (i.tagName() == QString(d_dbModel.name().c_str())) {
          (void)d_dbModel.toString(id, i);
        }
      }
    } catch (const detail::AddError &r) {
      d_dump << "messagestest.xml: " << r.text << "\n";
      writeDump(id, r.reason);
    }
  }

  MessagesTestDBPrivate() : d_dbMsg("msg"), d_dbModel("model"), d_dbAst("ast")
  {  // copy is made in testdir
    pcrxml::Document const doc(com::PathName("messagestest.xml"));
    d_root = doc.documentElement();
    pcrxml::forEachChildElement(d_root, *this);
  }

  ~MessagesTestDBPrivate()
  {
  }

  //! return sub element subElem of id, Null if not found
  QDomElement findE(const detail::DBCol &subElem, const std::string &id) const
  {
    auto i = find(id);
    if (i != end()) {
      return pcrxml::firstMatchByTagName(i->second, QString(subElem.name().c_str()));
    }
    return {};
  }

  //! return sub element subElem of id, empty string if not found
  std::string findS(const detail::DBCol &subElem, const std::string &id) const
  {
    QDomElement const e(findE(subElem, id));
    if (e.isNull()) {
      return "";
    }
    return subElem.toString(id, e);
  }

  bool dumpDiff(const std::string &id, const std::string &expected, const std::string &got) const
  {
    // write a dump for easy editing
    d_dump << "messagestest.xml: " << expected << "\n"
           << "unittest        : " << got << "\n";
    writeDump(id, "DIFFERENCE");
    return false;
  }

  bool equals(const std::string &id, const std::string &resultOfUnitTest) const
  {
    std::string const msg = findS(d_dbMsg, id);
    if (msg.empty()) {
      d_dump << "NOT PRESENT IN messagetest.xml\n"
             << "unittest        : " << resultOfUnitTest << "\n";
      writeDump(id, "ABSENCE");
      return false;
    }

    QDomElement const e(findE(d_dbMsg, id));
    std::string cmpTo(resultOfUnitTest);
    if (e.hasAttribute("replace")) {
      // order matters, \ and / as last ones!

      cmpTo = com::replaceCharByStr(cmpTo, '/', "X");
      cmpTo = com::replaceCharByStr(cmpTo, '\\', "X");
    }

    if (msg == cmpTo) {
      return true;
    }
    return dumpDiff(id, msg, cmpTo);
  }

  QDomElement xml(const std::string &id) const
  {
    QDomElement const ast = findE(d_dbAst, id);
    if (ast.isNull()) {
      d_dump << "AST NOT PRESENT IN messagetest.xml \n";
      writeDump(id, "ABSENCE");
    }
    std::vector<QDomElement> c(pcrxml::childElements(ast));
    return c[0];
  }

  std::string attrStr(QDomElement const &e, const char *name) const
  {
    return std::string(e.attribute(name, "").toLatin1());
  }

  std::string options(const std::string &id) const
  {
    QDomElement const e = findE(d_dbModel, id);
    return attrStr(e, "options");
  }

  bool hasXML(const std::string &id) const
  {
    return !findE(d_dbAst, id).isNull();
  }

  std::string model(const std::string &id) const
  {
    std::string s = findS(d_dbModel, id);
    if (s.empty()) {
      d_dump << "NOT PRESENT IN messagetest.xml:\n"
             << "element model\n";
      writeDump(id, "ABSENCE");
      return id;
    }
    return s;
  }
};

}  // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MESSAGESTESTDB MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF MESSAGESTESTDB MEMBERS
//------------------------------------------------------------------------------

calc::MessagesTestDB::MessagesTestDB()
{
  try {
    d_data = new MessagesTestDBPrivate();
  } catch (const com::Exception &e) {
    bool const badFormatOfXML = false;
    std::cerr << e.messages();
    PRECOND(badFormatOfXML);
    (void)badFormatOfXML;  // Shut up compiler
  }
}

calc::MessagesTestDB *calc::MessagesTestDB::instance()
{
  if (d_instance.get() == nullptr) {
    d_instance.reset(new MessagesTestDB());
  }
  return d_instance.get();
}

calc::MessagesTestDB::~MessagesTestDB()
{
  delete d_data;
}

//! check if \a e equals the msg element known for \a id
bool calc::MessagesTestDB::equals(const std::string &id, const com::Exception &e,
                                  const std::string &prefix) const
{
  std::string cmpTo(e.messages());
  com::removeFrontEndSpace(cmpTo);
  return d_data->equals(id, prefix + cmpTo);
}

//! check if \a fileCreated is created and content equals contents of id.res
bool calc::MessagesTestDB::equalsFile(const std::string &id, const std::string &fileCreated) const
{
  com::PathName res(id);
  res.setExtension("res");
  if (com::filesExistsAndEqual(res.toString(), fileCreated)) {
    return true;
  }
  std::string expect;
  std::string got;
  com::read(expect, res);
  com::read(got, fileCreated);

  // TODO refactor differing on begin and end space
  com::removeFrontEndSpace(expect);
  com::removeFrontEndSpace(got);
  if (expect != got) {
    return d_data->dumpDiff(id, expect, got);
  }
  return true;
}

//! return model for \a id
std::string calc::MessagesTestDB::model(const std::string &id) const
{
  return d_data->model(id);
}

//! return options for \a id, empty if non
std::string calc::MessagesTestDB::options(const std::string &id) const
{
  return d_data->options(id);
}

QDomElement calc::MessagesTestDB::xml(const std::string &id) const
{
  return d_data->xml(id);
}

bool calc::MessagesTestDB::hasXML(const std::string &id) const
{
  return d_data->hasXML(id);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
