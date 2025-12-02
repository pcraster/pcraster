/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_script.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::Script::d_elementName("Script");

//! ctor
pcrxml::Script::Script(const QDomElement &element)
    : Element(element, d_elementName), scriptFileName(element, "scriptFileName", false),
      scriptType(element, "scriptType", true), ioStrategy(element, "ioStrategy", false)
{
  try {
    ChildElementVisitor v(element);

    // optional element
    if (v.currentChildEq("integerTimer")) {
      integerTimer = new IntegerTimer(v.processChild());
    }
    // required element
    v.checkRequiredChild("ScriptData");
    scriptData = new ScriptData(v.processChild());
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::Script::Script() : Element()

{
}

const std::string &pcrxml::Script::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::Script::~Script()
{
  clean();
}

//! clean
void pcrxml::Script::clean()
{
  delete integerTimer;
  integerTimer = nullptr;
  delete scriptData;
  scriptData = nullptr;
}

//! copy ctor
pcrxml::Script::Script(const Script &src)
    : pcrxml::Element(src), scriptFileName(src.scriptFileName), scriptType(src.scriptType),
      ioStrategy(src.ioStrategy)
{
  integerTimer = (src.integerTimer) ? new IntegerTimer(*(src.integerTimer)) : nullptr;
  scriptData = new ScriptData(*(src.scriptData));
}

//! assignment operator
pcrxml::Script &pcrxml::Script::operator=(const Script &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    integerTimer = (src.integerTimer) ? new IntegerTimer(*(src.integerTimer)) : nullptr;
    scriptData = new ScriptData(*(src.scriptData));
  }
  return *this;
}

void pcrxml::Script::fill(QDomElement el) const
{
  scriptFileName.addToElement(el, "scriptFileName");
  scriptType.addToElement(el, "scriptType");
  ioStrategy.addToElement(el, "ioStrategy");
  if (integerTimer) {
    integerTimer->appendTo(el);
  }
  if (scriptData) {
    scriptData->appendTo(el);
  }
}
