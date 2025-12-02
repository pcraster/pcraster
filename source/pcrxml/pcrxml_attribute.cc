#include "stddefx.h"
#include "pcrxml_attribute.h"
#include "com_exception.h"
#include "pcrxml_stringconv.h"

#include <QDomDocument>

/*!
  \file
  This file contains the implementation of the Attribute class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ATTRIBUTE MEMBERS
//------------------------------------------------------------------------------

//! return node representing the attribute node, node isNull if not present
static QDomNode attrNode(const QDomNode &owningElement, const std::string &nameOfAttr)
{
  PRECOND(owningElement.nodeType() == QDomNode::ElementNode);
  QDomNamedNodeMap const map(owningElement.attributes());
  return map.namedItem(pcrxml::asQString(nameOfAttr));
}

//! returns the string value of the attribute as retrieved from the DomNode representation
/*! constructors of specialized classes use this one to retrieve value
 * precondition is that the value must be present()
 */
std::string pcrxml::Attribute::inputValueStr(const QDomNode &owningElement,
                                             const std::string &nameOfAttr)
{
  QDomNode const a(attrNode(owningElement, nameOfAttr));
  POSTCOND(!a.isNull());
  return asString(a.nodeValue());
}

//------------------------------------------------------------------------------
// DEFINITION OF ATTRIBUTE MEMBERS
//------------------------------------------------------------------------------

//! checks if a required string is really present
pcrxml::Attribute::Attribute(const QDomNode &owningElement, const std::string &nameOfAttr, bool required)
{
  QDomNode const a(attrNode(owningElement, nameOfAttr));
  d_present = !a.isNull();
  if (!present() && required)
    throw com::BadStreamFormat("expected attribute '" + nameOfAttr + "' as part of element " +
                               asString(owningElement.nodeName()));
}

//! default ctor of attribute that is not present
pcrxml::Attribute::Attribute(bool present) : d_present(present)
{
}

//! dtor
pcrxml::Attribute::~Attribute()
{
}

//! add attribute with name \a name having my value to element \a e
void pcrxml::Attribute::addToElement(QDomElement &e, const std::string &name) const
{
  if (present())
    e.setAttribute(asQString(name), asQString(attrValueStr()));
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
