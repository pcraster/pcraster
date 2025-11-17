#ifndef INCLUDED_PCRXML_DOM
#define INCLUDED_PCRXML_DOM

#include "stddefx.h"

#include <qdom.h>

#include <iostream>
#include <vector>


namespace pcrxml {
  // Dom declarations.
}



namespace pcrxml {


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

QDomElement firstMatchByTagName(
    const QDomElement& tree,
    const QString& tagName);

std::vector<QDomElement> matchByTagName(
    const QDomElement& tree,
    const QString& tagName);

std::vector<QDomElement> childElements(
    const QDomElement& parent);

std::vector<QDomElement>
 childrenByTagName(
    const QDomElement& parent,
    const QString&     tagName);

QDomElement firstChildByTagName(
    const QDomElement& parent,
    const QString&     tagName);


void changeAttrName(
          QDomElement& e,
    const QString&     oldName,
    const QString&     newName);

QString textOnlyContents(
          QDomElement e);


} // namespace pcrxml

std::ostream& operator<<(std::ostream& stream, const QDomElement& e);

#endif
