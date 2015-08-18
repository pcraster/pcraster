#ifndef INCLUDED_PCRXML_DOM
#define INCLUDED_PCRXML_DOM

/*!
  \file
  Extends QDom with some usefull functions
*/


#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.

// Module headers.



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
