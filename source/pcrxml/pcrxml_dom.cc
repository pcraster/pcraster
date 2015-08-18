#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_DOM
#include "pcrxml_dom.h"
#define INCLUDED_PCRXML_DOM
#endif

// Library headers.
#ifndef INCLUDED_QTEXTSTREAM
#include <qtextstream.h>
#define INCLUDED_QTEXTSTREAM
#endif


// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_PCRXML_DOMALGORITHM
#include "pcrxml_domalgorithm.h"
#define INCLUDED_PCRXML_DOMALGORITHM
#endif



/*!
  \file
  This file contains the implementation of the Dom class.
*/



//------------------------------------------------------------------------------

namespace pcrxml {
 namespace Private {
  struct ChildrenByTagName {
    QString d_tagName;
    std::vector<QDomElement> d_result;
    ChildrenByTagName(const QString& tagName):
       d_tagName(tagName)
    {
    }
    void operator()(const QDomElement& el) {
      if (el.tagName() == d_tagName)
        d_result.push_back(el);
    }
  };

 }
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//! find first element with name \a tagName
/*!
 * it will find the first one in preorder traversal \a tree, as in
 * DOM tree.elementsByTagName method. The match is a descendant of tree
 *
 * \param   tree    searc descendant elements of this parameter
 * \param   tagName the name to search for
 * \pre     tagName not empty
 * \returns the element found, if not found a null element
 */
QDomElement pcrxml::firstMatchByTagName(
    const QDomElement& tree,
    const QString& tagName)
{
  // find descendant elements
  QDomNodeList mrsList(tree.elementsByTagName(tagName));

  // get first one in preorder traversal
  if (mrsList.count() != 0) {
        PRECOND(!mrsList.item(0).toElement().isNull());
        return mrsList.item(0).toElement();
  }

  return QDomElement();
}

//! find elements with name \a tagName
/*!
 * find all elements in preorder traversal \a tree, as in
 * DOM tree.elementsByTagName method. The match is a descendant of tree
 *
 * \param   tree    searc descendant elements of this parameter
 * \param   tagName the name to search for
 * \pre     tagName not empty
 * \returns vector of elements, can be empty if none found
*  \sa      pcrxml::firstMatchByTagName()
 */
std::vector<QDomElement>
 pcrxml::matchByTagName(
    const QDomElement& tree,
    const QString& tagName)
{
  QDomNodeList mrsList(tree.elementsByTagName(tagName));
  std::vector<QDomElement> v;
  for(size_t i=0; i < (size_t)mrsList.count(); i++)
       v.push_back(mrsList.item(i).toElement());
  return v;
}

/*! return a list of child elements
 */
std::vector<QDomElement> pcrxml::childElements(
    const QDomElement& parent)
{
 QDomNodeList nodes(parent.childNodes());
  std::vector<QDomElement> v;
  for(size_t i=0; i < (size_t)nodes.count(); i++)
    if (nodes.item(i).isElement())
       v.push_back(nodes.item(i).toElement());
  return v;
}

/*! change an attribute name of \a e from \a oldName to \a newName, while
 * remaing the same attribute value
 * if \a e does not have an attribute with name \a oldName nothing
 * is done.
 */
void pcrxml::changeAttrName(
          QDomElement& e,
    const QString&     oldName,
    const QString&     newName)
{
  QString attrValue = e.attribute(oldName);
  if (attrValue.isNull())
    return;
  e.removeAttribute(oldName);
  e.setAttribute(newName,attrValue);
}

//! \a e is an element with only text (PCDATA)
/*! e will be normalized and its contents is returned as string
 *  if the element is empy, an empty string is returned
 * \pre
 *   e does not contains sub-elements or CDATA sections
 * \todo
 *  code is very equal to
 *    pcrxml::ElementCDATASection::ElementCDATASection(QDomNode n)
 */
QString pcrxml::textOnlyContents(
          QDomElement e)
{
  e.normalize();
  QDomNodeList c(e.childNodes());
  if (!c.length())
    return "";
  PRECOND(c.length() == 1 && c.item(0).isCharacterData());
  return c.item(0).nodeValue();
}

//! find child elements with name \a tagName
/*!
 * \param   parent  parent to search for children with \a tagName
 * \param   tagName the name to search for
 * \pre     tagName not empty
 * \returns vector of elements, can be empty if none found
*  \sa      pcrxml::matchByTagName()
 */
std::vector<QDomElement>
 pcrxml::childrenByTagName(
    const QDomElement& parent,
    const QString& tagName)
{
  PRECOND(!tagName.isEmpty());


  Private::ChildrenByTagName fo(tagName);
  forEachChildElement(parent,fo);
  return fo.d_result;
}

//! find first child element with name \a tagName
/*!
 * \param   parent  parent of child to search for
 * \param   tagName the name of the child to search for
 * \pre     tagName not empty
 * \returns the element found, if not found a null element
 */
QDomElement pcrxml::firstChildByTagName(
    const QDomElement& parent,
    const QString& tagName)
{
  std::vector<QDomElement> els(childrenByTagName(parent,tagName));
  if (els.empty())
     return QDomElement();
  return els[0];
}

//! stream element contents
std::ostream& operator<<(std::ostream& stream, const QDomElement& e)
{
        QString      result;
        QTextStream pr(&result);
        pr << e << "\n";
        stream << std::string(result.toLatin1());
        return stream;
}
