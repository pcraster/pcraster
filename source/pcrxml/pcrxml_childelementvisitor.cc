#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#endif

#ifndef INCLUDED_PCRXML_STRINGCONV
#include "pcrxml_stringconv.h"
#define INCLUDED_PCRXML_STRINGCONV
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

/*!
  \file
  This file contains the implementation of the ChildElementVisitor class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CHILDELEMENTVISITOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CHILDELEMENTVISITOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
/*! Constructor initializes visitor, the first call to processChild yields the
    first child element.
    \a elem should not be isNull
 */
pcrxml::ChildElementVisitor::ChildElementVisitor(const QDomNode& elem):
  d_elem(elem),
  d_children(elem.childNodes()),
  d_current(0)
{
  POSTCOND(!d_elem.isNull());
  advance();
}

//! dtor
pcrxml::ChildElementVisitor::~ChildElementVisitor()
{
}

//! set d_current to next element node or past end of elements
void pcrxml::ChildElementVisitor::advance()
{
  for(  ;!allChildrenProcessed(); d_current++) {
    if (currentChild().isElement())
      break;
  }
  POSTCOND(allChildrenProcessed() || currentChild().isElement());
}

//! check if all elements are processed
bool pcrxml::ChildElementVisitor::allChildrenProcessed() const
{
  return d_current >= d_children.length();
}

//! return current child and advance to next
QDomElement pcrxml::ChildElementVisitor::processChild()
{
  PRECOND(!allChildrenProcessed());
  unsigned int thisOne(d_current);
  d_current++;
  advance();
  PRECOND(d_children.item(thisOne).isElement());
  return  d_children.item(thisOne).toElement();
}

/*! returns true if current element has name \a childName, false if not or no
    more nodes present.
 */
bool pcrxml::ChildElementVisitor::currentChildEq(const std::string& childName) const
{
  if (allChildrenProcessed())
    return false;
  return currentChild().nodeName() == asQString(childName);
}

//! part of validation
/*! \throws com::BadStreamFormat if next element is not \a childName
*/
void pcrxml::ChildElementVisitor::checkRequiredChild(const std::string& childName) const
{
  if (!currentChildEq(childName))
    throw com::BadStreamFormat("expected child element "+childName+" as part of element "
                                +asString(d_elem.nodeName()));
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



