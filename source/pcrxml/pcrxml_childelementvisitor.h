#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef  INCLUDED_QDOM
#include <qdom.h>
#define  INCLUDED_QDOM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

class QDomNode;

namespace pcrxml {

//! visit child elements in sequential order
/*!
  This class keeps track of a current child element, and is able to iterate over
  all child elements. Child elements are all sub nodes of type QDomNode::ElementNode, other
  node types are skipped in the visitor.
  \todo
    move into or integratie with pcrxml_domalgorithm.h stuff
*/

class ChildElementVisitor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ChildElementVisitor&           operator=           (const ChildElementVisitor&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ChildElementVisitor               (const ChildElementVisitor&);

  //! the element whose children are visited here
  const QDomNode      d_elem;

  //! list with all children of \a d_elem, including non-element nodes
  const QDomNodeList  d_children;

  //! index into \a d_children
  uint                d_current;

  void                advance();
  bool                allChildrenProcessed() const;

  QDomNode             currentChild()         const {
    PRECOND(d_current < d_children.length());
    return d_children.item(d_current);
  }

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ChildElementVisitor              (const QDomNode& e);

  /* virtual */   ~ChildElementVisitor              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  QDomElement processChild();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool currentChildEq(const std::string& childName) const;

  void checkRequiredChild(const std::string& childName) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace pcrxml

#endif
