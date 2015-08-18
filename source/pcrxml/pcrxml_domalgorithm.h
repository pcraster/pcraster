#ifndef INCLUDED_PCRXML_ALGORITHM
#define INCLUDED_PCRXML_ALGORITHM



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
// PCRaster library headers.

// Module headers.


namespace pcrxml {

//! function object for ordering nodes on Node::nodeName()
struct NodeNameLess {
  bool operator()(const QDomNode& n1, const QDomNode& n2) const
  {
    return n1.nodeName() < n2.nodeName();
  }
};

//! set of Nodes ordered on Node::nodeName
class NodeSet :
  public std::set<QDomNode,NodeNameLess>
{
  public:
    NodeSet(const QDomNamedNodeMap& map) {
      for(size_t i=0; i < (size_t)map.count(); ++i)
        insert(map.item(i));
    }
};

template <class Operation>
/*!
 * Visit all nodes, elements, attributes, PI's, etc, and all
 * its subNodes in prefix order: first
 * the node itself than its children. This allows for
 * alternating the tree in the operation \a o. If \a o
 * deletes nodes then this nodes are not visited, if \a
 * o insert nodes these node are visited.
 *
 * Attributes are visited in alfabetic order and before sub elements
 *
 * \param node tree root of xml fragment
 * \param o    operation function object with operator()(QDomNode n) to
 *             call for each node.
 */
void forEachNode(QDomNode node, Operation& o) {
  o(node);
  NodeSet attrs(node.attributes());
  for(NodeSet::iterator i=attrs.begin(); i != attrs.end(); ++i)
    o(*i);
  QDomNodeList list = node.childNodes();
  for(size_t i=0; i < (size_t)list.count(); ++i)
    forEachNode(list.item(i),o);
}

/*!
 * Visit all elements and all
 * its sub elements in prefix order: first
 * the element itself than its children. This allows for
 * alternating the tree in the operation \a o. If \a o
 * deletes elements then these elements are not visited, if \a
 * o insert elements these elements are visited.
 *
 * \param e xml element
 * \param o operation function object with operator()(QDomElement e) to
 *          call for each element.
 */
template <class Operation>
 void forEachElement(QDomElement e, Operation& o) {
  o(e);
  QDomNodeList list = e.childNodes();
   for(size_t i=0; i < (size_t)list.count(); i++)
     if (list.item(i).isElement())
       forEachElement(list.item(i).toElement(),o);
}

/*!
 * Visit all child elements of \a e in sibling order
 * Altering the element tree with \a o is undefined
 *
 * \param e xml element
 * \param o operation function object with operator()(QDomElement e) to
 *          call for each element.
 */
template <class Operation>
 void forEachChildElement(QDomElement e, Operation& o) {
 QDomNodeList list = e.childNodes();
   for(size_t i=0; i < (size_t)list.count(); i++)
     if (list.item(i).isElement())
       o(list.item(i).toElement());
}
}

#endif
