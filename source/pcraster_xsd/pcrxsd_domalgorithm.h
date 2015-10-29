#ifndef INCLUDED_PCRXSD_ALGORITHM
#define INCLUDED_PCRXSD_ALGORITHM

/*
 *  PORTED FROM PCRXML,
 *  dunno if we really need it
 */

// #ifndef INCLUDED_STDDEFX
// #include "stddefx.h"
// #define INCLUDED_STDDEFX
// #endif
// Library headers.
#include <cassert>
#include <xercesc/dom/DOMNodeList.hpp>
#include <xercesc/dom/DOMElement.hpp>
#include <xercesc/dom/DOMNamedNodeMap.hpp>
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

// toString only
#ifndef INCLUDED_PCRXSD_UTILS
#include "pcrxsd_utils.h"
#define INCLUDED_PCRXSD_UTILS
#endif
// PCRaster library headers.

// Module headers.


namespace pcrxml {

typedef XERCES_CPP_NAMESPACE::DOMNode     DOMNode;
typedef XERCES_CPP_NAMESPACE::DOMNodeList DOMNodeList;
typedef XERCES_CPP_NAMESPACE::DOMElement  DOMElement;
typedef XERCES_CPP_NAMESPACE::DOMNamedNodeMap  DOMNamedNodeMap;

//! function object for ordering nodes on Node::getNodeName()
struct NodeNameLess {
  bool operator()(const DOMNode* n1, const DOMNode* n2) const
  {
     assert(n1);
     assert(n2);
    return pcrxsd::toString(n1->getNodeName()) < pcrxsd::toString(n2->getNodeName());
  }
};

//! set of Nodes ordered on Node::getNodeName
class NodeSet :
  public std::set<DOMNode *,NodeNameLess>
{
  public:
    NodeSet(const DOMNamedNodeMap* map) {
     if (!map)
        return;
     for(size_t i=0; i < map->getLength(); ++i) {
        insert(map->item(i));
      }
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
 * \param o    operation function object with operator()(DOMNode n) to
 *             call for each node.
 */
void forEachNode(DOMNode* node, Operation& o) {
  assert(node);
  o(node);
  NodeSet nodeSet(node->getAttributes());
  for(NodeSet::iterator i=nodeSet.begin(); i != nodeSet.end(); ++i)
    o(*i);
  DOMNodeList* list = node->getChildNodes();
  assert(list);
  for(size_t i=0; i < list->getLength(); ++i)
    forEachNode(list->item(i),o);
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
 * \param o operation function object with operator()(DOMElement e) to
 *          call for each element.
 */
template <class Operation>
 void forEachElement(DOMElement* e, Operation& o) {
  assert(e);
  o(e);
  DOMNodeList* list = e->getChildNodes();
   for(size_t i=0; i < list->getLength(); i++)
     if (list->item(i)->getNodeType() == DOMNode::ELEMENT_NODE)
       forEachElement(dynamic_cast<DOMElement *>(list->item(i)),o);
}

/*!
 * Visit all child elements of \a e in sibling order
 * Altering the element tree with \a o is undefined
 *
 * \param e xml element
 * \param o operation function object with operator()(DOMElement e) to
 *          call for each element.
 */
template <class Operation>
 void forEachChildElement(DOMElement* e, Operation& o) {
 DOMNodeList* list = e->getChildNodes();
 for(size_t i=0; i < list->getLength(); i++)
     if (list->item(i)->getNodeType() == DOMNode::ELEMENT_NODE)
       o(dynamic_cast<DOMElement *>(list->item(i)));
}
}

#endif
