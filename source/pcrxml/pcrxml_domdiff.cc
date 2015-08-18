#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_DOMDIFF
#include "pcrxml_domdiff.h"
#define INCLUDED_PCRXML_DOMDIFF
#endif

// Library headers.
#ifndef INCLUDED_LIST
#include <list>
#define INCLUDED_LIST
#endif
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_QTEXTSTREAM
#include <qtextstream.h>
#define INCLUDED_QTEXTSTREAM
#endif
#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_PCRXML_DOMALGORITHM
#include "pcrxml_domalgorithm.h"
#define INCLUDED_PCRXML_DOMALGORITHM
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the DomDiff class.
*/



//------------------------------------------------------------------------------

namespace pcrxml {
namespace Private {

/*!
 * NodeList does the stuff for diffing
 * idea:
 *  - put two in list compare the list
 *  - delete empty text nodes
 *  - compare node by node
 *  - on offending node mark or throw something
 *  .
 */
struct NodeList : public std::list<QDomNode> {
   void operator()(QDomNode n) {
     push_back(n);
   }
/*
 *  WANT THIS  but ended up with create() below
   NodeList(QDomNode n) {
    forEachNode(n,std::ptr_fun(this->push_back));
     nl.deleteEmptyTextNodes();
   }
*/
   static bool emptyTextNode(QDomNode n) {
     QDomText t= n.toText();
     if (t.isNull())
       return false;
     QString contents=t.nodeValue().trimmed();
     return contents.isEmpty();
   }
   void deleteEmptyTextNodes() {
     remove_if(NodeList::emptyTextNode);
   }
   static NodeList create(QDomNode n) {
     NodeList nl;
     forEachNode(n,nl);
     // TODO seems empty are already deleted
     nl.deleteEmptyTextNodes();
     return nl;
   }
   bool equal(
       const NodeList& nl2, bool throwOnDiff)
   {
     typedef NodeList::const_iterator I;

     struct Equal {
       bool               d_throwOnDiff;
       void print(QDomNode n, std::ostringstream& s) const {
         QString str;
         QTextStream qs(&str);
         qs << "nodeName: "   << n.nodeName()  <<
               " nodeValue: " << n.nodeValue() <<
               " nodeType: "  << n.nodeType()  <<
               " nodeAsXml (next line):\n" << n << "\n";
         s << std::string(str.toLatin1());
       }
       bool error    (const std::string& msg) const {
         if (d_throwOnDiff)
           throw com::Exception(msg);
         return false;
       }
       //! conditional error
       bool operator ()(bool eq, I n1, I n2, const char* cause) const {
         if (eq)
           return true;
         std::ostringstream ostr;
         ostr << "DomDiff difference on: " << cause << std::endl;
         print(*n1,ostr);
         print(*n2,ostr);
         return error(ostr.str());
       }
     } equal; equal.d_throwOnDiff=throwOnDiff;

     I ni =    begin();
     I ni2=nl2.begin();
     for(  ; ni!=end() && ni2!=nl2.end(); ++ni,++ni2) {
       if (!equal(ni->nodeName()== ni2->nodeName(),ni,ni2,"nodeName"))
         return false;
       if (!equal(ni->nodeValue()== ni2->nodeValue(),ni,ni2,"nodeValue"))
         return false;
       if (!equal(ni->nodeType()== ni2->nodeType(),ni,ni2,"nodeType"))
         return false;
      /* print all nodes
       std::ostringstream s;
       equal.print(*ni,s);
       equal.print(*ni2,s);
       std::cerr << s.str() << std::endl;
      */
         /*enum QDomNode::NodeType:
           ElementNode = 1
           AttributeNode = 2
           TextNode = 3
           CDATASectionNode = 4
           EntityReferenceNode = 5
           EntityNode = 6
           ProcessingInstructionNode = 7
           CommentNode = 8
           DocumentNode = 9
           DocumentTypeNode = 10
           DocumentFragmentNode = 11
           NotationNode = 12
           BaseNode = 21
           CharacterDataNode = 22
         */
     }
     if (empty())
       return equal.error("this node list is empty");
     if (nl2.empty())
       return equal.error("node list 2 is empty");
/*
   extensive log of nrOfNodes difference
   compare size, on error print last node contents

     if (ni != end() || ni2 != nl2.end()) {
       std::ostringstream ostr;
       ostr << "nrOfNodes\n";
       while(ni!= end()) {
         equal.print(*ni,ostr);++ni;
       }
       while(ni2!= nl2.end()) {
         equal.print(*ni2,ostr);++ni2;
       }
       throw com::Exception(ostr.str());
     }
*/
     // short log of nrOfNodes difference, print - end nodes
     if (!equal(!(ni != end() || ni2 != nl2.end()),--end(),--nl2.end(),
           "nrOfNodes-lastNodesPrintedBelow"))
       return false;
    return true;
   }
};

}
} // namespace pcrxml



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOMDIFF MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DOMDIFF MEMBERS
//------------------------------------------------------------------------------

pcrxml::DomDiff::DomDiff(
              const std::string& contents1,
              const std::string& contents2)
{
  d_doc1.setContent(QString(contents1.c_str()));
  d_doc2.setContent(QString(contents2.c_str()));
}



/* NOT IMPLEMENTED
//! Copy constructor.
pcrxml::DomDiff::DomDiff(DomDiff const& rhs)

  : Base(rhs)

{
}
*/



pcrxml::DomDiff::~DomDiff()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
pcrxml::DomDiff& pcrxml::DomDiff::operator=(DomDiff const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

bool pcrxml::DomDiff::equal(bool throwOnDiff) const
{
  Private::NodeList nv1(Private::NodeList::create(d_doc1));
  Private::NodeList nv2(Private::NodeList::create(d_doc2));

  return  nv1.equal(nv2,throwOnDiff);
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



