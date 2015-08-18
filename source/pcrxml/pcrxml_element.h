#ifndef INCLUDED_PCRXML_ELEMENT
#define INCLUDED_PCRXML_ELEMENT

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace com {
 class PathName;
}


class QDomNode;
class QDomDocument;
class QDomElement;

namespace pcrxml {

class Document;

//! base class of all pcrxml elements
/*!
  <h3>implementation details derived classes</h3>
  Single sub elements are represented by pointers, if the pointer is 0 the element is not
  present. Repetitions, 0,1 or more, are represented by a vector of pointers. A vector of
  size 0 means not present. All pointers are adopted; Once pointers are attached to the elements,
  the elements owns the pointer; if the elements is destroyed all pointed objects are destroyed.
*/
class Element
{

  virtual void fill(QDomElement el) const=0;

protected:

                   Element               ();
                   Element               (const Element&);
                   Element               (const QDomElement& e,
                                          const std::string& elementName);
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

     virtual       ~Element              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------


  //! name of element
  virtual const std::string& elementName()const=0;

  //! create a deep DomElement representation.
  /*!
   * if \a doc has a different namespace than pcr then the pcr namespace
   * is defined.
   * \param doc document the returned element will belong to
   */
  QDomElement toDomElement(QDomDocument doc) const;

  //! create a (new) Document of this (deep)
  Document toDomDocument() const;

  std::string toString() const;


  //! add element (subtree) as (last) child to parent
  void appendTo(QDomNode& parent) const;

  void             write               (std::string const& fileName) const;

  void write(const com::PathName& fileName) const;

  void writeToFile(const char *fileName) const;
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
