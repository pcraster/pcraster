#ifndef INCLUDED_PCRXML_ELEMENTCDATASECTION
#define INCLUDED_PCRXML_ELEMENTCDATASECTION



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Standard library headers.

// Library headers.
#ifndef INCLUDED_PCRXML_ELEMENT
#include "pcrxml_element.h"
#define INCLUDED_PCRXML_ELEMENT
#endif

// Target headers.


class QDomNode;

namespace pcrxml {


//! A PCDATA element, e.g empty of simple content model.
/*!
 * Base class for empty of simple content model elements, e.g.
 * elements that are in the DTD modelled as 
 * \code
 * <!ELEMENT name (#PCDATA)>.
 * \endcode
 *
 * Contents of this type of elements is a series of 0 or more
 * text nodes or CDATA sections.
 *
 * Underlying parser should correctly handle the three builtIn entities, and
 * the 2 handies:
 * \code
 * &   &amp;
 * <   &lt;
 * "   &quot;
 *
 * >   &gt;
 * '   &apos;
 * \endcode
 */

class PCDATAElement : public Element
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  PCDATAElement&           operator=           (const PCDATAElement&);


  std::string      d_contents;
  //! control output as entire CDATA section yes/no, default false
  bool             d_asCDATASection;


protected:
                   PCDATAElement               ();
                   PCDATAElement               (const PCDATAElement&);
                   PCDATAElement               (const QDomElement& n,
                                                const std::string& elementName);

  QDomNode         contentsNode        (const QDomNode& parent) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


                    PCDATAElement(const std::string& contents);

  /* virtual */    ~PCDATAElement              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setAsCDATASection   (bool asCDATASection);
  void             setContents         (const std::string& contents);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool             asCDATASection      ()                 const;

  const std::string& contents          ()                 const;
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
