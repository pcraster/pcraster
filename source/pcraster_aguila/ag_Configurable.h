#ifndef INCLUDED_AG_CONFIGURABLE
#define INCLUDED_AG_CONFIGURABLE



#include <string>
#include <QtXml>
#include "ag_Configure.h"



namespace ag {



//! A Configurable is an object which can configure itself.
/*!
  The result of a configuration is stored in a DOM node.
*/
//       1         2         3         4         5         6         7         8
class PCR_AG_DECL Configurable
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Configurable&    operator=           (const Configurable&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Configurable        (const Configurable&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Configurable        ();

  virtual          ~Configurable       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  // configures
  virtual void     configure     (const QDomElement& c) = 0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! document Element of configuration
  virtual QDomElement configuration (const std::string& writeToFileName) const = 0;

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



} // namespace ag

#endif
