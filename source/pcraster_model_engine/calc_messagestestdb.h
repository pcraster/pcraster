#ifndef INCLUDED_CALC_MESSAGESTESTDB
#define INCLUDED_CALC_MESSAGESTESTDB



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif
// PCRaster library headers.

// Module headers.

namespace com {
  class Exception;
}

class QDomElement;

namespace calc {

class MessagesTestDBPrivate;


//! implements the database of test messages that must be compared
/*!
 *  this class has a singleton interface
 */
class MessagesTestDB
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  MessagesTestDB&           operator=           (const MessagesTestDB&);

  //! Copy constructor. NOT IMPLEMENTED.
                   MessagesTestDB               (const MessagesTestDB&);

  MessagesTestDBPrivate               *d_data;

  static std::auto_ptr<MessagesTestDB> d_instance;

private:
                   MessagesTestDB               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
        static     MessagesTestDB* instance     ();

  /* virtual */    ~MessagesTestDB              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool        equals(const std::string& id, const com::Exception& e,
                     const std::string prefix) const;
  QDomElement xml   (const std::string& id)                          const;
  bool        hasXML(const std::string& id)                          const;
  std::string model (const std::string& id)                          const;
  std::string options(const std::string& id)                         const;

  bool equalsFile( const std::string& id,
                          const std::string& fileCreated) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
