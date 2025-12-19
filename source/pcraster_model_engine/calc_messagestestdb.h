#ifndef INCLUDED_CALC_MESSAGESTESTDB
#define INCLUDED_CALC_MESSAGESTESTDB

#include "stddefx.h"

#include <string>
#include <memory>


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

  static std::unique_ptr<MessagesTestDB> d_instance;

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
                     const std::string& prefix) const;
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
