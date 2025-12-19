#ifndef INCLUDED_CALC_ASTDEFINITION
#define INCLUDED_CALC_ASTDEFINITION

#include "stddefx.h"
#include "calc_id.h"
#include "calc_definitionrole.h"

#include <string>
#include <map>


namespace calc {
  // ASTDefinition declarations.
  class Dimension;
}



namespace calc {



//! definitions for a single variable as parsed between { and }
/*!
 * a definition is a list of key-value pairs that is present
 * in the interface or local section.
 * \todo
 *   has some similarity with com::KeyValueTable but no advantage
 *   in using it?
 */
class ASTDefinition
{
private:

  typedef std::map<Id, Id> KeyValue;

  Id               d_name;
  KeyValue         d_items;
  std::string      d_definitionRole;



private:

  //  Assignment operator. default
  // ASTDefinition&  operator=  (ASTDefinition const& rhs);

  //  Copy constructor. default
  //  ASTDefinition             (ASTDefinition const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ASTDefinition       ();

  /* virtual */    ~ASTDefinition      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void               setName           (const Id& name);
  void               add               (const Id& key, const Id& value);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const std::string& name              () const;

  DefinitionRole     definitionRole    () const;

//const std::string& input             () const;
//const std::string& output            () const;
//const std::string& constant          () const;
  Dimension          unit              () const;
  std::string        description       () const;

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
