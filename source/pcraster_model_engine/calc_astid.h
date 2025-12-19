#ifndef INCLUDED_CALC_ASTID
#define INCLUDED_CALC_ASTID

#include "stddefx.h"
#include "calc_astnode.h"

#include <string>


namespace calc {
  // ASTId declarations.
  class Id;
}



namespace calc {

//! A node with an user supplied symbol or name
/*! name/identifier of the symbol, like id,+,etc..
 *  ASTId does ASTNode::setNrReturns(1) in its ctors: by default
 *  return a single value.
 */
class ASTId : public ASTNode
{
  //! name of the symbol, like id,+,etc.., never empty except if default ctor is used
  std::string d_name;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                   ASTId               ();

  ASTId&           operator=           (const ASTId&);

                   ASTId               (const ASTId&);

                   ASTId               (const std::string& s);

                   ASTId               (const Id& id);


          ~ASTId               () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void setName(const std::string& newName);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ASTId*        createClone         () const override =0;

  const std::string&    name                () const;
  bool                  debugOnlyValid      () const;
  std::string           qName               () const;

  bool                  isNumber            () const;
  double                toNumber            () const;

};

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool operator<(const ASTId& lhs, const ASTId& rhs);
bool operator==(const ASTId& lhs, const ASTId& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
