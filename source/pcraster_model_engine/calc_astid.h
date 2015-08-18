#ifndef INCLUDED_CALC_ASTID
#define INCLUDED_CALC_ASTID



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.  // Module headers.
#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif

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


  virtual         ~ASTId               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void setName(const std::string& newName);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual ASTId*        createClone         () const=0;

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
