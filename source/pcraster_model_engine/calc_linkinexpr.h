#ifndef INCLUDED_CALC_LINKINEXPR
#define INCLUDED_CALC_LINKINEXPR

#include "stddefx.h"
#include "calc_id.h"
#include "calc_baseexpr.h"

#include <memory>
#include <string>


namespace calc {
  // LinkInExpr declarations.
  class Operator;
  class LinkInLibrary;
  class ASTPar;
}
namespace pcrxml {
  class RunContext;
  class CallPoint;
}



namespace calc {

//! Expression for a Link In library call point
class LinkInExpr : public BaseExpr
{

private:

  Id               d_nameBefore;
  Id               d_nameAfter;

  /*! name of "class", if not empty, then this is a method invocation of an instance
      named d_nameBefore calling method d_nameAfter of d_className
   */
  std::string      d_libraryName;
  std::string      d_className;
  std::string      d_objectName;
  std::string      d_methodName;
  std::string      d_functionName;

  std::shared_ptr<ASTPar>   d_objectPar;


  std::string                   d_stringArgument;
  std::shared_ptr<Operator>   d_op;
  //! not owned
  const LinkInLibrary*          d_library{nullptr};

  pcrxml::RunContext            context       (RunTimeEnv const& rte) const;
  pcrxml::CallPoint             callPoint     () const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LinkInExpr               (Id const& nameBefore,
                                             Id const& nameAfter,
                                             std::string const& stringArgument);

                   LinkInExpr               (LinkInExpr const& rhs) = default;

  /* virtual */    ~LinkInExpr              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  LinkInExpr&           operator=           (LinkInExpr const& rhs) = delete;

  void loadLibrary                          (const LinkInLibrary *library);

  void setAsMethod                          (std::string const&   className);
  void setAsConstructor                     (ASTPar      const&   objectName);
  void setAsFunction                        ();

  void check                                ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool                       isMethod            () const;
  bool                       isConstructor       () const;
  bool                       isFunction          () const;
  const ASTPar*              objectPar           () const;

  std::string const&         nameBefore          () const;
  std::string const&         nameAfter           () const;

  const Operator&            op                  () const override;
  ASTId*                     createClone         () const override;
  void                       exec                (RunTimeEnv*) const override;

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
