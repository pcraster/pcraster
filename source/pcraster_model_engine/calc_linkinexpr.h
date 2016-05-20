#ifndef INCLUDED_CALC_LINKINEXPR
#define INCLUDED_CALC_LINKINEXPR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ID
#include "calc_id.h"
#define INCLUDED_CALC_ID
#endif
#ifndef INCLUDED_CALC_BASEEXPR
#include "calc_baseexpr.h"
#define INCLUDED_CALC_BASEEXPR
#endif

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

  //! Assignment operator. NOT IMPLEMENTED.
  LinkInExpr&           operator=           (LinkInExpr const& rhs);

  //  Copy constructor. DEFAULT
  //               LinkInExpr               (LinkInExpr const& rhs);

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

  boost::shared_ptr<ASTPar>   d_objectPar;


  std::string                   d_stringArgument;
  boost::shared_ptr<Operator>   d_op;
  //! not owned
  const LinkInLibrary*          d_library;

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

  /* virtual */    ~LinkInExpr              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
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

  const Operator&            op                  () const;
  ASTId*                     createClone         () const;
  void                       exec                (RunTimeEnv*) const;

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
