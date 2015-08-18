#ifndef INCLUDED_CALC_BASEEXPR
#define INCLUDED_CALC_BASEEXPR

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTID
#include "calc_astid.h"
#define INCLUDED_CALC_ASTID
#endif

namespace calc {
  // BaseExpr declarations.
}

namespace calc {

class Operator;
class RunTimeEnv;
class ASTNodeVector;


//! expression holding an operation with a number of return values (0 or more)
//! Operation Node with 0 or more arguments
/*!
 * This is a NamedNode since the name of the operation as used by the
 * user must be set (or,|| etc)
 * Arguments are child nodes of this and are added with multiple transferArg()
 * calls or a single transferFunctionArgs call.
 */
class BaseExpr : public ASTId
{

private:
  friend class ASTTestFactory;

  //! Assignment operator.
  BaseExpr&           operator=           (const BaseExpr& rhs);


  //! child nodes
  ASTNodeVector*        d_args;

protected:
  //! Copy constructor.
                   BaseExpr               (const BaseExpr& rhs);
                   BaseExpr               (const std::string& opNameAsParsed);
                   BaseExpr               (const Position     *pos,
                                           const std::string&  opName);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual         ~BaseExpr               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             transferArg           (ASTNode *n);
  virtual void     transferFunctionArgs  (ASTNodeVector *args);

  virtual void     accept                (ASTVisitor& v);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual const Operator&       op        () const=0;

  size_t                nrArgs            () const;
  std::vector<DataType> dataTypeArgs      () const;
  ASTNodeVector*        args              () const;
  ASTNode*              arg               (size_t i) const;

  virtual void          exec              (RunTimeEnv* rte)  const=0;

  size_t                nrResults         () const;
  DataType              resultType        (size_t r=0) const;

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
