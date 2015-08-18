#ifndef INCLUDED_CALC_ASTNODE
#define INCLUDED_CALC_ASTNODE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif

namespace calc {
  // ASTNode declarations.
  class ASTVisitor;
  class Position;
}



namespace calc {



//! Node of the Abstract Syntax Tree
/*!  as an id (like ASTPar) or a compound element (like ASTExpr)
    The Abstract Syntax Tree (AST) is a collection of nodes that are
    "single-referenced". All nodes that make the tree are only pointed
    to by a single parent. Therefor each pointer to a node is known to be an unique
    point/position in the script.

    Each node may return 0 or more data items: returnDataType()
    A statement does not return anything, an ASTPar 1, an ASTExpr 0 or more.

    createClone() is a deep-copy operation: the entire sub tree of the clone
    created is copied/cloned. A shallow copy conflicts with the "single reference
    principle"
 */
class ASTNode
{
private:
  //! position it has, default PositionName with empty string
  Position *d_pos;

  //! the return data types, default empty
  /*!
   * setNrReturns will set them to the default DataType (all).
   * BuildTypesVisitor will fill with the correct type
   */
  std::vector<DataType> d_returnDataType;


protected: // since ASTNode is pure virtual
                   ASTNode                ();

                   ASTNode                (const ASTNode& e);
  ASTNode&         operator=              (const ASTNode& e);

  void             setNrReturns           (size_t n);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                   ASTNode                (const Position *pos);


     virtual       ~ASTNode              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  virtual void           accept            (ASTVisitor& v)=0;
  void                   setPosition       (const Position* p);
  DataType&              returnDataType    (size_t i=0);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  /*!
    createClone() is a deep-copy operation: the entire sub tree of the clone
    created is copied/cloned. A shallow copy conflicts with the "single reference
    principle".
    \warning no copying should be done AFTER the AST is constructed and analysis
    starts; ASTSymbolInfo does contains ptrs into the AST.
  */
  virtual ASTNode*           createClone    ()const=0;
  virtual void               runtimeError   (size_t timeStep,
                                             const std::string& inMsg) const;

  virtual void               posError       (const std::string& msg) const;
  virtual void               posError       (const std::ostringstream& msg) const;

  std::string                shortPosText   () const;

  const Position            *position       () const;

  const DataType&            returnDataType (size_t i=0) const;
  size_t                     nrReturns      () const;
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

std::string runtimeErrorFmt(
  size_t   timeStep,
  const std::string& msg);

} // namespace calc

#endif
