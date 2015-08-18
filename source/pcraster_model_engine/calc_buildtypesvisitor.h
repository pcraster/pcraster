#ifndef INCLUDED_CALC_BUILDTYPESVISITOR
#define INCLUDED_CALC_BUILDTYPESVISITOR

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_CFGVISITOR
#include "calc_cfgvisitor.h"
#define INCLUDED_CALC_CFGVISITOR
#endif
#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif
#ifndef INCLUDED_CALC_DATATYPECHANGER
#include "calc_datatypechanger.h"
#define INCLUDED_CALC_DATATYPECHANGER
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h"
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif



namespace calc {
  // BuildTypesVisitor declarations.
}


namespace calc {



//! builds up and check types for an CFGNode graph
/*!
    - type info is updated and syncronized between the graph and an ASTSymbolTable
    - resulting table is in table() after the visit
    - the graph features  containsDynamicSection() and 
      hasStatementWithReportKeyword() are deduced in the visit

   Implements one sweep of the closure algorithm to derive
   types for both the AST node presented here as CFG and the
   ASTSymbolTable.

   The ASTNode::dataType() may be changed and or symbol table
   entries information is also exchanged between the ASTNode's and
   ASTSymbolTable. If any information item in either any of the ASTNode's or
   the ASTSymbolTable changes then nrChanges() is larger;

   If the visit start with an empty symbol table then a visit is described
as:<i>
  Check and deduce correct types and argument use,
  collect requirements WITHOUT actually looking to input.<i>
  (Later ASTSymbolInfo::resolve() checks if the input matches the result
  table)

   \todo
    forcing an argument to be nonspatial in the
     TopDownExprRestrictor.

  All operations on d_table (ASTSymbolTable) must be garantueed to succeed if repeated
  a unlimited number of times, since BuildTypesVisitor is called often
  to resolve as a closure algorithm.

  BuildTypesVisitor is bit misnamed, in compiler terms this is the
  context analyser.

  results in a ASTSymbolTable with all information
  gathered from the AST. This table describes what
  data is needed and what data is generated from the AST.

  A CFGVisitor is used in order to garantuee the evaluation of ASTNode 
  descendants before the ASTNode itself.

 */
class BuildTypesVisitor : public CFGVisitor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  BuildTypesVisitor&           operator=           (const BuildTypesVisitor&);

  //! Copy constructor. NOT IMPLEMENTED.
                   BuildTypesVisitor               (const BuildTypesVisitor&);

  //! own symbol table
  ASTSymbolTable d_table;

  //! number of changes in ASTNode's and d_table
  DataTypeChanger d_dtc;

  //! remember rhs-type for lhs-type in p=timeoutput(...)
  VS             d_outputTssVs;

  bool             d_containsDynamicSection;
  bool             d_hasStatementWithReportKeyword;

  void          singleAss (ASTPar *p, const DataType& dtRhs);

  void          checkOnTimeinput(BaseExpr    *o);
  const IOType& ioType(const std::string& name) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BuildTypesVisitor               (CFGNode *cfg);

  /* virtual */    ~BuildTypesVisitor              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void visitExpr                                   (BaseExpr  *e);
  void visitPar                                    (ASTPar    *p);
  void visitNumber                                 (ASTNumber *n);
  void visitAss                                    (ASTAss    *a);
  void visitStat                                   (ASTStat   *s);
  void enterDynamicSection                         (DynamicSection *);
  void visitNonAssExpr                             (NonAssExpr   *e);
  void jumpOutRepeatUntil                          (RepeatUntil *);

  void init                (const ASTSymbolTable&          table);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const ASTSymbolTable& table                        () const;
        size_t          nrChanges                    () const;
  bool                  containsDynamicSection       () const;
  bool                  hasStatementWithReportKeyword() const;

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
