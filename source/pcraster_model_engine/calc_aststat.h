#ifndef INCLUDED_CALC_ASTSTAT
#define INCLUDED_CALC_ASTSTAT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif
#ifndef INCLUDED_CALC_ID
#include "calc_id.h"
#define INCLUDED_CALC_ID
#endif


namespace calc {
  // ASTStat declarations.
  class Report ;
}



namespace calc {



//! AST Statement
/*
  A PCRaster statement is every thing between between 2 statements
  seperators (;). Most statements have the form of an optional report
  clauses followed by an assignment of the form p = expr (see ASTAss).
  Other possible constructs are:
  <ul>
   <li>StdoutStatement (not functioning at this moment)</li>
   <li>LinkInExpr with no returns, a class::method</li>
  </ul>
*/
class ASTStat: public ASTNode
{

private:
  //! does the statement starts with the report clause
  /*!
   *  if d_reportInSitu==0 && d_reportById.empty() and
   *  d_reportParsed is true, then the reportDefault is used.
   */
  bool             d_reportParsed;
  //! the id is found in the report table, empty is valid
  Id               d_reportById;
  //! the definition is in situ, 0 is valid
  Report*          d_reportInSitu;

  ASTNode*         d_stat;


  //! Assignment operator. NOT IMPLEMENTED.
  ASTStat&           operator=           (const ASTStat& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ASTStat               (const ASTStat& rhs);

  void             init();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ASTStat               ();

                   ASTStat               (ASTNode *stat);

  /* virtual */    ~ASTStat              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  virtual void     accept              (ASTVisitor& v);
  void             setReportParsed     (bool reportParsed);
  void             setReportById       (const Id& reportById);
  void             transferReportInSitu(Report* reportInSitu);
  void             transferStat        (ASTNode* stat);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ASTStat*          createClone          () const;
  ASTNode*          stat                 () const;

  bool             reportParsed        () const;
  const Id&        reportById          () const;
  Report*          reportInSitu        () const;

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
