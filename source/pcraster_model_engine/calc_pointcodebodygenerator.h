#ifndef INCLUDED_CALC_POINTCODEBODYGENERATOR
#define INCLUDED_CALC_POINTCODEBODYGENERATOR

#include "stddefx.h"
#include "calc_domainill.h"
#include "calc_parset.h"
#include "calc_cfgvisitor.h"

#include <sstream>
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <set>


namespace calc {
  // PointCodeBodyGenerator declarations.
}



namespace calc {

//! PointCode stack item
class PointCodeSI {
  BaseExpr*         d_expr{nullptr};

   //! single name or _f....
  std::string      d_value;

   //! "leave node" args (entries of v, numbers or tmp's)
   std::set<std::string> d_names;

 public:
   PointCodeSI(const std::string& name);

   void                         setNames   (const std::set<std::string>& names);
   void                         setValue   (const std::string& value);
   void                         setExpr    (BaseExpr* expr);

   const std::string&           value      () const;
   const std::set<std::string>& names      () const;
   BaseExpr*                     expr       () const;
};


//! Generates the body of a point code function written to an external DLL
class PointCodeBodyGenerator : public CFGVisitor
{

  friend class PointCodeBodyGeneratorTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  PointCodeBodyGenerator&           operator=           (PointCodeBodyGenerator const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   PointCodeBodyGenerator               (PointCodeBodyGenerator const& rhs);

  //----------------------------------------------------------------------------
  // VISITORS
  //----------------------------------------------------------------------------

  void visitStat   (ASTStat* s) override;
  void visitExpr   (BaseExpr* e) override;
  void visitPar    (ASTPar*  p) override;
  void visitNumber (ASTNumber*  n) override;
  void visitAss    (ASTAss*  a) override;

  void doIfThenElse(BaseExpr* e);
  void doExpr      (BaseExpr* e);

  //----------------------------------------------------------------------------
  // TOOLS
  //----------------------------------------------------------------------------

  std::string              par        (ASTPar *p);
  std::string              domainCheck(DomainIll d,
                                       const std::string& templateArg) const;
  std::string              templateArg(const BaseExpr* e) const;
  std::string              f          (const BaseExpr* e) const;

  // streaming tools
  std::string              tmpDef     (const BaseExpr* e) const;
  void                     assignment (const std::string& result,
                                       const std::string& domainCheck) const;

  std::string              pop        ();
//void                     pop        (size_t nrPops);
  void                     popArgsPushResult(const std::string& function,
                                             size_t             nrArgs);

  void                     push       (const std::string& argName);
  const PointCodeSI&       fArg       (size_t argNr) const;
  const std::string&       arg        (size_t argNr) const;
  void                     reverseTop (size_t nrArgs);

  void                     selectPart (const DataType& dt);

  // as a stack
  std::vector<PointCodeSI>          d_args;

  std::map<std::string,std::string> d_parNames;

  //! non spatial part
  std::ostringstream                d_ns;
  //! part with loop for cells
  std::ostringstream                d_loop;

  std::ostringstream*               d_curr{nullptr};

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PointCodeBodyGenerator               (CFGNode*      cfg,
                                                         const ParSet& vContents);

  /* virtual */    ~PointCodeBodyGenerator              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void             print                                (std::ostream& s) const;

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
