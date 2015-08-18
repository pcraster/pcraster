#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#define INCLUDED_CALC_ASTSYMBOLTABLE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
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
#ifndef INCLUDED_CALC_ASTSYMBOLINFO
#include "calc_astsymbolinfo.h"
#define INCLUDED_CALC_ASTSYMBOLINFO
#endif


namespace calc {
  // ASTSymbolTable declarations.
  class LinkInLibrary;
  class ASTPar;
  class ASTNode;
  class IoFieldStrategy;
  class SymException;
}


namespace calc {


/*! \brief
 *  table with information on the parameters of an AST fragment
 *
 *  Names that only occur in the binding are not always included.
 *
 *
 * All operations on ASTSymbolTable must be garantueed to succeed if repeated
 * a unlimited number of times, since BuildTypesVisitor is called often
 * to resolve as a closure algorithm.
 *
 *  collects all requirements for input parameters and stores info
 *  on output parameters.
 *  It is simple a table with unique names (the parameters) with some
 *  type info attached.
 *
 */
class ASTSymbolTable:
  public   std::map<std::string,ASTSymbolInfo>
{
  typedef std::map<std::string,ASTSymbolInfo> Base;

  typedef std::map<std::string, boost::shared_ptr<LinkInLibrary> > LinkInLibraries;

  LinkInLibraries    d_linkInLibraries;

public:

  struct LinkInLibraryException {
    std::string message;
  };


  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    ASTSymbolTable              (Base const& syms);
                    ASTSymbolTable              ();

  /* virtual */    ~ASTSymbolTable              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  ASTSymbolInfo&    operator[]                 (const ASTPar *p);

  LinkInLibrary const* linkInLibrary           (std::string const& name);
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool               contains                   (const std::string& name) const;
  bool               contains                   (const ASTPar *p)         const;

  bool               containsMemoryExchangeSymbols()                      const;

  const ASTSymbolInfo& operator[]               (const ASTPar *p)         const;
  const ASTSymbolInfo& operator[]               (const std::string& n )   const;

  void               throwSym                   (const SymException& s) const;

  void               checkDifferentExternalNames() const;

};

//! handy for BOOST_FOREACH
typedef std::pair<std::string, ASTSymbolInfo> ASTSymbolTablePair;



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------






//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------


std::ostream &operator<<(std::ostream& s, const calc::ASTSymbolTable& t);

} // namespace calc

extern "C"  const char *interfaceAsXML();

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------
#endif
