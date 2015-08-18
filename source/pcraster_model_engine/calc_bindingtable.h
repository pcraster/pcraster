#ifndef INCLUDED_CALC_BINDINGTABLE
#define INCLUDED_CALC_BINDINGTABLE

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTNODEVECTOR
#include "calc_astnodevector.h"
#define INCLUDED_CALC_ASTNODEVECTOR
#endif


namespace calc {
  // BindingTable declarations.

}



namespace calc {

class RunDirectory;
class ASTSymbolTable;

//! Table of bindings as parsed in script binding section
/*!
   Implemented as ASTNodeVector, parser garantuees the following
   structure:
     ASTNodeVector is list of ASTAss nodes, each ASTAss node has
     the layout: ASTAss::d_par is a single par with no indices,
     ASTAss::d_rhs is a ASTPar or ASTNumber node.

   Advantage of a list, over a std::map, is that the definition order
   is saved in list. This is needed for the interface generation.

  3 types of bindings:
    1) binding is a constant number
          should go to DataTable holding that NonSpatial
        Needed BEFORE BuildTypeVisitor no redefs as normal pars allowed
        or check after BuildTypeVisitor is these symbols are assigned in
        AST
    2) binding is the name used for input, when !firstIsCreation() holds
        Needed on resolve()
    3)  binding is the name used for output
        Needed on exec()
  Types 2 and 3 can be only be discerned AFTER BuildTypeVisitor analysis; what is in and what is out?
*/
class BindingTable: public ASTNodeVector
{
protected:
  void             addLastDefinition          (const ASTNodeVector &l);
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

/* DEFAULT
 * BindingTable&           operator=           (const BindingTable&);
 *
 *                  BindingTable               (const BindingTable&);
 */

                   BindingTable               ();

  /* virtual */   ~BindingTable               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
};

/* Bindings as effective after interface and external bindings file are
 *  added.
 *
 */
class EffectiveBindings: public BindingTable {
public:

  EffectiveBindings(BindingTable const&);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             overwrite                  (const ASTNodeVector& e);

  void             applyToSymbols             (ASTSymbolTable& t,
                                               const std::set<std::string>&
                                                        interfaceSyms);
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
