#ifndef INCLUDED_CALC_BINDINGTABLE
#define INCLUDED_CALC_BINDINGTABLE

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif


namespace calc {
  // BindingTable declarations.
}



namespace calc {

class RunDirectory;
class IScript;
class StatementBlock;
class UserSymbol;

//!Table of bindings
/*!
 * \todo
 *   sequentie order
*/
class BindingTable
{

private:
  enum DefinitionLevel { External, InScript };

  typedef struct Right {
    DefinitionLevel   d_definitionLevel;
    Symbol d_value;
    //! enforced single vs by typecast, VS_FIELD if not set
    VS     d_vs;
    Right(DefinitionLevel definitionLevel, const Symbol& value, VS vs);
  } Right;

  //! left = right is stored with left being the key
  typedef std::map<Symbol,Right> Table;
  Table d_table;


  //! Assignment operator. NOT IMPLEMENTED.
  BindingTable&           operator=           (const BindingTable&);

  //! Copy constructor. NOT IMPLEMENTED.
                   BindingTable               (const BindingTable&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BindingTable               ();

  /* virtual */   ~BindingTable               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void setExternalBindings(IScript            *addToThis,
                           const RunDirectory& rd);

  void add(const Symbol& left, const Symbol& right,VS vs);

  std::vector<UserSymbol *> moveConstantToParameters(StatementBlock *block);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const Symbol *find(const std::string& name) const;


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
