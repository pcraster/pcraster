#ifndef INCLUDED_CALC_STACKEDVALUE
#define INCLUDED_CALC_STACKEDVALUE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_DATAVALUE
#include "calc_datavalue.h"
#define INCLUDED_CALC_DATAVALUE
#endif

namespace calc {
  // StackedValue declarations.
}



namespace calc {

class RunTimeEnv;
class ASTSymbolInfo;

/*! Must fetch the DataValue from the symboltable, at the
 *  moment the DataValue is popped from RunTimeStack
 *  on its turn the DataTable may need to read the data external and store
 *  the data  for subsequent use after the operation that triggered the load().

    Reason for this class is that the actual reading of external must
    be delayed as much as possible. For example, when (a+(b+(c+(d+e+(..+z)))))
    is computed, a naive implementation would read all a..z first, and then
    execute. The current implementation needs only 2 maps at once in RAM.
    That  implies that spatials are handled by reference on the stack, and
    setUseDefs() sets the ASTPar::d_lastUse in the order the the data is 
    POPPED from RunTimeEnv::d_fieldStack not PUSHED onto it.

 */
class StackedValue : public DataValue {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  StackedValue&           operator=           (const StackedValue& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   StackedValue               (const StackedValue& rhs);

 //! in this table the read value should be stored
 RunTimeEnv&        d_rte;

 const ASTSymbolInfo& d_symbol;

 bool d_lastUse;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   StackedValue               ( RunTimeEnv&   rte,
                                                const ASTSymbolInfo& symbol,
                                                bool          lastUse);

  /* virtual */    ~StackedValue              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  DataValue*        load                           ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool              readOnlyReference              () const;
  OVS               ovs                            () const;

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
