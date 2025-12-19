#ifndef INCLUDED_CALC_STATTABLE
#define INCLUDED_CALC_STATTABLE

#include "stddefx.h"
#include "com_pathname.h"
#include "csftypes.h"
#include "com_interval.h"
#include "calc_baseexpr.h"
#include "calc_astsymbolinfo.h"

#include <iostream>


namespace calc {

class Field;
class ASTPar;
class ASTSymbolInfo;
class StatTable : public BaseExpr {
public:
  struct InputMap {
    //! name to put in table;
    std::string            d_name;
    ASTNode*               d_intervals{nullptr};
    ASTNode*               d_field{nullptr};
    InputMap();
  };
private:
  //! model identifier
  ASTPar*          d_id;

  //! write to, empty if d_copyStringToMemoryOutputId is valid
  com::PathName    d_writeToFile;
  size_t           d_copyStringToMemoryOutputId;

  InputMap         d_subject,d_cross;

  //! verbose, print more (debug) data in table
  static bool      d_verbose;

  Operator        *d_op{nullptr};

private:

  //! Assignment operator. NOT IMPLEMENTED.
  StatTable&           operator=           (const StatTable& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   StatTable               (const StatTable& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                   StatTable               (std::string const& id,
                                            InputMap const& subject,
                                            InputMap const& cross);

  /* virtual */    ~StatTable              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------


  void             setIdBinding        (const ASTSymbolInfo& sym);
  static void      setVerbose          (bool verbose);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void             exec                (RunTimeEnv* rte)const override;
  const Operator&  op                  () const override;
  ASTId*           createClone         () const override;
  ASTPar*          id                  () const;

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
