#ifndef INCLUDED_CALC_STATTABLE
#define INCLUDED_CALC_STATTABLE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif
#ifndef INCLUDED_COM_INTERVAL
#include "com_interval.h"
#define INCLUDED_COM_INTERVAL
#endif

// Module headers.
#ifndef INCLUDED_CALC_BASEEXPR
#include "calc_baseexpr.h"
#define INCLUDED_CALC_BASEEXPR
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLINFO
#include "calc_astsymbolinfo.h"
#define INCLUDED_CALC_ASTSYMBOLINFO
#endif
namespace calc {

class Field;
class ASTPar;
class ASTSymbolInfo;
class StatTable : public BaseExpr {
public:
  struct InputMap {
    //! name to put in table;
    std::string            d_name;
    ASTNode*               d_intervals;
    ASTNode*               d_field;
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

  Operator        *d_op;

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

  /* virtual */    ~StatTable              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------


  void             setIdBinding        (const ASTSymbolInfo& sym);
  static void      setVerbose          (bool verbose);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void             exec                (RunTimeEnv* rte)const;
  const Operator&  op                  () const;
  ASTId*           createClone         () const;
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
