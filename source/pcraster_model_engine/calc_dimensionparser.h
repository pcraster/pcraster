#ifndef INCLUDED_CALC_DIMENSIONPARSER
#define INCLUDED_CALC_DIMENSIONPARSER

#include "stddefx.h"

#include <vector>
#include <string>



namespace calc {
  // DimensionParser declarations.
}



namespace calc {



//! parse a interface::unit string composed of a Dimension vector
class DimensionParser
{
public:
  struct SymbolPower {
    std::string d_symbol;
    int         d_power{1};
    SymbolPower(const std::string& symbol):
      d_symbol(symbol)
    {
    }
  };

private:

  std::vector<SymbolPower> d_symbols;


  //! Assignment operator. NOT IMPLEMENTED.
  DimensionParser&           operator=           (DimensionParser const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DimensionParser               (DimensionParser const& rhs);


public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DimensionParser               (const std::string& str);

  /* virtual */    ~DimensionParser              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             add                           (const std::string& symbol);
  void             set                           (int power);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  static void      throwUnknown                  (const std::string& unknown);

  const std::vector<SymbolPower>&
                   symbols                       () const;

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
