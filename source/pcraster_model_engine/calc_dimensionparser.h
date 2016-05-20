#ifndef INCLUDED_CALC_DIMENSIONPARSER
#define INCLUDED_CALC_DIMENSIONPARSER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.



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
    int         d_power;
    SymbolPower(const std::string& symbol):
      d_symbol(symbol),d_power(1)
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
