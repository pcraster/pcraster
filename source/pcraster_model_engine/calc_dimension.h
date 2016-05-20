#ifndef INCLUDED_CALC_DIMENSION
#define INCLUDED_CALC_DIMENSION



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
  // Dimension declarations.
}

namespace pcrxml {
  // Dimension declarations.
  class Dimension;
}


namespace calc {



//! OpenMI-like IDimension/DimensionBase
/*!
 * Maar dan ff anders, in OpenMI, OpenMI definitie:
 *
    - dimension=L3T-1
    - unit = "(feet*acre)/day" ) L3T-1
    - dus conversion nodig
    .

    \sa http://www.servocomm.freeserve.co.uk/Cpp/physical_quantity/index.html

  Lijst van units

   - Length Meter m
   - Mass Kilogram kg
   - Time Second s
   - ElectricCurrent Ampere A
   - Temperature Kelvin K
   - AmountOfSubstance Mole mol
   - LuminousIntensity Candela cd
   - Currency1 Euro E (moeilijk)
   .
 *
*/
class Dimension : public std::vector<double>
{

private:

public:
  enum Base {
        Length=0,
        Mass=1,
        Time=2,
        ElectricCurrent=3,
        Temperature=4,
        AmountOfSubstance=5,
        LuminousIntensity=6,
        Currency=7,
        NumBaseQuanties=8
  };

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Dimension               ();

                   Dimension               (const std::string& str);

  Dimension&       operator=               (Dimension const& rhs);

                   Dimension               (Dimension const& rhs);

  /* virtual */    ~Dimension              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool              none                   () const;

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
