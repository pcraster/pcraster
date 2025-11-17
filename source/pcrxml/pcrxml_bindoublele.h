#ifndef INCLUDED_PCRXML_BINDOUBLELE
#define INCLUDED_PCRXML_BINDOUBLELE

#include "stddefx.h"
#include "pcrxml_attribute.h"



namespace pcrxml {
  // BinDoubleLE declarations.
}



namespace pcrxml {



//! attribute with a double value, encoded as an 8 byte hex string in little endian format
class BinDoubleLE : public Attribute
{

private:
  double  d_value{};

  //  Assignment operator. DEFAULT
  // BinDoubleLE&           operator=           (const BinDoubleLE&);

  //  Copy constructor.DEFAULT
  //               BinDoubleLE               (const BinDoubleLE&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BinDoubleLE            (const QDomNode& owningElement,
                                           const std::string& name, bool required);
                   BinDoubleLE            (double value);
                   BinDoubleLE            ();

  /* virtual */    ~BinDoubleLE              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  std::string attrValueStr() const override;

  double value() const      { return d_value; }
  double operator()() const { return d_value; }

  static double hexToDouble (const std::string& hexString);

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



} // namespace pcrxml

#endif
