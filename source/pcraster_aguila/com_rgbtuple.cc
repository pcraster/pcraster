#include "com_rgbtuple.h"
#include "csftypes.h"



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------
namespace com {
const RgbTuple RgbTuple::red_    (255,0,  0);
const RgbTuple RgbTuple::green_  (0,  255,0);
const RgbTuple RgbTuple::blue_   (0,  0,  255);
const RgbTuple RgbTuple::white_  (255,255,255);
const RgbTuple RgbTuple::black_  (0,  0,  0);
const RgbTuple RgbTuple::gray50_ (128,128,128);
}


//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

/*!
  The rgb values will be initialised with 0, black
*/
com::RgbTuple::RgbTuple()

  : d_red(0), d_green(0), d_blue(0)

{
}



/*!
  \param     r Red value.
  \param     g Green value.
  \param     b Blue value.
*/
com::RgbTuple::RgbTuple(UINT2 r, UINT2 g, UINT2 b)

  : d_red(r), d_green(g), d_blue(b)

{
}



com::RgbTuple::~RgbTuple()
{
}



/*!
  \param     r Red value.
  \param     g Green value.
  \param     b Blue value.
*/
void com::RgbTuple::setRgb(UINT2 r, UINT2 g, UINT2 b)
{
  d_red   = r;
  d_green = g;
  d_blue  = b;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------

bool   com::operator==          (const RgbTuple &lhs,
                                 const RgbTuple &rhs)
{
  return
    lhs.red() == rhs.red()     &&
    lhs.green() == rhs.green() &&
    lhs.blue() == rhs.blue();
}
bool   com::operator!=          (const RgbTuple &lhs,
                                 const RgbTuple &rhs)
{
  return ! (lhs == rhs);
}

std::ostream&   com::operator<< (std::ostream& stream,
                                 const RgbTuple& r)
{
  // stream << "(r:" << r.red() << ",g:" << r.green() << ",b:" << r.blue() << ")";
  stream << r.red() << " " << r.green() << " " << r.blue();
  return stream;
}

//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


