#ifndef INCLUDED_COM_RGBTUPLE
#define INCLUDED_COM_RGBTUPLE



#include "csftypes.h"
#include <iostream>



namespace com {



/*!
  \class RgbTuple
  \brief The RgbTuple class is for objects holding rgb values.

  Rbg values range between [0 - (2^16 - 1)].
*/
//       1         2         3         4         5         6         7         8
class RgbTuple
{

private:

  //! Red value.
  UINT2            d_red;

  //! Green value.
  UINT2            d_green;

  //! Blue value.
  UINT2            d_blue;


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   RgbTuple            ();

  //! Constructor.
                   RgbTuple            (UINT2 r,
                                        UINT2 g,
                                        UINT2 b);

  //! Destructor.
  /* virtual */    ~RgbTuple           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Sets the rgb-values of the tuple.
  void             setRgb              (UINT2 r,
                                        UINT2 g,
                                        UINT2 b);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the red value.
  UINT2            red                 () const;

  //! Returns the green value.
  UINT2            green               () const;

  //! Returns the blue value.
  UINT2            blue                () const;


  //! some predefined constants
  static const  RgbTuple  red_,green_,blue_,black_,white_,gray50_;
};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

inline UINT2 RgbTuple::red() const
{ return d_red; }

inline UINT2 RgbTuple::green() const
{ return d_green; }

inline UINT2 RgbTuple::blue() const
{ return d_blue; }



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

  std::ostream&      operator<<       (std::ostream& stream,
                                       const RgbTuple& r);

  bool           operator==          (const RgbTuple &lhs,
                                      const RgbTuple &rhs);
  bool           operator!=          (const RgbTuple &lhs,
                                      const RgbTuple &rhs);


//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
