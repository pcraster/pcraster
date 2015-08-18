#ifndef INCLUDED_COM_RAWPALETTE
#define INCLUDED_COM_RAWPALETTE



#include <iostream>
#include <vector>
#include "com_rgbtuple.h"




namespace com {



/*!
  \class RawPalette
  \brief short_description

  Every palette has a min and a max value. These min and max values define the
  range of rgb values in the palette. Some palettes, for example, range from 0
  to 255 and others from 0 to 65535. The min value is always 0. The max value
  has to be set and is returned by the max() member function.
*/
//       1         2         3         4         5         6         7         8
class RawPalette
{

private:

  //! RGB tuples.
  std::vector<RgbTuple> d_tuples;

  //! Max value of the palette, inclusive e.g. 255 for byte rgb encodings
  UINT2            d_max;

public:

  typedef std::vector<RgbTuple>::const_iterator const_iterator;
  typedef std::vector<RgbTuple>::iterator iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   RawPalette          ();

  //! Constructor
                   RawPalette          (const UINT2 *p,
                                        size_t n,
                                        UINT2 max);

  //! Constructor.
                   RawPalette          (const UINT2 p[][3],
                                        size_t n,
                                        UINT2 max);

  //! Destructor.
  /* virtual */    ~RawPalette         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  // //! Input operator for reading a palet from a stream.
  // friend std::istream &operator>>      (std::istream &s,
  //                                       com::RawPalette &p);

  //! Resizes the palette to contain \a n tuples.
  void             resize              (size_t n);

  //! Sets the maximum value of the palette.
  void             setMaximum          (UINT2 m);

  //! Insert rgb tuple \a t before \a pos.
  void             insert              (iterator pos,
                                        const RgbTuple &t);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             equals              (RawPalette const& rhs) const;

  //! Returns a const iterator to the first rgb tuple.
  const_iterator   begin               () const;

  //! Returns an iterator to the first rgb tuple.
  iterator         begin               ();

  //! Returns a const iterator to the one-past-the-last rgb tuple.
  const_iterator   end                 () const;

  //! Returns an iterator to the one-past-the-last rgb tuple.
  iterator         end                 ();

  RgbTuple const&  colour              (size_t i) const;

  //! Returns true if the palette is empty (contains no colours).
  bool             empty               () const;

  //! Returns the number of rgb tuples in the palette.
  size_t           nrColours           () const;

  //! Returns the minimum value of the palette.
  UINT2            min                 () const;

  //! Returns the maximum value of the palette.
  UINT2            max                 () const;

  static const RawPalette* COLOURCLASSPALETTE();
  static const RawPalette* COLOURBOOLPALETTE();
  static const RawPalette* B2WPALETTE();
  static const RawPalette* B2WMPALETTE();
  static const RawPalette* W2BPALETTE();
  static const RawPalette* COLOURCONTPALETTE();
  static const RawPalette* RAINBOWPALETTE();
  static const RawPalette* R2PPALETTE();
  static const RawPalette* R2PMPALETTE();
  static const RawPalette* P2RMPALETTE();
  static const RawPalette* B2YPALETTE();
  static const RawPalette* Y2BPALETTE();
  static const RawPalette* Y2BMPALETTE();
  static const RawPalette* GRAYDIRECTPALETTE();
  static const RawPalette* W2BMPALETTE();
  static const RawPalette* R2GPALETTE();
  static const RawPalette* R2GMPALETTE();
  static const RawPalette* G2RMPALETTE();
  static const RawPalette* B2RMPALETTE();
  static const RawPalette* B2YMPALETTE();
  static const RawPalette* netscapePalette();

  static const RawPalette* booleanPalette();
  static const RawPalette* nominalPalette();
  static const RawPalette* ordinalPalette();
  static const RawPalette* scalarPalette();

  static const RawPalette* directionalPalette();

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (RawPalette const& lhs,
                                        RawPalette const& rhs);

bool               operator!=          (RawPalette const& lhs,
                                        RawPalette const& rhs);


//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

  const RawPalette* defaultPalette     (CSF_VS vs);

  std::vector<const RawPalette*> classPalettes();

  std::vector<const RawPalette*> rangePalettes();

  //! Converts and scales the rgb values in palette p to array \a a.
  void             convert             (const RawPalette &p,
                                        UINT2 max,
                                        UINT2 *a);

  //! Writes palette \a p to stream \a s.
  void             write               (std::ostream &s,
                                        const RawPalette &p);



} // namespace com

#endif
