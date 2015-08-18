#include "com_bitvector.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the BitVector class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC BITVECTOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BITVECTOR MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     size Number of bits in the vector.

  All bits are cleared with clear(size_t).
*/
com::BitVector::BitVector(size_t size)

  : d_size(size), d_bits(new int[1 + d_size / BITSPERWORD])

{
  for(size_t i = 0; i < d_size; ++i) {
    clear(i);
  }
}



//! Destructor.
/*!
*/
com::BitVector::~BitVector()
{
  delete [] d_bits;
}



//! Sets a bit on.
/*!
  \param     index Index of bit to turn on.
  \warning   The result is undefined if \a index >= size.
  \sa        clear(size_t), operator[](size_t)
*/
void com::BitVector::set(size_t index)
{
#ifdef DEBUG_DEVELOP
  assert(index < d_size);
#endif

  d_bits[index >> SHIFT] |= (1 << (index & MASK));
}



//! Sets a bit off.
/*!
  \param     index Index of bit to turn off.
  \warning   The result is undefined if \a index >= size.
  \sa        set(size_t), operator[](size_t)
*/
void com::BitVector::clear(size_t index)
{
#ifdef DEBUG_DEVELOP
  assert(index < d_size);
#endif

  d_bits[index >> SHIFT] &= ~(1 << (index & MASK));
}



//! Tests if a bit is turned on.
/*!
  \param     index Index of bit to test.
  \warning   The result is undefined if \a index >= size.
  \sa        set(size_t), clear(size_t)
*/
bool com::BitVector::operator[](size_t index) const
{
#ifdef DEBUG_DEVELOP
  assert(index < d_size);
#endif

  return (d_bits[index >> SHIFT] & (1 << (index & MASK))) != 0;
}



//! Returns the size of the vector.
/*!
  \return    Size.
*/
size_t com::BitVector::size() const
{
  return d_size;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



