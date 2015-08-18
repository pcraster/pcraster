#ifndef INCLUDED_COM_BITVECTOR
#define INCLUDED_COM_BITVECTOR



// Library headers.

// PCRaster library headers.

// Module headers.



namespace com {
  // BitVector declarations.
}



namespace com {



//! The BitVector class is for arbitrarely long vectors of bits.
/*!
  Vectors of bits can be used as a set for a (sparse) range of integers.

  Code is adapted from Jon Bentley's Programming Peals, 2nd edition, p. 140.

  More bit stuff is already implemented in misc/bitset.c
*/
class BitVector
{

private:

  enum { BITSPERWORD = 32, SHIFT = 5, MASK = 0x1F };

  //! Number of bits in vector.
  size_t           d_size;

  //! Array of bits.
  int             *d_bits;

  //! Assignment operator. NOT IMPLEMENTED.
  BitVector&       operator=           (const BitVector&);

  //! Copy constructor. NOT IMPLEMENTED.
                   BitVector           (const BitVector&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BitVector           (size_t size);

  /* virtual */    ~BitVector          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             set                 (size_t index);

  void             clear               (size_t index);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             operator[]          (size_t index) const;

  size_t           size                () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
