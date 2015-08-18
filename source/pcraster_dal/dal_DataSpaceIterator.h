#ifndef INCLUDED_DAL_DATASPACEITERATOR
#define INCLUDED_DAL_DATASPACEITERATOR



// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_DEV_COMPILER
#include "dev_Compiler.h"
#define INCLUDED_DEV_COMPILER
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
#endif



namespace dal {
  // DataSpaceIterator declarations.
  class DataSpace;
}



namespace dal {



//! When given a DataSpace object this iterator iterates over all valid addresses.
/*!
  \warning   Make sure that the space object passed into the iterator contains
             only relevant dimensions. Otherwise iteration might take a longer
             time then needed and result in addresses you don't need. For
             example, in most cases you don't want to iterator over each
             individual cell in a raster.

  A data space iterator does not iterate over an existing range of addresses,
  but maintains one address which is updated when the iterator is incremented
  or decremented.

  When the iterator has moved beyond the valid range of addresses (one position
  before the first or after the last address in the data space) it is still
  possible to return to the valid range of addresses by incrementing or
  decrementing the iterator respectively. Dereferencing such an iterator
  results in disaster though.

  \code
  # Simplified from CSFRasterDriver::extremes<T>(T&, T&, std::string const&, DataSpace const&).
  for(DataSpaceIterator it = space.begin(); it != space.end(); ++it) {
    raster = open(pathForDataSpaceAddress(name, space, *it));
    min = std::min(min, raster->template min<T>());
    max = std::max(max, raster->template max<T>());
  }
  \endcode
*/
class PCR_DAL_DECL DataSpaceIterator
{

  friend class DataSpaceIteratorTest;
  friend class DataSpace;

private:

  //! Dataspace to iterate over.
  DataSpace*       d_space;

  //! Value of the iterator.
  DataSpaceAddress d_address;

  //! Indices of dimensions which have a set of exact values.
  std::vector<size_t> d_setIndices;

  //! Set to true when the end of the iterator range is reached.
  bool             d_endReached;

  //! Set to true when the rend of the iterator range is reached.
  bool             d_rendReached;

  void             setToBegin          ();

  void             setToEnd            ();

  void             setToRBegin         ();

  void             setToREnd           ();

  void             setToAddress        (DataSpaceAddress const& address);

  bool             endReached          () const;

  bool             rEndReached         () const;

  void             initialiseOnFirst   (size_t index = 0);

  void             initialiseOnLast    (size_t index = 0);

  void             initialiseOnAddress (DataSpaceAddress const& address);

  void             increment           ();

  void             decrement           ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataSpaceIterator   ();

                   DataSpaceIterator   (DataSpace const& space);

                   DataSpaceIterator   (DataSpace const& space,
                                        DataSpaceAddress const& address);

                   DataSpaceIterator   (DataSpaceIterator const& rhs);

  /* virtual */    ~DataSpaceIterator  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  DataSpaceIterator& operator=         (DataSpaceIterator const& rhs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  DataSpaceAddress const& operator*    () const;

  DataSpaceIterator& operator++        ();

  DataSpaceIterator& operator--        ();

  bool             equals              (DataSpaceIterator const& rhs) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

PCR_DAL_DECL bool  operator==          (DataSpaceIterator const& lhs,
                                        DataSpaceIterator const& rhs);

PCR_DAL_DECL bool  operator!=          (DataSpaceIterator const& lhs,
                                        DataSpaceIterator const& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
