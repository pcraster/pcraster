#ifndef INCLUDED_DAL_DATASPACEADDRESSMAPPER
#define INCLUDED_DAL_DATASPACEADDRESSMAPPER



// Library headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif



namespace dal {
  // DataSpaceAddressMapper declarations.
  class CoordinateMapper;
}



namespace dal {



//! Class for objects mapping the coordinates of a data space address.
/*!
  This class contains a CoordinateMapper for each dimension of the DataSpace
  of the DataSpaceAddress es used. Using these mappers coordinates can be
  mapped from one range (the source range) to another (the destination range).
  Furthermore, addresses or individual coordinates can be written to a
  string for presentation purposes.
*/
class PCR_DAL_DECL DataSpaceAddressMapper: private boost::noncopyable
{

  friend class DataSpaceAddressMapperTest;

private:

  //! Data space of the coordinates used.
  DataSpace        _space;

  //! For each dimension a mapper for the coordinates.
  std::vector<CoordinateMapper*> _mappers;

  void             initMappers         ();

  void             deleteMappers       ();

  DataSpace const& space               () const;

  std::vector<CoordinateMapper*> const& mappers() const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataSpaceAddressMapper();

                   DataSpaceAddressMapper(DataSpace const& space);

  /* virtual */    ~DataSpaceAddressMapper();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setDataSpace        (DataSpace const& space);

  void             setMapper           (size_t index,
                                        CoordinateMapper* mapper);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  CoordinateMapper const* mapper       (size_t index) const;

  DataSpaceAddress destination         (DataSpaceAddress const& address) const;

  DataSpaceAddress source              (DataSpaceAddress const& address) const;

  std::string      toString            (DataSpaceAddress const& address) const;

  std::string      toString            (DataSpaceAddress const& address,
                                        size_t index) const;

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



} // namespace dal

#endif
