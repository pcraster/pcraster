#ifndef INCLUDED_DAL_DATASPACEADDRESS
#define INCLUDED_DAL_DATASPACEADDRESS



// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_ANY
#include <boost/any.hpp>
#define INCLUDED_BOOST_ANY
#endif

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



namespace dal {
  // DataSpaceAddress declarations.
}



namespace dal {

//! Class for addresses into a data space object.
/*!
  An address has coordinates which are positions along the dimensions of a
  data space. This class is very flexible in that no assumption is made about
  the type of the coordinates. Integral, floating points, strings and / or
  objects of user defined types can be used as coordinates. As long as you
  keep the address and data space in sync with each other.

  A data space object is needed to be able to interpret the coordinates
  of an data space address.

  \code
  // Create empty address.
  DataSpaceAddress address;

  // Add coordinate for dimension with size_t values.
  address.addCoordinate<size_t>(3);

  // Change coordinate.
  address.setCoordinate<size_t>(0, 4);

  // Create address with 3 dimensions, for example scenario names, sample
  // numbers and time steps.
  address = DataSpaceAddress(3);

  address.setCoordinate<std::string>(0, "aap");
  address.setCoordinate<std::string>(1, 5);
  address.setCoordinate<std::string>(1, 10);

  // Get coordinate for sample dimension.
  size_t sampleNumber = address.coordinate<size_t>(1);
  \endcode

  None of the member functions of this class throw exceptions except for the
  template function coordinate<T>(size_t) which throws a boost::bad_any_cast
  in case this class is not used properly.
*/
class PCR_DAL_DECL DataSpaceAddress
{

  friend class DataSpaceAddressTest;

private:

  //! Collection of coordinates.
  std::vector<boost::any> d_coordinates;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataSpaceAddress    ();

                   DataSpaceAddress    (size_t size);

  /* virtual */    ~DataSpaceAddress   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  template<typename T>
  void             addCoordinate       (T const& value);

  template<typename T>
  void             setCoordinate       (size_t index,
                                        T const& value);


  void             unsetCoordinate     (size_t index);

  void             eraseCoordinate     (size_t index);

  void             resize              (size_t size);

  void             clear               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           size                () const;

  bool             isValid             (size_t index) const;

  bool             isValid             () const;

  size_t           nrInvalidCoordinates() const;

  boost::any const& coordinate         (size_t index) const;

  template<typename T>
  T const&         coordinate          (size_t index) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Adds a coordinate to the address.
/*!
  \param     value New coordinate to add.
  \todo      Rename to appendCoordinate.
*/
template<typename T>
inline void DataSpaceAddress::addCoordinate(T const& value)
{
  d_coordinates.push_back(value);
}

//! Sets the value of the coordinate with index \a index to \a value.
/*!
  \param     index Index of coordinate to set.
  \param     value New value.
  \warning   The coordinate with index \a index must already exist.
*/
template<typename T>
inline void DataSpaceAddress::setCoordinate(
         size_t index,
         T const& value)
{
  d_coordinates[index] = value;
}

//! Returns the value of the coordinate with index \a index.
/*!
  \param     index Index of coordinate to return.
  \return    Value
  \except    boost::bad_any_cast In case of a programming error when T is not
             the same as the type of the stored coordinate.
*/
template<typename T>
inline T const& DataSpaceAddress::coordinate(
         size_t index) const
{
  return boost::any_cast<T const&>(d_coordinates[index]);
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif
