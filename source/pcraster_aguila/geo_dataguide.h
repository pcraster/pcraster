#ifndef INCLUDED_GEO_DATAGUIDE
#define INCLUDED_GEO_DATAGUIDE



#include "csftypes.h"
#include "com_rcptr.h"
#include "com_rcsize_t.h"
#include "geo_DataType.h"



namespace geo {



//! A DataGuide object refers (guides) to a piece of data.
/*!
  Collections of data contain addresses of data. The data is of a certain data
  type (eg stack) and has a valuescale (eg scalar). Collections can contain
  multiple addresses for the same data. For example, in a visualization
  application two stacks of the same data can be loaded: one for height values
  and the other for colouring the height ranges. In practice, the stack is
  loaded once and its address is stored twice. Also, two DataGuide objects are
  created. To be able to differentiate between the guides, a DataGuide object
  carries an index. This index is most probably a position in a data structure
  for storing the data addresses.

  Note that the index member is actually a pointer to an index. This way all
  copies of the DataGuide will get updated if the index of the guide in the
  data structure changes.

  DataGuide objects are supposed to be lightweight objects which are cheap to
  copy.
*/
class DataGuide
{

public:

  //! Generic pointer type of data.
  typedef const void* Address;

private:

  //! Index of the data in a data structure.
  com::RCPtr<com::RCSize_t> d_index;

  //! Generic pointer to the data, only used for addressing.
  Address          d_address;

  //! Type of data this guide points to.
  DataType         d_type;

  //! Valuescale of the data.
  CSF_VS           d_valueScale;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataGuide           ();

                   DataGuide           (size_t index,
                                        Address address,
                                        DataType type,
                                        CSF_VS valueScale);

  virtual          ~DataGuide          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setIndex            (size_t index);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isValid             () const;

  size_t           index               () const;

  Address          address             () const;

  DataType         type                () const;

  CSF_VS           valueScale          () const;

  bool             isRangeData         () const;

  bool             equals              (const DataGuide& dataGuide) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (const DataGuide& lhs,
                                        const DataGuide& rhs);

bool               operator!=          (const DataGuide& lhs,
                                        const DataGuide& rhs);

bool               operator<           (DataGuide const& lhs,
                                        DataGuide const& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
