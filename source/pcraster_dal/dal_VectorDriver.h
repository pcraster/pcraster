#ifndef INCLUDED_DAL_VECTORDRIVER
#define INCLUDED_DAL_VECTORDRIVER



// External headers.
#include <boost/scoped_ptr.hpp>

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DRIVER
#include "dal_Driver.h"
#define INCLUDED_DAL_DRIVER
#endif

#ifndef INCLUDED_DAL_VECTOR
#include "dal_Vector.h"
#define INCLUDED_DAL_VECTOR
#endif



namespace dal {
  // VectorDriver declarations.
}



namespace dal {

//! Raster based vector attribute driver.
/*!
  A raster based vector attribute is defined by a magnitude in x-direction and
  a magnitude in y-direction.

  This driver uses the raster drivers to perform the actual I/O to
  vector attributes. It maintains a collection of raster drivers which
  are tried in turn to open the data.

  \sa        Vector
  \todo      Make sure the properties of a Vector object are filled. The Vector
             object may use the layered Matrix objects(?).
*/
class PCR_DAL_DECL VectorDriver: public Driver
{

  friend class VectorDriverTest;

private:

  class Data;

  boost::scoped_ptr<Data> _data;

  template<typename T>
  bool             extremes            (T& min,
                                        T& max,
                                        std::string const& name,
                                        DataSpace const& space) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VectorDriver        ();

  /* virtual */    ~VectorDriver       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             exists              (std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const;

  Vector*          open                (std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const;

  DataSpace        dataSpace           (std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const;

  Vector*          read                (std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const;

  void             read                (Vector& vector,
                                        std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const;

  void             read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const;

  bool             extremes            (boost::any& min,
                                        boost::any& max,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space=DataSpace()) const;

  void             browse              (std::vector<BrowseInfo>& attributes,
                                        std::string const& location) const;

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
