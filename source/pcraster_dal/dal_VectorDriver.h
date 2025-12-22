#ifndef INCLUDED_DAL_VECTORDRIVER
#define INCLUDED_DAL_VECTORDRIVER

#include "dal_Configure.h"
#include "dal_Driver.h"
#include "dal_Vector.h"

#include <memory>



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

  std::unique_ptr<Data> _data;

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

  /* virtual */    ~VectorDriver       () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  using Driver::read;

  bool             exists              (std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const override;

  Vector*          open                (std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const override;

  DataSpace        dataSpace           (std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const override;

  Vector*          read                (std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const override;

  void             read                (Vector& vector,
                                        std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const;

  void             read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space=DataSpace(),
                                        DataSpaceAddress const& address=DataSpaceAddress()) const override;

  bool             extremes            (std::any& min,
                                        std::any& max,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space=DataSpace()) const;

  void             browse              (std::vector<BrowseInfo>& attributes,
                                        std::string const& location) const override;

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
