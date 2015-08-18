#ifndef INCLUDED_DAL_BLOCKDRIVER
#define INCLUDED_DAL_BLOCKDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DRIVER
#include "dal_Driver.h"
#define INCLUDED_DAL_DRIVER
#endif

#ifndef INCLUDED_DAL_BLOCK
#include "dal_Block.h"
#define INCLUDED_DAL_BLOCK
#endif



namespace dal {
  // BlockDriver declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  Driver properties:

  <table>
    <tr>
      <td>DAL_DRIVER_GENERAL</td>
      <td>DriverProperies</td>
      <td>0</td>
    </tr>
  </table>
*/
class PCR_DAL_DECL BlockDriver: public Driver
{

  friend class BlockDriverTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  BlockDriver&     operator=           (BlockDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   BlockDriver         (BlockDriver const& rhs);

protected:

                   BlockDriver         (Format const& format);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~BlockDriver        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  virtual Block*   open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  virtual Block*   open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const=0;

  virtual DataSpace dataSpace          (std::string const& name) const;

  virtual DataSpace dataSpace          (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  virtual Block*   read                (std::string const& name) const;

  virtual Block*   read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  virtual Block*   read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const=0;

  void             read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             write               (Block const& block,
                                        std::string const& name) const;

  virtual void     write               (Block const& block,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const=0;

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
