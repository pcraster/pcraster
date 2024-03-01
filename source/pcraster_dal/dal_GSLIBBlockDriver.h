#ifndef INCLUDED_DAL_GSLIBBLOCKDRIVER
#define INCLUDED_DAL_GSLIBBLOCKDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_BLOCKDRIVER
#include "dal_BlockDriver.h"
#define INCLUDED_DAL_BLOCKDRIVER
#endif

#ifndef INCLUDED_DAL_TEXTFILEDRIVER
#include "dal_TextFileDriver.h"
#define INCLUDED_DAL_TEXTFILEDRIVER
#endif



namespace dal {
  // GSLIBBlockDriver declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  - http://ekofisk.stanford.edu/SCRFweb/GSLIB
  - http://sgems.sourceforge.net
*/
class PCR_DAL_DECL GSLIBBlockDriver: public BlockDriver,
                        public TextFileDriver
{

  friend class GSLIBBlockDriverTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  GSLIBBlockDriver& operator=          (GSLIBBlockDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   GSLIBBlockDriver    (GSLIBBlockDriver const& rhs);

  template<typename T>
  void             write               (Block const& block,
                                        std::ofstream& stream) const;

  void             write               (Block const& block,
                                        std::filesystem::path const& path) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GSLIBBlockDriver    ();

  /* virtual */    ~GSLIBBlockDriver   () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  using BlockDriver::read;
  using BlockDriver::open;

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

  Block*           open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const override;

  Block*           read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const override;

  void             write               (Block const& block,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const override;

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
