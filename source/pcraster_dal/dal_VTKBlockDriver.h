#ifndef INCLUDED_DAL_VTKBLOCKDRIVER
#define INCLUDED_DAL_VTKBLOCKDRIVER



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
  // VTKBlockDriver declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \todo      Add support for partly implemented drivers to dal. This driver
             only supports writing.
*/
class PCR_DAL_DECL VTKBlockDriver: public BlockDriver,
                      public TextFileDriver
{

  friend class VTKBlockDriverTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  VTKBlockDriver&  operator=           (VTKBlockDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   VTKBlockDriver      (VTKBlockDriver const& rhs);

  void             write               (Block const& block,
                                        boost::filesystem::path const& path) const;

  static void      regularBlockProperties(
                                        size_t* nrVoxelsPerStack,
                                        double* voxelThickness,
                                        Raster const& voxels);

  template<typename ValueType>
  static void      cellData            (std::string& result,
                                        Block const& block);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VTKBlockDriver      ();

  /* virtual */    ~VTKBlockDriver     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Block*           open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const;

  Block*           read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const;

  void             write               (Block const& block,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const;

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
