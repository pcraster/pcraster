#ifndef INCLUDED_DAL_PCRBLOCKDRIVER
#define INCLUDED_DAL_PCRBLOCKDRIVER



// Library headers.
#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

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
  // PCRBlockDriver declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  Layout:

  \begincode
    char[21]  "PCRaster Block Format", magic string.
    UINT4     Contents: 0 discretisation, 1 data
    UINT4     Number of rows.
    UINT4     Number of columns.

  If file contains discretisation information (typeId is REAL4):
    REAL8     Cell size.
    REAL8     West.
    REAL8     North.

    For every cell, starting with west cell from north row:
      REAL4   Base elevation.

      If base elevation is not a missing value:
        UINT4 Number of voxels in the stack.

        For every voxel, starting with the bottom one.
          T   Thickness.

  If file contains data:
    TypeId    Type id.

    For every cell, starting with west cell from north row:
      UINT4   Number of voxels in the stack.

      For every voxel, starting with the bottom one.
        T     Attribute value.
  \endcode

  \todo Writing and reading arrays can be done in one go instead of value by
        value.
*/
class PCR_DAL_DECL PCRBlockDriver: public BlockDriver,
                      public TextFileDriver
{

  friend class PCRBlockDriverTest;

private:

  static char      d_magicString[];

  //! Length of magic string, excluding the terminating \0.
  static size_t const d_lengthOfMagicString;

  //! Assignment operator. NOT IMPLEMENTED.
  PCRBlockDriver&  operator=           (PCRBlockDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   PCRBlockDriver      (PCRBlockDriver const& rhs);

  void             readThicknesses     (std::ifstream& stream,
                                        Block& block) const;

  template<typename T>
  void             readVoxels          (std::ifstream& stream,
                                        std::vector<T>& stack) const;

  template<typename T>
  void             readVoxels          (std::ifstream& stream,
                                        Block& block) const;

  void             writeThicknesses    (Block const& block,
                                        std::ofstream& stream) const;

  template<typename T>
  void             writeVoxels         (std::vector<T>const& stack,
                                        std::ofstream& stream) const;

  template<typename T>
  void             writeVoxels         (Block const& block,
                                        std::ofstream& stream) const;

  Block*           open                (std::ifstream& stream,
                                        TypeId typeId) const;

  Block*           open                (boost::filesystem::path const& path,
                                        TypeId typeId) const;

  Block*           read                (boost::filesystem::path const& path,
                                        TypeId typeId) const;

  void             write               (Block const& block,
                                        boost::filesystem::path const& path) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PCRBlockDriver      ();

  /* virtual */    ~PCRBlockDriver     ();

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

template<typename T>
inline void PCRBlockDriver::readVoxels(
         std::ifstream& stream,
         std::vector<T>& stack) const
{
  UINT4 nrVoxels;
  stream.read((char*)&(nrVoxels), 4);

  stack.resize(nrVoxels);

  for(size_t i = 0; i < nrVoxels; ++i) {
    stream.read((char*)&(stack[i]), sizeof(T));
  }
}

template<typename T>
inline void PCRBlockDriver::readVoxels(
         std::ifstream& stream,
         Block& block) const
{
  for(size_t i = 0; stream.good() && i < block.nrCells(); ++i) {
    readVoxels<T>(stream, block.cell<std::vector<T> >(i));
  }
}

template<typename T>
inline void PCRBlockDriver::writeVoxels(
         std::vector<T>const& stack,
         std::ofstream& stream) const
{
  UINT4 nrVoxels(stack.size());
  stream.write((char*)&nrVoxels, sizeof(nrVoxels));

  for(size_t j = 0; j < stack.size(); ++j) {
    stream.write((char const*)&stack[j], sizeof(T));
  }
}

template<typename T>
inline void PCRBlockDriver::writeVoxels(
         Block const& block,
         std::ofstream& stream) const
{
  for(size_t i = 0; stream.good() && i < block.nrCells(); ++i) {
    writeVoxels<T>(block.cell<std::vector<T> >(i), stream);
  }
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
