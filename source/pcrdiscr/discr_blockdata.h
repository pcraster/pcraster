#ifndef INCLUDED_DISCR_BLOCKDATA
#define INCLUDED_DISCR_BLOCKDATA



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif

#ifndef INCLUDED_BOOST_SIGNALS2_CONNECTION
#include <boost/signals2/connection.hpp>
#define INCLUDED_BOOST_SIGNALS2_CONNECTION
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.
#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif



namespace discr {
  // BlockData declarations.
}



namespace discr {



//! Class for block data objects.
/*!
  Objects of this class contain data values. Discretisation information is
  provided by the layered pointer to a Block object.
*/
template<typename ValueType>
class BlockData: public RasterData<std::vector<ValueType> >
{

  friend class BlockDataTest;

private:

  //! Discretisation information.
  Block*           d_block;

  //! Value to use when new discretisation space is created.
  RasterData<ValueType> d_defaultValue;

  //! Connection for adding voxels.
  boost::signals2::connection d_addVoxelsConnection;

  //! Connection for removing voxels.
  boost::signals2::connection d_removeVoxelsConnection;

  // //! Connection for cutting voxels.
  // boost::signals2::connection d_cutVoxelConnection;

  //! Assignment operator. NOT IMPLEMENTED.
  BlockData&       operator=           (BlockData const& rhs);

  void             createConnections   ();

  void             initVoxels          ();

  void             initVoxels          (BlockData const& data);

  void             addVoxels           (size_t index,
                                        size_t nr);

  void             addVoxels           (size_t index,
                                        std::vector<ValueType> const& values);

  void             removeVoxels        (size_t index,
                                        size_t nr);

  // void             cutVoxel            (size_t index,
  //                                       REAL4 fraction);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BlockData           (Block* block);

                   BlockData           (Block* block,
                                        ValueType defaultValue);

                   BlockData           (Block* block,
                                        RasterData<ValueType> const& defaultValue);

                   BlockData           (BlockData const& rhs);

  /* virtual */    ~BlockData          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  // void             setCutVoxelStrategy (CutVoxelStrategy strategy);

  void             setDefaultValue     (ValueType value);

  void             setDefaultValue     (RasterData<ValueType> const& value);

  void             setDefaultValueMissing();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  Block const*     block               () const;

  Block*           block               ();

  RasterData<ValueType> const& defaultValue() const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     block Discretisation information.

  The default value is set to a missing value.
*/
template<typename ValueType>
inline BlockData<ValueType>::BlockData(
         Block* block)
  : RasterData<std::vector<ValueType> >(block),
    d_block(block),
    d_defaultValue(block)
{
  PRECOND(block);
  setDefaultValueMissing();
  createConnections();
  // setCutVoxelStrategy(LeaveValue);
  initVoxels();
}

//! Constructor
/*!
  \param     block Discretisation information.
  \param     defaultValue Default value for new voxels.
*/
template<typename ValueType>
inline BlockData<ValueType>::BlockData(
         Block* block,
         ValueType defaultValue)
  : RasterData<std::vector<ValueType> >(block),
    d_block(block),
    d_defaultValue(block, defaultValue)
{
  PRECOND(block);
  createConnections();
  // setCutVoxelStrategy(LeaveValue);
  initVoxels();
}

//! Constructor.
/*!
  \param     block Discretisation information.
  \param     defaultValue Default value for new voxels.
*/
template<typename ValueType>
inline BlockData<ValueType>::BlockData(
         Block* block,
         RasterData<ValueType> const& defaultValue)
  : RasterData<std::vector<ValueType> >(block),
    d_block(block),
    d_defaultValue(defaultValue)
{
  PRECOND(block);
  createConnections();
  // setCutVoxelStrategy(LeaveValue);
  initVoxels();
}

//! Copy constructor.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Don't call the copy constructor of the base class, this is meant for
  elementary types only, not for class types (std::vector<ValueType>).
  We could pimp the RasterData copy constructor to have two versions.
*/
template<typename ValueType>
inline BlockData<ValueType>::BlockData(
         BlockData const& rhs)
  : RasterData<std::vector<ValueType> >(rhs.d_block),
    d_block(rhs.d_block),
    d_defaultValue(rhs.d_defaultValue)
{
  createConnections();
  // setCutVoxelStrategy(LeaveValue);
  initVoxels(rhs);
}

//! Destructor.
/*!
*/
template<typename ValueType>
inline BlockData<ValueType>::~BlockData()
{
  d_addVoxelsConnection.disconnect();
  d_removeVoxelsConnection.disconnect();
  // d_cutVoxelConnection.disconnect();
}

//! Initialises voxel values.
/*!
  \warning   To be called from a constructor only.

  Based on the information in the discretisation object this function creates
  new voxels. The data value of these voxels is based on the default value
  set.
*/
template<typename ValueType>
inline void BlockData<ValueType>::initVoxels()
{
  for(size_t i = 0; i < d_block->nrCells(); ++i) {
    // Only meant to be called from the constructor.
    DEVELOP_PRECOND(this->cell(i).empty());

    if(!d_block->cell(i).isMV()) {
      addVoxels(i, d_block->cell(i).size());
    }

    DEVELOP_POSTCOND(this->cell(i).size() == d_block->cell(i).size());
  }
}

template<typename ValueType>
inline void BlockData<ValueType>::initVoxels(
         BlockData const& data)
{
  for(size_t i = 0; i < d_block->nrCells(); ++i) {
    // Only meant to be called from the constructor.
    DEVELOP_PRECOND(this->cell(i).empty());

    if(!d_block->cell(i).isMV()) {
      addVoxels(i, data.cell(i));
    }

    DEVELOP_POSTCOND(this->cell(i).size() == d_block->cell(i).size());
  }
}

//! Creates connections between signals in the discretisation object and slots in this object.
/*!
  \warning   To be called from a constructor only.

  These connections make sure we are signalled when the discretisation of the
  space changes (voxels added, removed or cut). This is important because a
  data object needs to be in sync with its discretisation at all times.
*/
template<typename ValueType>
inline void BlockData<ValueType>::createConnections()
{
  void (BlockData::*addVoxels)(size_t, size_t) = &BlockData::addVoxels;

  d_addVoxelsConnection = d_block->addVoxelsSignal().connect(
         boost::bind(addVoxels, this, _1, _2));
  d_removeVoxelsConnection = d_block->removeVoxelsSignal().connect(
         boost::bind(&BlockData<ValueType>::removeVoxels, this, _1, _2));
  // d_cutVoxelConnection = d_block->cutVoxelSignal().connect(
  //        boost::bind(&BlockData<ValueType>::cutVoxel, this, _1, _2));
}

// template<>
// inline void BlockData<REAL4>::setCutVoxelStrategy(
//          CutVoxelStrategy strategy)
// {
//   d_cutVoxelConnection.disconnect();
// 
//   switch(strategy) {
//     case CutValue: {
//       d_cutVoxelConnection = d_block->cutVoxelSignal().connect(
//          boost::bind(&BlockData<REAL4>::cutVoxel, this, _1, _2));
//       break;
//     }
//     case LeaveValue:
//     default: {
//       break;
//     }
//   }
// }

//! Sets the default values used when new voxels are created.
/*!
  \param     value New default value. A missing value is allowed here.
*/
template<typename ValueType>
inline void BlockData<ValueType>::setDefaultValue(
         ValueType value)
{
  if(pcr::isMV(value)) {
    setDefaultValueMissing();
  }
  else {
    d_defaultValue = value;
  }
}

template<typename ValueType>
inline void BlockData<ValueType>::setDefaultValue(
         RasterData<ValueType> const& value)
{
  d_defaultValue = value;
}

//! Sets the default value to a missing value.
/*!
  When new voxels are created they will contain a missing value.
*/
template<typename ValueType>
inline void BlockData<ValueType>::setDefaultValueMissing()
{
  d_defaultValue.setAllMV();
  // pcr::setMV(d_defaultValue);
}

//! Adds \a nr voxels to the top of voxel stack \a index.
/*!
  \param     index Index of voxel stack to add voxels to.
  \param     nr Number of voxels to add.

  The new value of the voxels will be the default value set.
*/
template<typename ValueType>
inline void BlockData<ValueType>::addVoxels(
         size_t index,
         size_t nr)
{
  std::vector<ValueType>& voxels(this->cell(index));

  if(pcr::isMV(d_defaultValue.cell(index))) {
    voxels.insert(voxels.end(), nr, ValueType());
    if(!voxels.empty()){
      pcr::setMV(&(*(voxels.end() - nr)), nr);
    }
  }
  else {
    voxels.insert(voxels.end(), nr, d_defaultValue.cell(index));
  }
}

template<typename ValueType>
inline void BlockData<ValueType>::addVoxels(
         size_t index,
         std::vector<ValueType> const& values)
{
  std::vector<ValueType>& voxels(this->cell(index));
  voxels.resize(voxels.size() + values.size());

  for(size_t i = voxels.size() - values.size(), j = 0;
         i < voxels.size(); ++i, ++j) {
    if(pcr::isMV(values[j])) {
      pcr::setMV(voxels[i]);
    }
    else {
      voxels[i] = values[j];
    }
  }
}

//! Removes \a nr voxels from the end of stack \a index.
/*!
  \param     index Index of stack to manipulate.
  \param     nr Number of voxels to remove.
  \warning   \a nr must be in range [0, cell(index).size()>
*/
template<typename ValueType>
inline void BlockData<ValueType>::removeVoxels(
         size_t index,
         size_t nr)
{
  std::vector<ValueType>& voxels(this->cell(index));
  DEVELOP_PRECOND(nr <= voxels.size());
  voxels.erase(voxels.end() - nr, voxels.end());
}

// //! Recalculates a new value for the cut top voxel of voxel stack \a index.
// /*!
//   \param     index Index of voxel stack to cut.
//   \param     fraction Fraction of the voxel which is removed.
//   \todo      Remove from interface, this function is useless isn't it?!
// 
//   The original value is unchanged.
// */
// template<typename ValueType>
// inline void BlockData<ValueType>::cutVoxel(
//          size_t /* index */,
//          REAL4 /* fraction */)
// {
// }

// //! Recalculates a new REAL4 value for the cut top voxel of voxel stack \a index.
// /*!
//   \param     index Index of voxel stack to cut.
//   \param     fraction Fraction of the voxel which is removed.
// 
//   The new value is calculated as the original value minus a \a fraction of this value.
// */
// template<>
// inline void BlockData<REAL4>::cutVoxel(
//          size_t index,
//          REAL4 fraction)
// {
//   std::vector<REAL4>& voxels(this->cell(index));
//   voxels.back() -= fraction * voxels.back();
// }

//! Returns the discretisation object this object is linked to.
/*!
  \return    discretisation
*/
template<typename ValueType>
inline Block const* BlockData<ValueType>::block() const
{
  return d_block;
}

//! Returns the discretisation object this object is linked to.
/*!
  \return    discretisation
*/
template<typename ValueType>
inline Block* BlockData<ValueType>::block()
{
  return d_block;
}

//! Returns the default value set.
/*!
  \return    default value
*/
template<typename ValueType>
inline RasterData<ValueType> const& BlockData<ValueType>::defaultValue() const
{
  return d_defaultValue;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

#ifdef DEBUG_DEVELOP
  #define DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(data) \
  for(size_t i = 0; i < data.block()->nrCells(); ++i) { \
    if(data.block()->cell(i).isMV()) { \
      PRECOND(data.cell(i).empty()); \
    } \
    PRECOND(data.block()->cell(i).size() == data.cell(i).size()); \
  }
#else
  #define DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(data)
#endif

} // namespace discr

#endif
