#ifndef INCLUDED_DAL_MEMORYRASTERDATA
#define INCLUDED_DAL_MEMORYRASTERDATA



// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_ANY
#include <boost/any.hpp>
#define INCLUDED_BOOST_ANY
#endif

#include <boost/shared_ptr.hpp>

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif

#ifndef INCLUDED_DAL_MEMORYDATA
#include "dal_MemoryData.h"
#define INCLUDED_DAL_MEMORYDATA
#endif

#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif



namespace dal {
  // MemoryRasterData declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \todo  Support objects which carry the data for the whole dataspace and
         objects which carry the data for a specific address. The pool should
         be able to merge MemoryData objects.
  \todo  Create docs about who owns stuff in the dal library. Stuff in memory
         is owned by the one who added it in the first place. But ownership
         might be transferred by calling the right functions on the Library
         or MemoryDataPool object. Tricky business?
  \todo  The user of this class has to make sure that the type id of the values
         is supported by the application.
  \todo  At each address, a Raster object should be stored(?). We get rasters
         and the client wants rasters.
*/
class MemoryRasterData: public MemoryData
{

  friend class MemoryRasterDataTest;

private:

  //! Values for all dimensions of the data space in a hierarchical layout.
  std::vector<boost::any> d_values;

  //! Data space of the data values.
  DataSpace        d_dataSpace;

  //! Type of the data values.
  TypeId           d_typeId;

  //! Number of rows in the raster.
  size_t           d_nrRows;

  //! Number of cols in the raster.
  size_t           d_nrCols;

  //! Cell size of the raster cells.
  double           d_cellSize;

  //! Most western coordinate.
  double           d_west;

  //! Most northern coordinate.
  double           d_north;

  //! Minimum data value in the data set.
  boost::any       d_min;

  //! Maximum data value in the data set.
  boost::any       d_max;

#ifdef DEBUG_DEVELOP
  template<class T>
  void             checkConsistency    (std::vector<boost::any> values);

  void             checkConsistency    (std::vector<boost::any> values);

  template<class T>
  void             checkConsistency    (std::vector<boost::any> values,
                                        DataSpace space);

  void             checkConsistency    (std::vector<boost::any> values,
                                        DataSpace space);

  void             checkConsistency    ();
#endif

  void*            cells               (std::vector<boost::any> values);

  template<class T>
  void*            cells               (std::vector<boost::any> values,
                                        DataSpace space,
                                        DataSpaceAddress address);

  void*            cells               (std::vector<boost::any> values,
                                        DataSpace space,
                                        DataSpaceAddress address);

  bool             hasExtremes         ();

  void             updateExtremes      ();

  template<typename T>
  void             updateExtremes      ();

protected:

public:

  enum RasterContents {
    HeaderOnly,
    IncludingValues
  };

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MemoryRasterData    (DataSpace const& dataSpace,
                                        TypeId typeId,
                                        size_t nrRows,
                                        size_t nrCols,
                                        double cellSize,
                                        double west,
                                        double north);

                   MemoryRasterData    (std::vector<boost::any>& values,
                                        DataSpace const& dataSpace,
                                        TypeId typeId,
                                        size_t nrRows,
                                        size_t nrCols,
                                        double cellSize,
                                        double west,
                                        double north);

                   MemoryRasterData    (MemoryRasterData const& rhs);

  /* virtual */    ~MemoryRasterData   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  MemoryRasterData& operator=          (MemoryRasterData const& rhs);

  void             add                 (MemoryRasterData const& data,
                                        DataSpaceAddress const& address);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  DataSpace const& dataSpace           () const;

  bool             exists              () const;

  bool             exists              (DataSpaceAddress const& address) const;

  Raster*          raster              (RasterContents contents);

  Raster*          raster              (TypeId typeId,
                                        RasterContents contents);

  Raster*          raster              (DataSpaceAddress const& address,
                                        RasterContents contents);

  Raster*          raster              (DataSpaceAddress const& address,
                                        TypeId typeId,
                                        RasterContents contents);

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
void MemoryRasterData::updateExtremes()
{
  bool initialised = false;
  T min, max;
  boost::shared_ptr<Raster> raster;

  // Scan all data values and set extreme values.
  if(d_dataSpace.rank() == 0) {
    raster.reset(this->raster(IncludingValues));

    size_t i = 0;

    // Find first non-MV value.
    for(; i < raster->nrCells(); ++i) {
      if(!pcr::isMV(raster->cell<T>(i))) {
        min = raster->cell<T>(i);
        max = raster->cell<T>(i);
        initialised = true;
        break;
      }
    }

    // Check next values.
    for(;i < raster->nrCells(); ++i) {
      if(!pcr::isMV(raster->cell<T>(i))) {
        min = std::min<T>(min, raster->cell<T>(i));
        max = std::max<T>(max, raster->cell<T>(i));
      }
    }
  }
  else {
    for(DataSpaceIterator it = d_dataSpace.begin(); it != d_dataSpace.end();
           ++it) {
      raster.reset(this->raster(*it, IncludingValues));

      if(!initialised) {
        size_t i = 0;

        for(; i < raster->nrCells(); ++i) {
          if(!pcr::isMV(raster->cell<T>(i))) {
            min = raster->cell<T>(i);
            max = raster->cell<T>(i);
            initialised = true;
            break;
          }
        }

        for(;i < raster->nrCells(); ++i) {
          if(!pcr::isMV(raster->cell<T>(i))) {
            min = std::min<T>(min, raster->cell<T>(i));
            max = std::max<T>(max, raster->cell<T>(i));
          }
        }
      }
      else {
        for(size_t i = 0; i < raster->nrCells(); ++i) {
          if(!pcr::isMV(raster->cell<T>(i))) {
            min = std::min<T>(min, raster->cell<T>(i));
            max = std::max<T>(max, raster->cell<T>(i));
          }
        }
      }
    }
  }

  if(initialised) {
    d_min = min;
    d_max = max;
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
