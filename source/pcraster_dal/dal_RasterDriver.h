#ifndef INCLUDED_DAL_RASTERDRIVER
#define INCLUDED_DAL_RASTERDRIVER



// Library headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM
#include <boost/filesystem.hpp>
#define INCLUDED_BOOST_FILESYSTEM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DRIVER
#include "dal_Driver.h"
#define INCLUDED_DAL_DRIVER
#endif

#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif


namespace dal {
  // RasterDriver declarations.
  /// class Table;
}



namespace dal {



//! This class is a base class for i/o drivers for Raster datasets.
/*!
  \todo Add virtual read(string, Raster) (skip Angle, Valuescale in
        CSFRasterDriver).
  \todo Make reading / creating legend table optional / possible. In case a
        legend is stored it is used, otherwise a default legend is created.
        (only implemented for discrete cell value types).


*/
class PCR_DAL_DECL RasterDriver: public Driver
{

  friend class RasterDriverTest;

private:

  template<typename T>
  bool             extremes            (T& min,
                                        T& max,
                                        std::string const& name,
                                        DataSpace space,
                                        TypeId typeId) const;

  boost::tuple<bool, FilenameConvention, std::string>
                   determineFilenameCharacteristics(
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             cacheDatasetInfo    (std::string const& key,
                                        FilenameConvention convention,
                                        std::string const& extension) const;

protected:

                   RasterDriver        (Format const& format);

  boost::filesystem::path pathFor      (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             browseFileBasedRasterAttributes(
                                        std::vector<BrowseInfo>& attributes,
                                        boost::filesystem::path const& path,
                                        FilenameConventions conventions) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~RasterDriver       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  Raster*          open                (std::string const& name) const;

  Raster*          open                (std::string const& name,
                                        TypeId typeId) const;

  Raster*          open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  virtual Raster*  open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const=0;

  /*
  virtual Raster*  open                (std::string const& name,
                                        DataSpace const& space) const;
                                        */

  virtual DataSpace dataSpace          (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Raster*          read                (std::string const& name) const;

  //! Reads raster \a name. Cell values are converted to \a typeId types.
  /*!
    \param     name Name of raster.
    \param     typeId Id of type to be used to store cell values.
    \return    New raster.
    \warning   The underlying I/O library is responsible for converting values
               to \a typeId. It is possible that certain conversions are not
               supported by the library. The caller should check whether the
               conversion is feasible.
  */
  Raster*          read                (std::string const& name,
                                        TypeId typeId) const;

  // Raster*          read                (std::string const& name,
  //                                       DataSpaceAddress const& address) const;

  Raster*          read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  virtual Raster*  read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const=0;

  /// void             read                (Table& table,
  ///                                       std::string const& name,
  ///                                       DataSpace const& space) const;

  void             read                (Raster& raster,
                                        std::string const& name) const;

  virtual void     read                (Raster& raster,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const=0;

  virtual void     read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const=0;

  void             write               (Raster const& raster,
                                        std::string const& name) const;

  virtual void     write               (Raster const& raster,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const=0;

  bool             extremes            (boost::any& min,
                                        boost::any& max,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
inline bool RasterDriver::extremes(
         T& min,
         T& max,
         std::string const& name,
         DataSpace space,
         TypeId typeId) const
{
  assert(!space.hasSpace());

  bool initialised = false;
  Raster* raster = 0;

  if(space.isEmpty()) {
    raster = open(name, typeId);

    if(raster) {
      if(!raster->hasExtremes()) {
        // Extremes unknown, possibly because source has no header.
        read(*raster, name);
      }

      if(!raster->allMV()) {
        min = raster->template min<T>();
        max = raster->template max<T>();
        initialised = true;
      }
    }
  }
  else {
    for(DataSpaceIterator it = space.begin(); it != space.end(); ++it) {
      raster = open(name, space, *it, typeId);

      if(raster) {
        if(!raster->hasExtremes()) {
          // Extremes unknown, possibly because source has no header.
          read(*raster, name, space, *it);
        }

        if(!raster->allMV()) {
          if(!initialised) {
            min = raster->template min<T>();
            max = raster->template max<T>();
            initialised = true;
          }
          else {
            min = std::min(min, raster->template min<T>());
            max = std::max(max, raster->template max<T>());
          }
        }
      }
    }
  }

  return initialised;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
