#ifndef INCLUDED_DAL_CSFMAP
#define INCLUDED_DAL_CSFMAP




// Library headers.
#ifndef INCLUDED_BOOST_ANY
#include <boost/any.hpp>
#define INCLUDED_BOOST_ANY
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif



struct MAP;
namespace dal {
  // CSFMap declarations.
  class DataSpace;
  class DataSpaceAddress;
  class Table;
}



namespace dal {



//! Wrapper class around the CSF MAP structure and library functions.
/*!
  This class is used by the CSFRasterDriver class.

  Detailed documentation about the CSF library functions can be found in its
  documentation. Find it at http://pcraster.geo.uu.nl.
*/
class CSFMap
{

  friend class CSFMapTest;

private:

  // Default extension, including leading dot.
  // static std::string const d_defaultExtension;

  //! Path to file.
  boost::filesystem::path d_path;

  //! Map pointer, always connected to an open file.
  MAP *            d_map;

  //! Assignment operator. NOT IMPLEMENTED.
  CSFMap&          operator=           (CSFMap const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CSFMap              (CSFMap const& rhs);

  void             open                (bool allowUpdate);

  void             create              (size_t nrRows,
                                        size_t nrCols,
                                        double west,
                                        double north,
                                        double angle,
                                        double cellSize,
                                        TypeId typeId,
                                        CSF_VS valueScale,
                                        CSF_PT projectionType= PT_YDECT2B);

  void             close               ();

  size_t           nrLegendEntries     () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CSFMap              (boost::filesystem::path const& path,
                                        bool allowUpdate=false);

                   CSFMap              (boost::filesystem::path const& path,
                                        size_t nrRows,
                                        size_t nrCols,
                                        double west,
                                        double north,
                                        double angle,
                                        double cellSize,
                                        TypeId typeId,
                                        CSF_VS valueScale,
                                        CSF_PT projectionType= PT_YDECT2B);

  /* virtual */    ~CSFMap             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  // static bool      exists              (std::string const& name,
  //                                       DataSpace const& space,
  //                                       DataSpaceAddress const& address);

  void             useAs               (TypeId typeId);

  void             putCells            (void const* buffer);

  void             putCells            (size_t offset,
                                        size_t nrCells,
                                        void const* buffer);

  void             setExtremes         (boost::any min,
                                        boost::any max);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           nrRows              () const;

  size_t           nrCols              () const;

  size_t           nrCells             () const;

  double           cellSize            () const;

  double           west                () const;

  double           north               () const;

  double           angle               () const;

  CSF_VS           valueScale          () const;

  CSF_PT           projectionType      () const;

  TypeId           fileTypeId          () const;

  TypeId           useTypeId           () const;

  void             getCells            (size_t offset,
                                        size_t nrCells,
                                        void* buffer) const;

  void             getCell             (size_t row,
                                        size_t col,
                                        void* value) const;

  bool             isMV                (void const* value) const;

  boost::any       min                 () const;

  boost::any       max                 () const;

  void*            malloc              (size_t nrCells) const;

  bool             hasLegend           () const;

  Table            legend              () const;

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
