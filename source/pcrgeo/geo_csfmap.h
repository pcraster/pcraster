#ifndef INCLUDED_GEO_CSFMAP
#define INCLUDED_GEO_CSFMAP

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

#ifndef INCLUDED_COM_LEGEND
#include "com_legend.h"
#define INCLUDED_COM_LEGEND
#endif



struct MAP;

namespace com {
  class PathName;
}

namespace geo {

class RasterSpace;


/*!
  \class CSFMap
  \brief The CSFMap class can be used for creating, reading and writing
         individual csf raster layers.

  Most member functions are just wrappers around functions from the PCRaster
  csf library. Use the documentation of that library in conjunction with the
  comments of this class!
*/
class CSFMap {

private:

  //! Filename.
  std::string      d_fn;

  //! Csf map filepointer. It is always connected to an open file.
  MAP *            d_map;

  //! Assignment operator. NOT IMPLEMENTED.
  CSFMap &         operator=           (const CSFMap &);

  //! Frees dynamically allocated memory.
  void             clean               ();

  //! Opens the map filepointer.
  void             open                (bool allowUpdate);

  //! Creates a map filepointer.
  void             create              (size_t nr,
                                        size_t nc,
                                        CSF_VS vs,
                                        CSF_PT proj,
                                        REAL8 left,
                                        REAL8 top,
                                        REAL8 a,
                                        REAL8 cs,
                                        CSF_CR cr=CR_UNDEFINED);

  //! Closes the map filepointer.
  void             close               ();

  //! Returns true if the file is opened.
  bool             isOpen              () const;

  //! Throw a com::FileError if something goes wrong
  void             throwFileError      (const std::string& prefix,
                                        bool  mErrorDefined);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   CSFMap              (const std::string &fn,
                                        bool allowUpdate=false);

                   CSFMap              (const char        *fn,
                                        bool allowUpdate=false);

                   CSFMap              (const com::PathName &fn,
                                        bool allowUpdate=false);

  //! Constructor.
                   CSFMap              (const std::string &fn,
                                        size_t nr,
                                        size_t nc,
                                        CSF_VS vs,
                                        CSF_PT proj,
                                        REAL8 left,
                                        REAL8 top,
                                        REAL8 a,
                                        REAL8 cs,
                                        CSF_CR cr=CR_UNDEFINED);

                   CSFMap              (const std::string& name,
                                        const RasterSpace& rs,
                                        CSF_VS vs,
                                        CSF_CR cr=CR_UNDEFINED);

                   CSFMap              (const com::PathName& name,
                                        const RasterSpace& rs,
                                        CSF_VS vs,
                                        CSF_CR cr=CR_UNDEFINED);

                   CSFMap              (const std::string& name,
                                        const CSFMap& clone,
                                        CSF_VS vs,
                                        CSF_CR cr=CR_UNDEFINED);

  //! Copy constructor.
                   CSFMap              (const CSFMap &rhs);

  //! Destructor.
  /* virtual */    ~CSFMap             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Set in app cell representation.
  void             useAs               (CSF_CR cr);

  //! Set cell size of raster.
  void             putCellSize         (REAL8 cs);

  //! Set angle of raster.
  void             putAngle            (REAL8 a);

  //! set  minimum and maximum value of map
  void setMinMax(double min, double max);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  RasterSpace        rasterSpace       () const;

  //! Returns the filename of the raster.
  const std::string &filename          () const;

  //! Returns a readable version of the encapsulated csf MAP filepointer.
                   operator const MAP *() const;

  //! Returns a writable version of the encapsulated csf MAP filepointer.
                   operator MAP *      ();

  //! Returns the in app cell representation.
  CSF_CR           cellRepr            () const;

  //! Returns the valuescale.
  CSF_VS           valueScale          () const;

  //! Returns the projection.
  CSF_PT           projection          () const;

  //! Returns the left coordinate of the raster.
  REAL8            left                () const;

  //! Returns the top coordinate of the raster.
  REAL8            top                 () const;

  //! Returns the angle of the raster.
  REAL8            angle               () const;

  //! Returns the cell size of the raster.
  REAL8            cellSize            () const;

  //! Returns the number of cols in the raster.
  size_t           nrCols              () const;

  //! Returns the number of rows in the raster.
  size_t           nrRows              () const;

  //! Returns the number of cells in the raster.
  size_t           nrCells             () const;

  //! Reads all cells and writes them to \a buf.
  void             getCells            (void *buf);

  void             putCells            (const void *buf);

  void             putCells            (size_t offset,
                                        size_t nrCells,
                                        const void* buffer);

  //! Write all cells with the same value stored in \a buf.
  void             putNonSpatial       (const void *buf);

  //! Reads \a nc cells, offset by \a off and writes them to \a buf.
  void             getCells            (size_t off,
                                        size_t nc,
                                        void *buf);

  //! Sets the value pointed to by \a m to the minimum value in the raster.
  bool             min                 (void *m) const;

  //! Sets the value pointed to by \a m to the maximum value in the raster.
  bool             max                 (void *m) const;

  //! get  minimum and maximum value of map
  bool getMinMax(double &min, double& max) const;

  UINT4            version             () const;

  bool             hasLegend           () const;

  size_t           nrLegendEntries     () const;

  com::Legend<INT4> legend             () const;

};

//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------


/*!
  \fn        geo::CSFMap::operator const MAP *() const
  \return    Readable version of the MAP file pointer.
*/
inline CSFMap::operator const MAP *() const
{ return d_map; }

/*!
  \fn        geo::CSFMap::operator MAP *()
  \return    Writable version of the MAP file pointer.
  \warning   Be carefull with it! It's probably better to extend this class
             to contain the functionality you're writing right now.
*/
inline CSFMap::operator MAP *()
{ return d_map; }



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------




} // namespace geo

#endif
