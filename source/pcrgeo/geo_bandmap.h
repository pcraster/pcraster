#ifndef INCLUDED_GEO_BANDMAP
#define INCLUDED_GEO_BANDMAP

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif


namespace geo {

class RasterSpace;


//! The BandMap class models the BIL/BSQ/BIP format
/*!
  This first version only supports a subset of the BIL format
  as needed by Cees for the WL/Habitat project using the
  description supplied by WL.

  Interface is named after CSF interface as close as possible.

  The format describes 2 files: the .BIL file with the data and
  the .HDR file with ASCII formatted header information.
  The header file is case insensitive, all capitals seems preffered.

  example header, in preffered order:
  <pre>
  BYTEORDER      M
  LAYOUT       BIL
  NROWS         1800
  NCOLS         1800
  NBANDS        1
  NBITS         16
  BANDROWBYTES         3600
  TOTALROWBYTES        3600
  BANDGAPBYTES         0
  </pre>

  There is only marginal support for multiband files. If an existing BIL is multi
  band (NBANDS > 1), then the first band in read and returned.

  If NODATA is not present then it is assumed that no missing values are present
  in the data. Note that getCellsAsType (e.g. getCellsAsUINT1()) return data with
  the standard missing value. If that value is present in the data file a  real
  value (not the NODATA) then it is still regarded as the MV.

  Note that acoording to JJ some apps do write a NODATA as min and max to the .stx
  file, even though the maps do contains other NODATA values.

  The format has a standard projection of geo::Projection::YIncrB2T, no other projection 
  is definable in the header.
  ESRI software uses an additional file, named the world file, to define the
  projection.

  \todo
    zie Gdal code: /home/cees/tmp/gdal-1.1.9/frmts/raw
    make byteorder flexibeler (1e letter I/M/L) wegschrijven (I of L?)
    Gdal doet het met 32 bit als integer behalve als er een 
     byteorder (L|M)SBFIRST volledig staat, volslagen MAF.



*/
class BandMap : public boost::noncopyable
{
 public:
   typedef enum  Layout { BIL, BIP, BSQ } Layout;

   static  const std::string d_hostByteOrder;
   static  const std::string d_otherByteOrder;
private:

  //! pathName without the extension
  com::PathName    d_pn;

  //! rows and columns, 0 if header is not yet analyzed
  size_t d_nrRows, d_nrCols;

  size_t  d_nrBands;
  CSF_CR  d_cellRepr;

  bool    d_hasHostByteOrder;

  //! const FTTB since we only support BIL
  const Layout  d_layout;
  size_t  d_skipBytes, /* d_bandGapBytes, */ d_bandRowBytes,d_totalRowBytes;
  double  d_cellSize;
  //! centre of upper-left pixel
  double  d_ulXMap,d_ulYMap;

  bool    d_mvIsSet;
  double  d_mvValue;

  //! Read the header, open the data stream.
  void             open                (bool allowUpdate);

  //! create the header file
  void writeHeader();

  void initPathName();

  com::PathName dataPathName() const;
  com::PathName stxPathName() const;
  com::PathName headerPathName() const;

  size_t nrBits() const;

/*
  //! Assignment operator. NOT IMPLEMENTED.
  BandMap &         operator=           (const BandMap &);

  //! Frees dynamically allocated memory.
  void             clean               ();


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

  //! Throw a com_FileError if something goes wrong
  void             throwFileError      (const std::string& prefix,
                                        bool  mErrorDefined);
*/

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BandMap(const com::PathName &fn,
                           bool allowUpdate=false);

                   BandMap(const com::PathName &pn,
                           const RasterSpace &rs,
                           CSF_CR  cellRepr,
                           bool    mvIsSet,
                           double  mvValue);

/*
                   BandMap            (const std::string &fn,
                                        size_t nr,
                                        size_t nc,
                                        CSF_VS vs,
                                        CSF_PT proj,
                                        REAL8 left,
                                        REAL8 top,
                                        REAL8 a,
                                        REAL8 cs,
                                        CSF_CR cr=CR_UNDEFINED);

                   BandMap              (const std::string& name,
                                        const RasterSpace& rs,
                                        CSF_VS vs,
                                        CSF_CR cr=CR_UNDEFINED);

                   BandMap              (const com::PathName& name,
                                        const RasterSpace& rs,
                                        CSF_VS vs,
                                        CSF_CR cr=CR_UNDEFINED);

                   BandMap              (const std::string& name,
                                        const BandMap& clone,
                                        CSF_VS vs,
                                        CSF_CR cr=CR_UNDEFINED);

 */
  //! Copy constructor.
                   BandMap              (const BandMap &rhs);

  //! Destructor.
  /* virtual */    ~BandMap             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS SPECIFIC TO BAND MAP FORMAT
  // NB mvIsSet en mvValue kunnen uiteraard wel in CSFMap geimplementeerd worden
  //----------------------------------------------------------------------------

  bool             mvIsSet               () const;
  double           mvValue               () const;

  //----------------------------------------------------------------------------
  // ACCESSORS GENERIC TO OTHER MAP FORMATS
  //----------------------------------------------------------------------------
  /* DO NOT NAME THEM ALL getCells and rely on argument for polymorhism, too prone for error
   */

  void             getCellsRaw         (void  *buf) const;
  void             getCellsAsUINT1     (UINT1 *buf) const;
  void             getCellsAsINT4      (INT4  *buf) const;
  void             getCellsAsREAL4     (REAL4 *buf) const;

  void             putCellsRaw         (const void *buf) const;
  void             putCellsAsUINT1     (const UINT1 *buf) const;
  void             putCellsAsINT4      (const INT4  *buf) const;
  void             putCellsAsREAL4     (const REAL4 *buf) const;

  RasterSpace      rasterSpace         () const;

  CSF_CR           cellRepr            () const;

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


  void             putCells            (size_t offset,
                                        size_t nrCells,
                                        const void* buffer);

  //! Write all cells with the same value stored in \a buf.
  void             putNonSpatial       (const void *buf);

  //! Reads \a nc cells, offset by \a off and writes them to \a buf.
  void             getCells            (size_t off,
                                        size_t nc,
                                        void *buf);

};

//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

template <typename T>
 void createBil(const com::PathName &pn,
               const RasterSpace &rs,
               const T *data, T mvValue);
template <typename T>
 void createBil(const com::PathName &pn,
               const RasterSpace &rs,
               const T *data);

} // namespace geo

#endif
