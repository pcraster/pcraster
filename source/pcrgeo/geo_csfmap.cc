#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_CSFMAP
#include "geo_csfmap.h"
#define INCLUDED_GEO_CSFMAP
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

#ifndef  INCLUDED_COM_PATHINFO
#include "com_file.h"
#define  INCLUDED_COM_PATHINFO
#endif

#ifndef  INCLUDED_GEO_EXCEPTION
#include "geo_exception.h"
#define  INCLUDED_GEO_EXCEPTION
#endif

#ifndef  INCLUDED_CSF
#include "csf.h"
#define  INCLUDED_CSF
#endif

#ifndef  INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define  INCLUDED_GEO_RASTERSPACE
#endif

#ifndef  INCLUDED_GEO_UTIL
#include "geo_util.h"
#define  INCLUDED_GEO_UTIL
#endif



/*!
  \file
  encapsulates and enriches the C csf library

*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------




//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

/*!
   Open an existing map. Default in read only mode.
  \param     fn Filename of the csf raster file to open.
  \param allowUpdate is true then updates/writes are also valid

  \exception geo::NotA_PCRasterMap If the file exists but is not a csf map
  \exception com::OpenFileError If the file could not be opened.

  This constructor opens an existing csf raster file in mode \a m.
*/
geo::CSFMap::CSFMap(const std::string &fn, bool allowUpdate)
  : d_fn(fn), d_map(0)

{
  open(allowUpdate);
}

geo::CSFMap::CSFMap(const char *fn, bool allowUpdate)
  : d_fn(fn), d_map(0)
{
  open(allowUpdate);
}



geo::CSFMap::CSFMap(const com::PathName &fn, bool allowUpdate)
  : d_fn(fn.toString()), d_map(0)

{
  open(allowUpdate);
}



//! Copy constructor, use only on read only!
geo::CSFMap::CSFMap(const CSFMap &rhs)
  : d_fn(rhs.d_fn), d_map(0)

{
  PRECOND(MopenPerm(rhs.d_map) == M_READ);
  if(rhs.isOpen())
    open(false);
}



/*!
  \param     fn Filename of new csf raster file.
  \param     nr Number of rows.
  \param     nc Number of columns.
  \param     vs Valuescale.
  \param     proj Projection.
  \param     left Left coordinate.
  \param     top Top coordinate.
  \param     a Angle.
  \param     cs Cell size.
  \param     cr In-file cell representation. If CR_UNDEFINED then the
             default cell size for the value scale of this map.
  \exception com::FileError as thrown by create()

  This constructor creates a new csf raster file.
*/
geo::CSFMap::CSFMap(const std::string &fn, size_t nr, size_t nc,
         CSF_VS vs, CSF_PT proj, REAL8 left, REAL8 top, REAL8 a, REAL8 cs, CSF_CR cr)
  : d_fn(fn), d_map(0)

{
  create(nr, nc, vs, proj, left, top, a, cs,cr);
}

//! create csf map from space description
/*! \exception com::FileError as thrown by create()
 */
geo::CSFMap::CSFMap(const std::string& name,const geo::RasterSpace& rs,
      CSF_VS vs, CSF_CR cr): d_fn(name), d_map(0)
{
  create(rs.nrRows(), rs.nrCols(), vs,
    geoProjToCsf(rs.projection()), rs.left(), rs.top(), rs.angle(),
  rs.cellSize(),cr);
}

//! create csf map from space description
/*! \exception com::FileError as thrown by create()
 */
geo::CSFMap::CSFMap(const com::PathName& name,const geo::RasterSpace& rs,
      CSF_VS vs, CSF_CR cr): d_fn(name.toString()), d_map(0)
{
  create(rs.nrRows(), rs.nrCols(), vs,
    geoProjToCsf(rs.projection()), rs.left(), rs.top(), rs.angle(),
  rs.cellSize(),cr);
}

//! clone  csf map from other map description
/*! \exception com::FileError as thrown by create()
 */
geo::CSFMap::CSFMap(const std::string& name,const CSFMap& clone,
      CSF_VS vs, CSF_CR cr): d_fn(name), d_map(0)
{
  RasterSpace rs(clone.rasterSpace());
  create(rs.nrRows(), rs.nrCols(), vs,
    geoProjToCsf(rs.projection()), rs.left(), rs.top(), rs.angle(),
  rs.cellSize(),cr);
}



geo::CSFMap::~CSFMap()
{
  close();
}

//! rasterSpace
geo::RasterSpace geo::CSFMap::rasterSpace() const
{
  return RasterSpace(nrRows(),nrCols(), cellSize(),
                     left(), top(), geo::csfProjToGeo(projection()), angle());
}



/*!
  \param     allowUpdate if updating/writing is also allowed on this existing map.
  \exception com::OpenFileError as in com::testOpenForReading could not be opened.
  \exception geo::NotA_PCRasterMap If the file exists but is not a csf map
  \sa        close(), isOpen()
*/
void geo::CSFMap::open(bool allowUpdate)
{
#ifdef DEBUG_DEVELOP
  PRECOND(!d_map);
#endif
  MOPEN_PERM p = allowUpdate ? M_READ_WRITE : M_READ;
  /* CW Note: the other M_WRITE (write only is an historical artifact)
   *       I have not seen any use of it
   */
  if (!allowUpdate)
        com::testOpenForReading(d_fn);
  // else com::testOpenForUpdate(d_fn);

  d_map = Mopen(d_fn.c_str(), p);

  if(!d_map) {
   if (Merrno == NOT_CSF)
    throw NotA_PCRasterMap(d_fn);
   throwFileError("error opening raster",true);
  }
}

//! throw a com_FileError
/*! throws a com_FileError of the format:
 *  <br> \a prefix file
 *  <br> \a prefix file : MstrError
 *  <br>
 * The second format is used if \a mErrorDefined is true. For example, put and write of data cells
 * does not set Merrno
 * \param prefix message prefix
 * \param mErrnoDefined if true, append the value of MstrError to message
 * \exception com_OpenFileError always
 */
void geo::CSFMap::throwFileError(
  const std::string& prefix,
  bool  mErrnoDefined)
{
    std::string msg = prefix;
    if (mErrnoDefined) {
        msg += ": "; // leestekens tegen het laatste woord, zie hier!
        msg += MstrError();
    }
    throw com::FileError(d_fn,msg);
}


/*!
  \param     nr Number of rows.
  \param     nc Number of columns.
  \param     vs Valuescale.
  \param     proj Projection.
  \param     left Left coordinate.
  \param     top Top coordinate.
  \param     a Angle.
  \param     cs Cell size.
  \param     cr In-file cell representation. If CR_UNDEFINED then the
             default cell size for the value scale of this map.
  \exception com_FileError If the csf raster file could not be created.
*/
void geo::CSFMap::create(size_t nr, size_t nc, CSF_VS vs,
         CSF_PT proj, REAL8 left, REAL8 top, REAL8 a, REAL8 cs, CSF_CR cr)
{
#ifdef DEBUG_DEVELOP
  PRECOND(!d_map);
#endif
  com::testOpenForWriting(d_fn);
  if (cr == CR_UNDEFINED)
    cr = ValueScale2CellRepr(vs).defaultCR();

  d_map = Rcreate(d_fn.c_str(), nr, nc, cr, vs, proj, left, top, a, cs);

  if(!d_map)
    throwFileError("error creating raster",true);
}



/*!
  \exception com_FileError If the csf raster file could not be closed.
  \sa        open(), isOpen()
*/
void geo::CSFMap::close()
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_map);
#endif

  if(Mclose(d_map))
    throwFileError("error closing raster",true);
  d_map = 0;
}



/*!
  \return    True if the filepointer is open.
  \sa        open(), close()
*/
bool geo::CSFMap::isOpen() const
{
  return d_map != 0;
}



/*!
  \param     cr In-app cell representation.
  \exception com_FileError If the csf raster file can't obey the conversion
             rules for converting between the in-file and in-app cell
             representation.
*/
void geo::CSFMap::useAs(CSF_CR cr)
{
  if(RuseAs(d_map, cr))
            throwFileError("conversion rules not obeyed",true);
}



/*!
  \param     cs New cell size.
  \exception com_FileError If the new cell size could not be set.
*/
void geo::CSFMap::putCellSize(REAL8 cs)
{
  if(RputCellSize(d_map, cs) < 0.0)
    throwFileError("error setting cell size:",true);
}


/*!
  \param     a New cell size.
  \exception com_FileError If the new angle could not be set.
*/
void geo::CSFMap::putAngle(REAL8 a)
{
  if(RputAngle(d_map, a) < 0.0)
    throwFileError("error setting angle:",true);
}



/*!
  \return    Filename of the csf raster file.
*/
const std::string &geo::CSFMap::filename() const
{
  return d_fn;
}



/*!
  \return    In-file cell representation.
*/
CSF_CR geo::CSFMap::cellRepr() const
{
  return RgetCellRepr(d_map);
}



/*!
  \return    Value scale.
*/
CSF_VS geo::CSFMap::valueScale() const
{
  return RgetValueScale(d_map);
}



/*!
  \return    Projection.
*/
CSF_PT geo::CSFMap::projection() const
{
  return MgetProjection(d_map);
}



/*!
  \return    Left coordinate.
*/
REAL8 geo::CSFMap::left() const
{
  return RgetXUL(d_map);
}



/*!
  \return    Top coordinate.
*/
REAL8 geo::CSFMap::top() const
{
  return RgetYUL(d_map);
}



/*!
  \return    Angle.
*/
REAL8 geo::CSFMap::angle() const
{
  return RgetAngle(d_map);
}



/*!
  \return    Cell size.
*/
REAL8 geo::CSFMap::cellSize() const
{
  return RgetCellSize(d_map);
}



/*!
  \return    Number of cols.
*/
size_t geo::CSFMap::nrCols() const
{
  return RgetNrCols(d_map);
}



/*!
  \return    Number of rows.
*/
size_t geo::CSFMap::nrRows() const
{
  return RgetNrRows(d_map);
}



/*!
  \return    Number of cells.

  The number of cells is equal to nrCols() * nrRows().
*/
size_t geo::CSFMap::nrCells() const
{
  return nrCols() * nrRows();
}



/*!
  \param     buf Buffer large enough to contain all cell values in the in-app
             cell representation.
  \exception com_FileError If an error occured while reading the cells.
  \sa        getCells(size_t, size_t, void *)
*/
void geo::CSFMap::getCells(void *buf)
{
  if(RgetSomeCells(d_map, 0, nrCells(), buf) != nrCells())
    throwFileError("error reading cells", false);
}

/*!
  \pre       RputDoNotChangeValues(d_map); a const void buffer suffice
  \param   buf containing all cell values
  \exception com_FileError if write fails
  \sa        putCells(size_t, size_t, const void*)
*/
void geo::CSFMap::putCells(const void *buf)
{
  PRECOND(RputDoNotChangeValues(d_map));
  if(RputSomeCells(d_map, 0, nrCells(), const_cast<void *>(buf)) != nrCells())
    throwFileError("error writing cells", false);
}



//! Write a stream of cells.
/*!
  \param     offset Offset from pixel (row,col) = (0,0).
  \param     nrCells number of cells to be read
  \param     buffer Buffer with \a nrCells values.
  \exception com::FileError If write fails.
  \sa        putCells(const void*)
*/
void geo::CSFMap::putCells(size_t offset, size_t nrCells, const void* buffer)
{
  PRECOND(RputDoNotChangeValues(d_map));
  if(RputSomeCells(d_map, offset, nrCells,
                   const_cast<void *>(buffer)) != nrCells) {
    throwFileError("error writing cells", false);
  }
}



//! Write all cells with the same value stored in \a buf.
/*!
  \exception com_FileError if write fails
 */
void geo::CSFMap::putNonSpatial(const void *buf)
{
  size_t n= nrCells();
  for(size_t i=0; i < n; i++)
   if (RputSomeCells(d_map,i,1, const_cast<void *>(buf)) != 1)
          throwFileError("error writing cells", false);
}

/*!
  \param     off Offset in the raster.
  \param     nc Number of cells to read.
  \param     buf Buffer large enough to contain \a nc cell values in the in-app
             cell representation.
  \exception com_FileError If an error occured while reading the cells.
  \sa        getCells(void *)
*/
void geo::CSFMap::getCells(size_t off, size_t nc, void *buf)
{
  if(RgetSomeCells(d_map, off, nc, buf) != nc)
    throwFileError("error reading cells", false);
}



/*!
  \param     m Pointer to value which will be set to the minimum.
  \return    True if the raster contains a minimum, false if the raster only
             contains missing values.
  \sa        max()
*/
bool geo::CSFMap::min(void *m) const
{
  return RgetMinVal(d_map, m) != 0;
}


/*!
  \param     m Pointer to value which will be set to the maximum.
  \return    True if the raster contains a maximum, false if the raster only
             contains missing values.
  \sa        min()
*/
bool geo::CSFMap::max(void *m) const
{
  return RgetMaxVal(d_map, m) != 0;
}

/*!
  \param     min value which will be set to the minimum.
  \param     max value which will be set to the minimum.
  \return    True if the raster contains a minimum and maximum,
             false if the raster only contains missing values.
  \sa        min(), max()
*/
bool geo::CSFMap::getMinMax(double &min, double& max) const
{
  // save current
  CSF_CR currCr= RgetUseCellRepr(d_map);
  RuseAs(d_map, CR_REAL8);
  bool minMaxSet = (RgetMinVal(d_map, &min) != 0)
                 & (RgetMaxVal(d_map, &max) != 0);
              // YES, bitwise &,eval both, do not shortcut
  // set to saved
  RuseAs(d_map,currCr);
  return minMaxSet;
}

/*!
  \param     min minimum value.
  \param     max maximum value.
*/
void geo::CSFMap::setMinMax(double min, double max)
{
  PRECOND(MopenPerm(d_map) != M_READ);
  // save current
  CSF_CR currCr= RgetUseCellRepr(d_map);

  RuseAs(d_map, CR_REAL8);
  RputMinVal(d_map, &min);
  RputMaxVal(d_map, &max);

  // set to saved
  RuseAs(d_map,currCr);
}



/*!
  \brief     Returns the CSF version of the layered map pointer.
  \return    CSF version.
*/
UINT4 geo::CSFMap::version() const
{
  return MgetVersion(d_map);
}



//! Returns true if the map has a legend.
/*!
  \return    true if the map has a legend.
  \sa        hasLegend(), nrLegendEntries()
*/
bool geo::CSFMap::hasLegend() const
{
  return (MattributeAvail(d_map, ATTR_ID_LEGEND_V1) ||
                   MattributeAvail(d_map, ATTR_ID_LEGEND_V2)) != 0;
}



//! Returns the number of legend entries plus 1 for the name of the legend.
/*!
  \return    Number of legend entries plus 1 or 0 if there's no legend.
  \sa        hasLegend(), legend()
*/
size_t geo::CSFMap::nrLegendEntries() const
{
  return MgetNrLegendEntries(d_map);
}



//! Returns the legend of the map.
/*!
  \return    Legend of the map.
  \exception com::Exception If an error occurs while reading the legend.
  \warning   The returned legend is empty if the map has no in-file legend.
  \sa        hasLegend(), nrLegendEntries()
*/
com::Legend<INT4> geo::CSFMap::legend() const
{
  com::Legend<INT4> legend;

  if(hasLegend()) {

    CSF_LEGEND *l = 0;

    try {
      size_t n = nrLegendEntries();
      PRECOND(n > 0);
      l = static_cast<CSF_LEGEND *>(new CSF_LEGEND[n]);

      if(MgetLegend(d_map, l) == 0) {
        throw com::Exception("Unable to read csf-legend");
      }

      legend.setTitle(l[0].descr);
      legend.setNrClasses(n - 1);

      com::Legend<INT4>::iterator it = legend.begin();
      for(size_t i = 1; i < n; ++i) {
        (*it).setValue(static_cast<INT4>(l[i].nr));
        (*it).setDescr(l[i].descr);
        ++it;
      }
    }
    catch(...) {
      delete[] l;
      throw;
    }

    delete[] l;
  }

  return legend;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


