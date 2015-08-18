#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_BANDMAP
#include "geo_bandmap.h"
#define INCLUDED_GEO_BANDMAP
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
#ifndef INCLUDED_IOMANIP
#include <iomanip>
#define INCLUDED_IOMANIP
#endif
// PCRaster library headers.
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif
#ifndef INCLUDED_COM_KEYVALUETABLE
#include "com_keyvaluetable.h"
#define INCLUDED_COM_KEYVALUETABLE
#endif
#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif
// Module headers.
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif
#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif

/*!
  \file The Band map formats aka BIL
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------
namespace geo {
#ifdef CPU_LITTLE_ENDIAN
const std::string BandMap::d_hostByteOrder("I");
const std::string BandMap::d_otherByteOrder("M");
#elif defined(CPU_BIG_ENDIAN)
const std::string BandMap::d_hostByteOrder("M");
const std::string BandMap::d_otherByteOrder("I");
#else
# error NO ENDIAN DEFINED
#endif
}


//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! ensure extension is stripped
void geo::BandMap::initPathName()
{
  if (d_pn.hasExtension())
   d_pn.removeExtension();
}

//! create new band map
/*!
 * \param pn filename to create (.bil extension is optional)
 * \param rs description of file
 * \param cellRepr one of the supported types (CR_{UINT1,INT2,REAL4})
 *                 or CR_INT4, INT4 is converted to INT2, hence a
 *                 succesive call to cellRepr() will return CR_INT2!
 * \param mvIsSet  is the \a mvValue argument set? If false then the
 *                 PCRaster CSF MV constants are used for the integer
 *                 types and -999 for the REAL4 type.
 * \param mvValue  the mvValue to write to the band map and set as
 *                 nodata value in the header.
 *
 * writes header file completely, a succesive call to one of the
 * putCell functions will create the data file.
 * Note that this ctor will ALWAYS write a NODATA value, see discussion
 * of the \a mvIsSet argument
 */
geo::BandMap::BandMap(const com::PathName &pn,
                      const RasterSpace &rs,
                      CSF_CR  cellRepr,
                      bool    mvIsSet,
                      double  mvValue):
  d_pn(pn),
  d_nrRows(rs.nrRows()), d_nrCols(rs.nrCols()),
  d_nrBands(1),
  d_cellRepr(cellRepr == CR_INT4 ? CR_INT2 : cellRepr),
  d_hasHostByteOrder(true),d_layout(BIL),
  d_skipBytes(0), // d_bandGapBytes(0),
  d_bandRowBytes(d_nrCols*CELLSIZE(d_cellRepr)),
  d_totalRowBytes(d_nrCols*CELLSIZE(d_cellRepr)),
  d_cellSize(rs.cellSize()),
  d_ulXMap(rs.left()+0.5*d_cellSize),
  d_ulYMap(rs.top() -0.5*d_cellSize),
  d_mvIsSet(mvIsSet),d_mvValue(mvValue)
{
  bool supportedCellRepr=d_cellRepr==CR_UINT1||
                         d_cellRepr==CR_INT2 ||
                         d_cellRepr==CR_REAL4 ;
#ifdef DEBUG
  PRECOND(supportedCellRepr);
#endif
  if(!supportedCellRepr)
    throw com::Exception("programming error unsupported cell representation");

  if (!d_mvIsSet) {
    d_mvIsSet=true;
    switch(d_cellRepr) {
      case CR_UINT1: d_mvValue=MV_UINT1; break;
      case  CR_INT2: d_mvValue=MV_INT2;  break;
      case CR_REAL4: d_mvValue=-999   ; break;
      default:;
    }
  }

  initPathName();
  writeHeader();
}

/*!
   Open an existing map. Default in read only mode.
  \param pn Filename of the raster file to open. The .bil extension is optional
  \param allowUpdate is true then updates/writes are also valid

  \exception geo::NotA_BandMap If the file exists but is not a (SUPPORTED)
             band map
  \exception com::OpenFileError If the file could not be opened.

  \todo  Linux only: make more flexible to search .HDR and .hdr and so on
         mayb if d_pn.baseName is all Uppercase then .HDR .BIL, lowercase
         otherwise --> addCaseExtension  in pathname!
*/
geo::BandMap::BandMap(const com::PathName &pn, bool allowUpdate):
  d_pn(pn),
  d_nrRows(0), d_nrCols(0),   // means not set
  d_nrBands(1),
  d_cellRepr(CR_UINT1),
  d_hasHostByteOrder(true),d_layout(BIL),
  d_skipBytes(0), // d_bandGapBytes(0),
  d_cellSize(1),
  d_ulXMap(0),
  d_mvIsSet(false)
{
  initPathName();
  open(allowUpdate);
}

namespace geo {

template <>
 void createBil<UINT1>(const com::PathName &pn,
                    const RasterSpace   &rs,
                    const UINT1         *data,
                    UINT1                mvValue)
{
BandMap bm(pn,rs,CR_UINT1,true,mvValue);
 // write all data
 bm.putCellsAsUINT1(data);
}

template <>
 void createBil<UINT1>(const com::PathName &pn,
                    const RasterSpace   &rs,
                    const UINT1         *data)
{
 BandMap bm(pn,rs,CR_UINT1,false,0);
 // write all data
 bm.putCellsAsUINT1(data);
}

template <>
 void createBil<INT4>(const com::PathName &pn,
                    const RasterSpace   &rs,
                    const INT4          *data,
                    INT4                 mvValue)
{
 BandMap bm(pn,rs,CR_INT4,true,mvValue);
 // write all data
 bm.putCellsAsINT4(data);
}

template <>
 void createBil<INT4>(
     const com::PathName &pn,
     const RasterSpace   &rs,
     const INT4         *data)
{
 BandMap bm(pn,rs,CR_INT4,false,0);
 // write all data
 bm.putCellsAsINT4(data);
}

template <>
 void createBil<REAL4>(const com::PathName &pn,
                    const RasterSpace   &rs,
                    const REAL4          *data,
                    REAL4                 mvValue)
{
 BandMap bm(pn,rs,CR_REAL4,true,mvValue);
 // write all data
 bm.putCellsAsREAL4(data);
}

template <>
 void createBil<REAL4>(
     const com::PathName &pn,
     const RasterSpace   &rs,
     const REAL4         *data)
{
 BandMap bm(pn,rs,CR_REAL4,false,0);
 // write all data
 bm.putCellsAsREAL4(data);
}

} // namespace geo

//! dtor
geo::BandMap::~BandMap()
{
}

com::PathName geo::BandMap::dataPathName() const
{
  com::PathName dataPn(d_pn);
  dataPn.addExtension("bil");
  return dataPn;
}

com::PathName geo::BandMap::stxPathName() const
{
  com::PathName dataPn(d_pn);
  dataPn.addExtension("stx");
  return dataPn;
}

com::PathName geo::BandMap::headerPathName() const
{
  com::PathName hdrPn(d_pn);
  hdrPn.addExtension("hdr");
  return hdrPn;
}

/*!
  \param     allowUpdate if updating/writing is also allowed on this existing map.
  \exception com::FileFormatError if header and data are missing, not consistent or
               has  unsupported features.
  \sa        close(), isOpen()

  \todo check if mv is in type range

  \todo check here is .bil has correct (large enough) size, so this
        method is quite complete on upfront checking
*/
void geo::BandMap::open(bool /*allowUpdate*/)
{
  com::testOpenForReading(dataPathName());

  com::PathName hdrPn(headerPathName());
  std::ifstream hdrStream;
  com::open(hdrStream,hdrPn);

  com::KeyValueTable kvt;
  kvt.setDiscardUnknownKeys(true);
  com::GreaterThan<double>  gt0(0);
  com::GreaterThanEqualTo<double>   ge0(0);

  com::KeyValueInteger nrRows("NROWS",&gt0);
  kvt.insertKey(nrRows,true);

  com::KeyValueInteger nrCols("NCOLS",&gt0);
  kvt.insertKey(nrCols,true);

  com::KeyValueInteger nrBands("NBANDS");
  nrBands.setInterval(gt0);
  kvt.insertKey(nrBands);

  com::KeyValueInteger nrBits("NBITS",&gt0);
  kvt.insertKey(nrBits);

  com::KeyValueInteger bandRowBytes("BANDROWBYTES",&gt0);
  kvt.insertKey(bandRowBytes);

  com::KeyValueInteger totalRowBytes("TOTALROWBYTES",&gt0);
  kvt.insertKey(totalRowBytes);

  // Only needed in BSQ
  com::KeyValueInteger bandGapBytes("BANDGAPBYTES",&ge0);
  kvt.insertKey(bandGapBytes);

  com::KeyValueInteger skipBytes("SKIPBYTES",&ge0);
  kvt.insertKey(skipBytes);

  com::KeyValueDouble ulXMap("ULXMAP");
  kvt.insertKey(ulXMap);
  com::KeyValueDouble ulYMap("ULYMAP");
  kvt.insertKey(ulYMap);
  com::KeyValueDouble xDim("XDIM",&gt0);
  kvt.insertKey(xDim);
  com::KeyValueDouble yDim("YDIM",&gt0);
  kvt.insertKey(yDim);

  com::KeyValueDouble mv("NODATA");
  kvt.insertKey(mv);

  com::KeyValueEnum byteOrder("BYTEORDER");
  byteOrder.insert(d_hostByteOrder);
  byteOrder.insert(d_otherByteOrder);
  kvt.insertKey(byteOrder);

  com::KeyValueEnum layout("LAYOUT");
  // FTTB only BIL
  layout.insert("BIL");
  kvt.insertKey(layout);

  std::string line;
  try {
   while(std::getline(hdrStream, line)) {
     std::vector<std::string> tokens = com::split(line);
     // ignore debris or comments
     // header lines may have comments after keyword value
     // so more than 2 tokens is ok
     if (tokens.size() < 2)
       continue;
     kvt.add(tokens[0],tokens[1]);
    } // while
    kvt.checkRequired();

    // the required ones
    d_nrRows=nrRows.value(kvt);
    d_nrCols=nrCols.value(kvt);

    nrBands.setConditional(d_nrBands,kvt);

    // before nrBits if one want to check on mv range
    // TODO check if mv is in type range
    if (kvt.isSet(mv)) {
      d_mvIsSet=true;
      d_mvValue = mv.value(kvt);
    }

    if (kvt.isSet(nrBits)) {
      switch(nrBits.value(kvt)) {
        case 8 : d_cellRepr = CR_UINT1;
                 break;
        case 16: d_cellRepr = CR_INT2;
                 break;
        case 32: d_cellRepr = CR_REAL4;
                 break;
        default:
           kvt.throwIllegalValue(nrBits,"not supported");
      }
    }

    if (kvt.isSet(byteOrder))
      d_hasHostByteOrder = (d_hostByteOrder == byteOrder.configValue(kvt));

    skipBytes.setConditional(d_skipBytes,kvt);

    size_t defaultBandRowBytes = d_nrCols*CELLSIZE(d_cellRepr);
    d_bandRowBytes = defaultBandRowBytes;
    bandRowBytes.setConditional(d_bandRowBytes,kvt);
    if (d_bandRowBytes != defaultBandRowBytes)
           kvt.throwIllegalValue(bandRowBytes,"not supported");

    size_t defaultTotalRowBytes = d_nrCols*CELLSIZE(d_cellRepr);
    d_totalRowBytes = defaultTotalRowBytes;
    totalRowBytes.setConditional(d_totalRowBytes,kvt);
    if (d_totalRowBytes != defaultTotalRowBytes)
           kvt.throwIllegalValue(totalRowBytes,"not supported");


    { double x=1,y=1;
      if (kvt.isSet(xDim) && kvt.isSet(yDim)) {
       xDim.setConditional(x,kvt);
       yDim.setConditional(y,kvt);
       if (x != y)
         kvt.throwIllegalValue(xDim,"not equal to YDIM");
      } // else set to default 1
      d_cellSize=x;
    }

    ulXMap.setConditional(d_ulXMap,kvt);
    d_ulYMap = d_nrRows-1;
    ulYMap.setConditional(d_ulYMap,kvt);

  } catch (const com::KeyValueTable::Error& e) {
    // All errors related to contents of .HDR file
    throw com::FileFormatError(hdrPn,e.messages());
  }
}

/*!
 *  \exception com_FileError if write fails
 */
void geo::BandMap::writeHeader()
{
  com::PathName headerPn(headerPathName());
  std::ofstream str;
  com::open(str, headerPn);

  const char* pixelType=0;
  switch(d_cellRepr) {
    case CR_UINT1:
    case CR_UINT2:
    case CR_UINT4: pixelType="UNSIGNEDINT"; break;
    case CR_INT1:
    case CR_INT2:
    case CR_INT4:  pixelType="SIGNEDINT"; break;
    case CR_REAL4:
    case CR_REAL8: pixelType="FLOAT"; break;
    default: DEVELOP_PRECOND(false);
  }

  str << "BYTEORDER      " << d_hostByteOrder << "\n"
      << "LAYOUT       "   << "BIL"           << "\n"
      << "NROWS         "  << d_nrRows        << "\n"
      << "NCOLS         "  << d_nrCols        << "\n"
      << "NBANDS        "  << d_nrBands       << "\n"
      << "NBITS         "  << CELLSIZE(d_cellRepr)*8 << "\n"
      << "BANDROWBYTES         " << d_bandRowBytes   << "\n"
      << "TOTALROWBYTES        " << d_totalRowBytes  << "\n"
      << "BANDGAPBYTES         " << "0"              << "\n"
      << "SKIPBYTES            " << d_skipBytes      << "\n"
      << "ULXMAP        " << std::setprecision(8) << d_ulXMap   << "\n"
      << "ULYMAP        " << std::setprecision(8) << d_ulYMap   << "\n"
      << "XDIM          " << d_cellSize << "\n"
      << "YDIM          " << d_cellSize << "\n";
  if (mvIsSet())
   str<< "NODATA        " << d_mvValue << "\n";
  if (pixelType)
   str<< "PIXELTYPE     " << pixelType << "\n";

  if (!str || str.eof())
    throw com::FileFormatError(headerPn," write error");
}

geo::RasterSpace geo::BandMap::rasterSpace() const
{
  /* projection fixed?
   highest y value
   ^
   |___
   |_|_|
   |_|_|_
   ^ lowest y value
  */
 return RasterSpace(d_nrRows,d_nrCols, d_cellSize,
                    d_ulXMap-0.5*d_cellSize,
                    d_ulYMap+0.5*d_cellSize, YIncrB2T);
}

/*!  read all data as is, from data file
     \exception com::FileError If an error occured while reading the cells.
 */
void geo::BandMap::getCellsRaw(void *buf) const
{
    com::PathName dataPn(dataPathName());
    std::ifstream stream;
    com::open(stream, dataPn, std::ios::binary);

    // skip header
    stream.seekg(d_skipBytes,std::ios::beg);

    if (d_nrBands == 1) {
      stream.read((char*)buf, CSFSIZEOF(nrCells(),d_cellRepr));
    } else {
      // default to 1st band
      // all in byte units
      size_t rowLen(CSFSIZEOF(nrCols(),d_cellRepr));
      size_t skipAfterRow=rowLen*(d_nrBands-1);
      char   *ptr=(char *)buf;
      for(size_t r=0; r < nrRows(); r++) {
       stream.read(ptr, rowLen);
       ptr+=rowLen;
       // skip other bands
       stream.seekg(skipAfterRow,std::ios::cur);
      }
    }

    if (!stream || stream.eof())
      throw com::FileFormatError(dataPn," read error");

    if (!d_hasHostByteOrder)
      switch(LOG_CELLSIZE(d_cellRepr)) {
        case 0 : break;
        case 1 :
          std::for_each((INT2 *)buf,((INT2 *)buf)+nrCells(),com::EndianSwapINT2());
          break;
        case 2 :
          std::for_each((INT4 *)buf,((INT4 *)buf)+nrCells(),com::EndianSwapINT4());
          break;
      }
}

//! read and convert to a PCRaster UINT1 type
/*!
 *  if this file has another missing value then MV_UINT1
 *  all these other missing values are converted to MV_UINT1
 */
void geo::BandMap::getCellsAsUINT1(UINT1 *buf) const
{
  // no casts allowed
  PRECOND(d_cellRepr == CR_UINT1);

  getCellsRaw(buf);

  if (mvIsSet() && com::isUINT1(d_mvValue)) {
    // there is a mv and it is a valid range UINT1
    pcr::AlterToStdMV<UINT1>tsm((UINT1)d_mvValue);
    std::for_each(buf,buf+nrCells(),tsm);
  }
}

void geo::BandMap::getCellsAsINT4(INT4 *buf) const
{
  // only up to broader casts allowed

  if (d_cellRepr == CR_UINT1) {
    getCellsAsUINT1((UINT1 *)buf); // with MV handling on UINT1
    // copy with MV transformation
    com::copyCells((INT4 *)buf,(const UINT1 *)buf,nrCells());
  } else {
    PRECOND(d_cellRepr == CR_INT2);

    INT2 *b2=(INT2 *)buf;
    getCellsRaw(b2);

    if (mvIsSet() && com::isINT2(d_mvValue)) {
      pcr::AlterToStdMV<INT2>tsm((INT2)d_mvValue);
      std::for_each(b2,b2+nrCells(),tsm);
    }
    // copy with MV transformation
    com::copyCells(buf,b2,nrCells());
  }
}

void geo::BandMap::getCellsAsREAL4(REAL4 *buf) const
{
  switch(d_cellRepr) {
   case CR_UINT1:
    getCellsAsUINT1((UINT1 *)buf); // with MV handling on UINT1
    // copy with MV transformation
    com::copyCells((REAL4 *)buf,(const UINT1 *)buf,nrCells());
    break;

   case CR_INT2:
   {
    INT2 *b2=(INT2 *)buf;
    getCellsRaw(b2);

    if (mvIsSet() && com::isINT2(d_mvValue)) {
      pcr::AlterToStdMV<INT2>tsm((INT2)d_mvValue);
      std::for_each(b2,b2+nrCells(),tsm);
    }
    // copy with MV transformation
    com::copyCells(buf,b2,nrCells());
   } break;

   case CR_REAL4:
    getCellsRaw(buf);
    if (mvIsSet()) { // assuming everything is a valid REAL4
      pcr::AlterToStdMV<REAL4>tsm((REAL4)d_mvValue);
      std::for_each(buf,buf+nrCells(),tsm);
    } break;
   default:
    PRECOND(FALSE);
  }

}

/*!
  \param     buf Buffer containing all cell values
  \exception com::FileError if write fails
  \sa        putCells(size_t, size_t, const void*)
*/
void geo::BandMap::putCellsRaw(const void *buf) const
{
  com::PathName dataPn(dataPathName());
  std::ofstream stream;
  com::open(stream, dataPn, std::ios::binary);

  // write empty header
  char filler=0;
  for(size_t i=0; i < d_skipBytes; i++)
    stream.write(&filler, 1);

  stream.write((char*)buf, nrCells()*CELLSIZE(d_cellRepr));

  if (!stream || stream.eof())
    throw com::FileFormatError(dataPn," write error");

  // write .stx file with  min and max
  double minV(-1),maxV(-1);
  switch(cellRepr()) {
    case CR_UINT1: {
          com::GetMinMax<UINT1> gmm((UINT1)mvValue());
          gmm.add((const UINT1 *)buf,nrCells());
          minV=gmm.min();
          maxV=gmm.max();
    } break;
    case CR_INT2: {
          com::GetMinMax<INT2> gmm((INT2)mvValue());
          gmm.add((const INT2 *)buf,nrCells());
          minV=gmm.min();
          maxV=gmm.max();
    } break;
    case CR_REAL4: {
          com::GetMinMax<REAL4> gmm((REAL4)mvValue());
          gmm.add((const REAL4 *)buf,nrCells());
          minV=gmm.min();
          maxV=gmm.max();
    } break;
    default:;
  }

  com::PathName stxPn(stxPathName());
  std::ofstream stx;
  com::open(stx, stxPn);
  stx << "1 " << minV << " " << maxV << "\n";
  if (!stx || stx.eof())
    throw com::FileFormatError(stxPn," write error");
}

/*!
 * \pre cellRepr() == CR_UINT1
 */
void geo::BandMap::putCellsAsUINT1(const UINT1 *buf) const
{
  PRECOND(cellRepr() == CR_UINT1);
  putCellsRaw(buf);
}

/*!
 * \pre cellRepr() == CR_REAL4
 */
void geo::BandMap::putCellsAsREAL4(const REAL4 *buf) const
{
  PRECOND(cellRepr() == CR_REAL4);
  if (d_mvIsSet) {
    geo::SimpleRaster<REAL4> dest(nrRows(),nrCols());
    std::transform(buf,buf+nrCells(),dest.begin(),
                   pcr::FromStdMV<REAL4>(static_cast<REAL4>(d_mvValue)));
    putCellsRaw(dest.cells());
  } else
    putCellsRaw(buf);
}

/*! write a INT4 map as INT2, MV_INT4 is replaced by MV_INT2
 * \pre cellRepr() == CR_INT2
 */
void geo::BandMap::putCellsAsINT4(const INT4 *buf) const
{
  PRECOND(cellRepr() == CR_INT2);
  geo::SimpleRaster<INT2> dest(nrRows(),nrCols());
  com::copyCells(dest.cells(),buf,nrCells());
  putCellsRaw(dest.cells());
}

/*!
  \return    Cell size.
*/
REAL8 geo::BandMap::cellSize() const
{
  return d_cellSize;
}

/*!
  \return    Number of cols.
*/
size_t geo::BandMap::nrCols() const
{
  return d_nrCols;
}

/*!
  \return    Number of rows.
*/
size_t geo::BandMap::nrRows() const
{
  return d_nrRows;
}



/*!
  \return    Number of cells.

  The number of cells is equal to nrCols() * nrRows().
*/
size_t geo::BandMap::nrCells() const
{
  return nrCols() * nrRows();
}

/*!
  \return    In-file cell representation.
*/
CSF_CR geo::BandMap::cellRepr() const
{
  return d_cellRepr;
}

//! the file has a missing value definition
bool geo::BandMap::mvIsSet() const
{
  return d_mvIsSet;
}

/*! return the missing value
 * \pre mvIsSet()
 */
double geo::BandMap::mvValue () const
{
  PRECOND(mvIsSet());
  return d_mvValue;
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

