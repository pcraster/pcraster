#include "ag_RasterDrawer.h"

// External headers.
#include <cassert>
#include <cmath>
#include <QPainter>

// Project headers.

// Module headers.
#include "ag_Raster.h"



/*!
  \file
  This file contains the implementation of the RasterDrawer class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERDRAWER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTERDRAWER MEMBERS
//------------------------------------------------------------------------------

//!
/*!
  \tparam    .
  \param     spaceDimensions The envelope around all spatial attributes.
  \param     rasterDimensions The raster properties of the attribute currently
             drawn.
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
RasterDrawer::RasterDrawer(
         dal::SpaceDimensions const& spaceDimensions,
         RasterDataset const* raster)

  : MapDrawer(spaceDimensions, raster->dimensions()),
    _raster(raster)

{
}



RasterDrawer::~RasterDrawer()
{
}



// RasterDataset const& RasterDrawer::raster() const
// {
//   return *_raster;
// }



double RasterDrawer::cellSizeInPixels(
         QwtScaleMap const& mapper) const
{
  return scale(mapper) * _raster->dimensions().cellSize();
}



size_t RasterDrawer::nrCellsPerPixel(
         QwtScaleMap const& mapper) const
{
  double nrCellsPerPixel = 1.0 / cellSizeInPixels(mapper);

  // Return a minimum of 1 cell per pixel, but possibly more. Err on the safe
  // side.
  return std::max(size_t(1), static_cast<size_t>(std::floor(nrCellsPerPixel)));
}



void RasterDrawer::draw(
         QPainter& painter,
         QRectF const& dirtyMapAreaInPixels,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  // painter.setPen(Qt::black);
  // painter.setBrush(Qt::blue);
  // painter.drawRect(dirtyMapAreaInPixels);
  // return;

  // Determine which cells fall in the dirtyMapAreaInPixels.
  assert(!dirtyMapAreaInPixels.isEmpty());
  assert(dirtyMapAreaInPixels.left() >= 0);
  assert(dirtyMapAreaInPixels.top() >= 0);

  // Convert dirtyMapAreaInPixels to dirtyMapAreaInWorldCoordinates.
  QRectF dirtyMapAreaInWorldCoordinates(
    QPointF(
         xMapper.invTransform(dirtyMapAreaInPixels.left()),
         yMapper.invTransform(dirtyMapAreaInPixels.top())),
    QPointF(
         xMapper.invTransform(dirtyMapAreaInPixels.right()),
         yMapper.invTransform(dirtyMapAreaInPixels.bottom())));
  // This rectangle is invalid in Qt's sense because of the y-coordinate
  // projection.
  // assert(!dirtyMapAreaInWorldCoordinates.isEmpty());

  // Determine rasterArea (rows/columns) within worldArea.
  // The resulting rectangle can be invalid: the column of the east border
  // coordinate of the raster is nrCols(), for example.
  QPointF northWestCellIndices, southEastCellIndices;
  _raster->dimensions().indices(
         dirtyMapAreaInWorldCoordinates.left(),
         dirtyMapAreaInWorldCoordinates.top(),
         northWestCellIndices.ry(), northWestCellIndices.rx());
  _raster->dimensions().indices(
         dirtyMapAreaInWorldCoordinates.right(),
         dirtyMapAreaInWorldCoordinates.bottom(),
         southEastCellIndices.ry(), southEastCellIndices.rx());
  // Add a cell to the dirtyMapAreaInCells to account for those drawers that
  // draw in the neighbouring cell.
  QRectF dirtyMapAreaInCells(northWestCellIndices - QPointF(1.0, 1.0),
         southEastCellIndices + QPointF(1.0, 1.0));

  assert(!dirtyMapAreaInCells.isEmpty());

  int firstRow = std::max(std::floor(dirtyMapAreaInCells.top()), 0.0);
  int lastRow = std::min(std::floor(dirtyMapAreaInCells.bottom()),
         _raster->dimensions().nrRows() - 1.0);
  int firstCol = std::max(std::floor(dirtyMapAreaInCells.left()), 0.0);
  int lastCol = std::min(std::floor(dirtyMapAreaInCells.right()),
         _raster->dimensions().nrCols() - 1.0);

  QRect indices(QPoint(firstCol, firstRow), QPoint(lastCol, lastRow));

  if(indices.isEmpty()) {
    return;
  }

  assert(indices.left() >= 0);
  assert(indices.right() < static_cast<int>(_raster->dimensions().nrCols()));
  assert(indices.top() >= 0);
  assert(indices.bottom() < static_cast<int>(_raster->dimensions().nrRows()));

  draw(painter, indices, xMapper, yMapper);
}



void RasterDrawer::draw2(
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  size_t nrCellsPerPixel = this->nrCellsPerPixel(xMapper);
  double leftScreen, topScreen, rightScreen, bottomScreen;
  double leftWorld, topWorld, rightWorld, bottomWorld;

  size_t firstRow = static_cast<size_t>(indices.top());
  size_t lastRow = static_cast<size_t>(indices.bottom());
  size_t firstCol = static_cast<size_t>(indices.left());
  size_t lastCol = static_cast<size_t>(indices.right());

  painter.setRenderHint(QPainter::Antialiasing, false);
  painter.setPen(Qt::black);
  painter.setBrush(Qt::red);

  for(size_t row = firstRow; row <= lastRow; row += nrCellsPerPixel) {
    for(size_t col = firstCol; col <= lastCol; col += nrCellsPerPixel) {

        _raster->dimensions().coordinates(row, col, leftWorld, topWorld);
        leftScreen = xMapper.invTransform(leftWorld);
        topScreen = yMapper.invTransform(topWorld);

        // Determine if the next cells should be drawn in the same colour.
        col += nrCellsPerPixel;

        while(col <= lastCol) {
          col += nrCellsPerPixel;
        }

        col -= nrCellsPerPixel;

        _raster->dimensions().coordinates(row + nrCellsPerPixel,
              col + nrCellsPerPixel, rightWorld, bottomWorld);
        rightScreen = xMapper.invTransform(rightWorld);
        bottomScreen = yMapper.invTransform(bottomWorld);

        painter.drawRect(leftScreen, topScreen, rightScreen - leftScreen + 1,
                 bottomScreen - topScreen + 1);
    }
  }
}



template<typename T>
void RasterDrawer::drawCells(
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  size_t nrCellsPerPixel = this->nrCellsPerPixel(xMapper);
  double leftScreen, topScreen, rightScreen, bottomScreen;
  double leftWorld, topWorld, rightWorld, bottomWorld;

  // dal::Matrix matrix(_raster->dimensions().nrRows(),
  //        _raster->dimensions().nrCols(), dal::TypeTraits<T>::typeId);
  // matrix.transfer(const_cast<T*>(_raster->cells<T>()),
  //        dal::Matrix::DoNotTakeOwnerShip);

  size_t firstRow = static_cast<size_t>(indices.top());
  size_t lastRow = static_cast<size_t>(indices.bottom());
  size_t firstCol = static_cast<size_t>(indices.left());
  size_t lastCol = static_cast<size_t>(indices.right());

  QColor colour(Qt::black);
  // colour.setAlpha(150);  No, results in weird striping pattern.

  painter.setRenderHint(QPainter::Antialiasing, false);
  painter.setPen(Qt::NoPen);

  for(size_t row = firstRow; row <= lastRow; row += nrCellsPerPixel) {
    for(size_t col = firstCol; col <= lastCol; col += nrCellsPerPixel) {
      if(!_raster->isMV(row, col)) {
        _raster->dimensions().coordinates(row, col, leftWorld, topWorld);
        leftScreen = xMapper.transform(leftWorld);
        topScreen = yMapper.transform(topWorld);

        // Determine if the next cells should be drawn in the same colour.
        col += nrCellsPerPixel;

        while(col <= lastCol && !_raster->isMV(row, col)) {
          col += nrCellsPerPixel;
        }

        col -= nrCellsPerPixel;

        _raster->dimensions().coordinates(row + nrCellsPerPixel,
              col + nrCellsPerPixel, rightWorld, bottomWorld);
        rightScreen = xMapper.transform(rightWorld);
        bottomScreen = yMapper.transform(bottomWorld);

        painter.fillRect(leftScreen, topScreen, (rightScreen - leftScreen) + 1,
              (bottomScreen - topScreen) + 1, colour);
      }
    }
  }
}



void RasterDrawer::drawCells(
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  switch(_raster->typeId()) {
    case dal::TI_INT1: {
      drawCells<INT1>(painter, indices, xMapper, yMapper);
      break;
    }
    case dal::TI_INT2: {
      drawCells<INT2>(painter, indices, xMapper, yMapper);
      break;
    }
    case dal::TI_INT4: {
      drawCells<INT4>(painter, indices, xMapper, yMapper);
      break;
    }
    case dal::TI_UINT1: {
      drawCells<UINT1>(painter, indices, xMapper, yMapper);
      break;
    }
    case dal::TI_UINT2: {
      drawCells<UINT2>(painter, indices, xMapper, yMapper);
      break;
    }
    case dal::TI_UINT4: {
      drawCells<UINT4>(painter, indices, xMapper, yMapper);
      break;
    }
    case dal::TI_REAL4: {
      drawCells<REAL4>(painter, indices, xMapper, yMapper);
      break;
    }
    case dal::TI_REAL8: {
      drawCells<REAL8>(painter, indices, xMapper, yMapper);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

