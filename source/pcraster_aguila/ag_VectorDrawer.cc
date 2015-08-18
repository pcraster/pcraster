#include "ag_VectorDrawer.h"

// External headers.
#include <QPainter>

// Project headers.

// Module headers.
#include "ag_Vector.h"



/*!
  \file
  This file contains the implementation of the VectorDrawer class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VECTORDRAWER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VECTORDRAWER MEMBERS
//------------------------------------------------------------------------------

VectorDrawer::VectorDrawer(
         Vector const* vector,
         dal::SpaceDimensions const& dimensions,
         RangeDrawProps const& properties)

  : RasterDrawer(dimensions, vector),
    _vector(vector),
    _properties(properties)

{
}



VectorDrawer::~VectorDrawer()
{
}



void VectorDrawer::draw (
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  if(!_vector->isRead() || _vector->allMV()) {
    return;
  }

  // Scale magnitude to max one cell size.
  double cellSizeInPixels = this->cellSizeInPixels(xMapper);

  if(cellSizeInPixels < 3) {
    drawCells(painter, indices, xMapper, yMapper);
    return;
  }

  double scale = cellSizeInPixels / _properties.maxCutoff();
  size_t nrCellsPerPixel = this->nrCellsPerPixel(xMapper);

  QVector<QLineF> lines(3);
  double centerX, centerY;
  REAL4 magnitude;
  REAL4 angle;
  QTransform matrix;

  size_t firstRow = static_cast<size_t>(indices.top());
  size_t lastRow = static_cast<size_t>(indices.bottom());
  size_t firstCol = static_cast<size_t>(indices.left());
  size_t lastCol = static_cast<size_t>(indices.right());

  firstRow = firstRow > 1
         ? firstRow - 1
         : firstRow;
  lastRow = lastRow < _vector->dimensions().nrRows() - 1
         ? lastRow + 1
         : lastRow;
  firstCol = firstCol > 1
         ? firstCol - 1
         : firstCol;
  lastCol = lastCol < _vector->dimensions().nrCols() - 1
         ? lastCol + 1
         : lastCol;

  // Default painter settings.
  painter.setRenderHint(QPainter::Antialiasing);
  painter.setPen(Qt::black);

  for(size_t row = firstRow; row <= lastRow; row += nrCellsPerPixel) {
    for(size_t col = firstCol; col <= lastCol; col += nrCellsPerPixel) {
      // TODO Get pointers to the arrays and use linear indices to get at the
      // TODO value. Saves us a cast to REAL4.
      REAL4 const& xMagnitude = _vector->x<REAL4>(row, col);
      REAL4 const& yMagnitude = _vector->y<REAL4>(row, col);

      // Stop in case of missing values or when both magnitudes are zero.
      if(!pcr::isMV(xMagnitude) && !pcr::isMV(yMagnitude) &&
         (!(dal::comparable(xMagnitude, 0.0f) &&
              dal::comparable(yMagnitude, 0.0f)))) {

        // An arrow consists of three lines whose lengths are proportional to
        // the magnitude.
        // - One stem.
        // - Two flags.
        _vector->value<REAL4>(magnitude, row, col);
        lines[0] = QLineF(0,         0,                 0,       magnitude);
        lines[1] = QLineF(0, magnitude, - 0.3 * magnitude, 0.7 * magnitude);
        lines[2] = QLineF(0, magnitude, + 0.3 * magnitude, 0.7 * magnitude);

        // Put origin in the center of the cell.
        matrix.reset();
        _vector->dimensions().coordinates(row + 0.5, col + 0.5,
              centerX, centerY);
        matrix.translate(xMapper.transform(centerX),
              yMapper.transform(centerY));

        // Rotate according to the vector's angle.
        // Since in Qt's coordinate system y-axis points downwards (south) and
        // the maps y-axis points upwards (north), we need to add 180 to the
        // angle.
        _vector->angle(angle, row, col);
        assert(!pcr::isMV(angle));
        matrix.rotate(angle + 180.0);

        // Scale according to the maximum magnitude.
        matrix.scale(scale, scale);

        painter.setWorldTransform(matrix);
        painter.drawLines(lines);
      }
    }
  }
}



template<typename T>
void VectorDrawer::drawCells(
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

  QColor colour(Qt::black);
  colour.setAlpha(100);

  T value;

  painter.setRenderHint(QPainter::Antialiasing, false);
  painter.setPen(Qt::NoPen);

  for(size_t row = firstRow; row <= lastRow; row += nrCellsPerPixel) {
    for(size_t col = firstCol; col <= lastCol; col += nrCellsPerPixel) {
      _vector->value<T>(value, row, col);

      if(!pcr::isMV(value) && !dal::comparable(value, T(0))) {
        _vector->dimensions().coordinates(row, col, leftWorld, topWorld);
        leftScreen = xMapper.transform(leftWorld);
        topScreen = yMapper.transform(topWorld);

        // Determine if the next cells should be drawn in the same colour.
        col += nrCellsPerPixel;

        while(col <= lastCol) {
          _vector->value<T>(value, row, col);

          if(pcr::isMV(value) || dal::comparable(value, T(0))) {
            break;
          }

          col += nrCellsPerPixel;
        }

        col -= nrCellsPerPixel;

        _vector->dimensions().coordinates(row + nrCellsPerPixel,
              col + nrCellsPerPixel, rightWorld, bottomWorld);
        rightScreen = xMapper.transform(rightWorld);
        bottomScreen = yMapper.transform(bottomWorld);

        painter.fillRect(leftScreen, topScreen, rightScreen - leftScreen + 1,
                 bottomScreen - topScreen + 1, colour);
      }
    }
  }
}



void VectorDrawer::drawCells(
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  switch(_vector->typeId()) {
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

