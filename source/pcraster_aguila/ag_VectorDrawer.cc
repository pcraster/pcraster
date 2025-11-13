#include "ag_VectorDrawer.h"

// External headers.

// Project headers.

// Module headers.
#include "ag_Vector.h"

#include <QPainter>
#include <cmath>

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
         QTransform const& world_to_screen,
         QTransform const& screen_to_world) const
{
  if(!_vector->isRead() || _vector->allMV()) {
    return;
  }

  // Scale magnitude to max one cell size.
  double const cellSizeInPixels = this->cellSizeInPixels(screen_to_world);

  if(cellSizeInPixels < 3) {
    drawCells(painter, indices, world_to_screen, screen_to_world);
    return;
  }

  double const scale = cellSizeInPixels / _properties.maxCutoff();
  size_t const nrCellsPerPixel = this->nrCellsPerPixel(world_to_screen);

  QVector<QLineF> lines(3);
  double centerX = NAN;
  double centerY = NAN;
  REAL4 magnitude = NAN;
  REAL4 angle = NAN;
  QTransform matrix;

  auto firstRow = static_cast<size_t>(indices.top());
  auto lastRow = static_cast<size_t>(indices.bottom());
  auto firstCol = static_cast<size_t>(indices.left());
  auto lastCol = static_cast<size_t>(indices.right());

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
      auto const& xMagnitude = _vector->x<REAL4>(row, col);
      auto const& yMagnitude = _vector->y<REAL4>(row, col);

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

        QPointF const p = QPointF(centerX, centerY);
        matrix.translate(world_to_screen.map(p).x(), world_to_screen.map(p).y());

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
         QTransform const& world_to_screen,
         QTransform const&  /*screen_to_world*/) const
{
  size_t const nrCellsPerPixel = this->nrCellsPerPixel(world_to_screen);
  double leftScreen = NAN;
  double topScreen = NAN;
  double rightScreen = NAN;
  double bottomScreen = NAN;
  double leftWorld = NAN;
  double topWorld = NAN;
  double rightWorld = NAN;
  double bottomWorld = NAN;

  auto firstRow = static_cast<size_t>(indices.top());
  auto lastRow = static_cast<size_t>(indices.bottom());
  auto firstCol = static_cast<size_t>(indices.left());
  auto lastCol = static_cast<size_t>(indices.right());

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

        QPointF p = QPointF(leftWorld, topWorld);
        leftScreen = world_to_screen.map(p).x();
        topScreen = world_to_screen.map(p).y();

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

        p = QPointF(rightWorld, bottomWorld);
        rightScreen = world_to_screen.map(p).x();
        bottomScreen = world_to_screen.map(p).y();

        painter.fillRect(leftScreen, topScreen, rightScreen - leftScreen + 1,
                 bottomScreen - topScreen + 1, colour);
      }
    }
  }
}



void VectorDrawer::drawCells(
         QPainter& painter,
         QRect const& indices,
         QTransform const& world_to_screen,
         QTransform const& screen_to_world) const
{
  switch(_vector->typeId()) {
    case dal::TI_REAL4: {
      drawCells<REAL4>(painter, indices, world_to_screen, screen_to_world);
      break;
    }
    case dal::TI_REAL8: {
      drawCells<REAL8>(painter, indices, world_to_screen, screen_to_world);
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

