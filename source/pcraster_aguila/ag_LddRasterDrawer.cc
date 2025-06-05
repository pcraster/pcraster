#include "ag_LddRasterDrawer.h"

// External headers.

// Project headers.

// Module headers.
#include "ag_Raster.h"

#include <QPainter>

#include <cmath>

/*!
  \file
  This file contains the implementation of the LddRasterDrawer class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC LDDRASTERDRAWER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LDDRASTERDRAWER MEMBERS
//------------------------------------------------------------------------------

LddRasterDrawer::LddRasterDrawer(
         Raster const* raster,
         dal::SpaceDimensions const& dimensions,
         LddDrawProps const& properties,
         QColor const& penColour)

  : RasterDrawer(dimensions, raster),
    _raster(raster),
    _properties(properties),
    _penColour(penColour)

{
}



LddRasterDrawer::~LddRasterDrawer()
{
}



void LddRasterDrawer::draw(
         QPainter& painter,
         QRect const& indices,
         QTransform const& world_to_screen,
         QTransform const& screen_to_world) const
{
  if(!_raster->isRead() || _raster->allMV()) {
    return;
  }

  if(this->cellSizeInPixels(world_to_screen) < 3) {
    drawCells(painter, indices, world_to_screen, screen_to_world);
    return;
  }

  size_t nrCellsPerPixel = this->nrCellsPerPixel(world_to_screen);

  dal::Matrix matrix(_raster->dimensions().nrRows(),
         _raster->dimensions().nrCols(), dal::TypeTraits<UINT1>::typeId);
  matrix.transfer(const_cast<UINT1*>(_raster->cells<UINT1>()),
         dal::Matrix::DoNotTakeOwnerShip);

  auto firstRow = static_cast<size_t>(indices.top());
  auto lastRow = static_cast<size_t>(indices.bottom());
  auto firstCol = static_cast<size_t>(indices.left());
  auto lastCol = static_cast<size_t>(indices.right());

  firstRow = firstRow > 1
         ? firstRow - 1
         : firstRow;
  lastRow = lastRow < _raster->dimensions().nrRows() - 1
         ? lastRow + 1
         : lastRow;
  firstCol = firstCol > 1
         ? firstCol - 1
         : firstCol;
  lastCol = lastCol < _raster->dimensions().nrCols() - 1
         ? lastCol + 1
         : lastCol;

  UINT1 value = 0;

  double cxWld = NAN;
  double cyWld = NAN;           // Center of cell.
  double ncxWld = NAN;
  double ncyWld = NAN;         // Center of neighb. cell (in ldd direction).
  double leftPitWld = NAN;
  double topPitWld = NAN;  // Upper left of pit.
  double cxPix = NAN;
  double cyPix = NAN;           // Center of cell.
  double ncxPix = NAN;
  double ncyPix = NAN;         // Center of neighb. cell.
  double leftPitPix = NAN;
  double topPitPix = NAN;  // Upper left of pit.
  double pitSizeX = NAN;
  double pitSizeY = NAN;     // Size of pit.

  pitSizeX = std::abs(0.5 * _raster->dimensions().cellSize() *
         this->scale(world_to_screen));
  pitSizeX = std::max(1.0, pitSizeX);
  pitSizeY = std::abs(0.5 * _raster->dimensions().cellSize() *
         this->scale(world_to_screen));
  pitSizeY = std::max(1.0, pitSizeY);

  painter.setRenderHint(QPainter::Antialiasing);
  painter.setPen(_penColour);
  painter.setBrush(_penColour);

  for(size_t row = firstRow; row <= lastRow; row += nrCellsPerPixel) {
    for(size_t col = firstCol; col <= lastCol; col += nrCellsPerPixel) {
      if(!pcr::isMV(matrix.cell<UINT1>(row, col))) {
        value = matrix.cell<UINT1>(row, col);

        if(value != 5) {

          // Calculate the pixelcoordinates of the middle of the cell.
          _raster->dimensions().coordinates(row + 0.5, col + 0.5, cxWld, cyWld);

          QPointF p = QPointF(cxWld, cyWld);
          cxPix = world_to_screen.map(p).x();
          cyPix = world_to_screen.map(p).y();

          if(value == 1) {
            // Next cell to the lowerleft.
            // Calc coordinates of that cell.
            _raster->dimensions().coordinates(row + 1 + 0.5, col - 1 + 0.5,
              ncxWld, ncyWld);
          }
          else if(value == 2) {
            // Next cell to bottom.
            // Calc coordinates of that cell.
            _raster->dimensions().coordinates(row + 1 + 0.5, col + 0.5,
              ncxWld, ncyWld);
          }
          else if(value == 3) {
            // Next cell to lowerright.
            // Calc coordinates of that cell.
            _raster->dimensions().coordinates(row + 1 + 0.5, col + 1 + 0.5,
              ncxWld, ncyWld);
          }
          else if(value == 4) {
            // Next cell to left.
            // Calc coordinates of that cell.
            _raster->dimensions().coordinates(row + 0.5, col - 1 + 0.5,
              ncxWld, ncyWld);
          }
          else if(value == 6) {
            // Next cell to right.
            // Calc coordinates of that cell.
            _raster->dimensions().coordinates(row + 0.5, col + 1 + 0.5,
              ncxWld, ncyWld);
          }
          else if(value == 7) {
            // Next cell to upperleft.
            // Calc coordinates of that cell.
            _raster->dimensions().coordinates(row - 1 + 0.5, col - 1 + 0.5,
              ncxWld, ncyWld);
          }
          else if(value == 8) {
            // Next cell to top.
            // Calc coordinates of that cell.
            _raster->dimensions().coordinates(row - 1 + 0.5, col + 0.5,
              ncxWld, ncyWld);
          }
          else if(value == 9) {
            // Next cell to upperright.
            // Calc coordinates of that cell.
            _raster->dimensions().coordinates(row - 1 + 0.5, col + 1 + 0.5,
              ncxWld, ncyWld);
          }

          p = QPointF(ncxWld, ncyWld);
          ncxPix = world_to_screen.map(p).x();
          ncyPix = world_to_screen.map(p).y();

          painter.drawLine(cxPix, cyPix, ncxPix, ncyPix);
        }
        else  { // *cellHandle == 5
          // Draw pit.
          // Calc coordinates of upperleft of pit-symbol.
          // painter.setPen(Qt::NoPen);
          _raster->dimensions().coordinates(row + 0.25, col + 0.25,
              leftPitWld, topPitWld);

          QPointF p = QPointF(leftPitWld, topPitWld);
          leftPitPix = world_to_screen.map(p).x();
          topPitPix = world_to_screen.map(p).y();

          painter.drawRect(leftPitPix, topPitPix, pitSizeX, pitSizeY);
        }
      }
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

