#include "ag_DirectionalRasterDrawer.h"

// External headers.
#include <QPainter>

// Project headers.

// Module headers.
#include "ag_Raster.h"



/*!
  \file
  This file contains the implementation of the DirectionalRasterDrawer class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DIRECTIONALRASTERDRAWER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DIRECTIONALRASTERDRAWER MEMBERS
//------------------------------------------------------------------------------

DirectionalRasterDrawer::DirectionalRasterDrawer(
         Raster const* raster,
         dal::SpaceDimensions const& dimensions,
         RangeDrawProps const& properties)

  : RasterDrawer(dimensions, raster),
    _raster(raster),
    _properties(properties)

{
}



DirectionalRasterDrawer::~DirectionalRasterDrawer()
{
}



void DirectionalRasterDrawer::draw(
         QPainter& painter,
         QRect const& indices,
         QTransform const& world_to_screen,
         QTransform const&  /*screen_to_world*/) const
{
  if(!_raster->isRead() || _raster->allMV() || _properties.nrClasses() == 0) {
    return;
  }

  size_t nrCellsPerPixel = this->nrCellsPerPixel(world_to_screen);
  double leftScreen, topScreen, rightScreen, bottomScreen;
  double leftWorld, topWorld, rightWorld, bottomWorld;

  dal::Matrix matrix(_raster->dimensions().nrRows(),
         _raster->dimensions().nrCols(), dal::TypeTraits<REAL4>::typeId);
  matrix.transfer(const_cast<REAL4*>(_raster->cells<REAL4>()),
         dal::Matrix::DoNotTakeOwnerShip);

  auto firstRow = static_cast<size_t>(indices.top());
  auto lastRow = static_cast<size_t>(indices.bottom());
  auto firstCol = static_cast<size_t>(indices.left());
  auto lastCol = static_cast<size_t>(indices.right());

  REAL4 value;
  QColor colour;

  painter.setRenderHint(QPainter::Antialiasing, false);
  painter.setPen(Qt::NoPen);

  for(size_t row = firstRow; row <= lastRow; row += nrCellsPerPixel) {
    for(size_t col = firstCol; col <= lastCol; col += nrCellsPerPixel) {
      if(!pcr::isMV(matrix.cell<REAL4>(row, col))) {
        value = matrix.cell<REAL4>(row, col);

        if(dal::comparable(value, REAL4(-1.0))) {
          colour = QColor(255, 0, 0);
        }
        else {
          colour = _properties.colour(value);
        }

        _raster->dimensions().coordinates(row, col, leftWorld, topWorld);

        QPointF p = QPointF(leftWorld, topWorld);
        leftScreen = world_to_screen.map(p).x();
        topScreen = world_to_screen.map(p).y();

        if(!dal::comparable(value, REAL4(-1.0))) {
          col += nrCellsPerPixel;

          while(col <= lastCol && !pcr::isMV(matrix.cell<REAL4>(row, col)) &&
                _properties.colour(matrix.cell<REAL4>(row, col)) == colour) {
            col += nrCellsPerPixel;
          }

          col -= nrCellsPerPixel;
        }

        _raster->dimensions().coordinates(row + nrCellsPerPixel,
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



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

