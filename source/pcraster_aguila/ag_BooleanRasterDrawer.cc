#include "ag_BooleanRasterDrawer.h"

// External headers.
#include <QPainter>

// Project headers.

// Module headers.
#include "com_rawpalette.h"
#include "ag_Raster.h"



/*!
  \file
  This file contains the implementation of the BooleanRasterDrawer class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BOOLEANRASTERDRAWER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BOOLEANRASTERDRAWER MEMBERS
//------------------------------------------------------------------------------

BooleanRasterDrawer::BooleanRasterDrawer(
         Raster const* raster,
         dal::SpaceDimensions const& dimensions,
         BooleanDrawProps const& properties)

  : RasterDrawer(dimensions, raster),
    _raster(raster),
    _properties(properties)

{
}



BooleanRasterDrawer::~BooleanRasterDrawer()
{
}



void BooleanRasterDrawer::draw(
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  if(!_raster->isRead() || _raster->allMV() ||
         _properties.palette()->nrColours() < 2) {
    return;
  }

  size_t nrCellsPerPixel = this->nrCellsPerPixel(xMapper);
  double leftScreen, topScreen, rightScreen, bottomScreen;
  double leftWorld, topWorld, rightWorld, bottomWorld;

  dal::Matrix matrix(_raster->dimensions().nrRows(),
         _raster->dimensions().nrCols(), dal::TypeTraits<UINT1>::typeId);
  matrix.transfer(const_cast<UINT1*>(_raster->cells<UINT1>()),
         dal::Matrix::DoNotTakeOwnerShip);
  UINT1 value;

  size_t firstRow = static_cast<size_t>(indices.top());
  size_t lastRow = static_cast<size_t>(indices.bottom());
  size_t firstCol = static_cast<size_t>(indices.left());
  size_t lastCol = static_cast<size_t>(indices.right());

  QColor trueColour = _properties.colourByIndex(1);
  QColor falseColour = _properties.colourByIndex(0);

  painter.setRenderHint(QPainter::Antialiasing, false);
  painter.setPen(Qt::NoPen);

  for(size_t row = firstRow; row <= lastRow; row += nrCellsPerPixel) {
    for(size_t col = firstCol; col <= lastCol; col += nrCellsPerPixel) {

      if(!pcr::isMV(matrix.cell<UINT1>(row, col))) {
        value = matrix.cell<UINT1>(row, col);

        _raster->dimensions().coordinates(row, col, leftWorld, topWorld);
        leftScreen = xMapper.transform(leftWorld);
        topScreen = yMapper.transform(topWorld);

        // Determine if the next cells should be drawn in the same colour.
        col += nrCellsPerPixel;

        while(col <= lastCol && !pcr::isMV(matrix.cell<UINT1>(row, col)) &&
              matrix.cell<UINT1>(row, col) == value) {
          col += nrCellsPerPixel;
        }

        col -= nrCellsPerPixel;

        _raster->dimensions().coordinates(row + nrCellsPerPixel,
              col + nrCellsPerPixel, rightWorld, bottomWorld);
        rightScreen = xMapper.transform(rightWorld);
        bottomScreen = yMapper.transform(bottomWorld);

        painter.fillRect(leftScreen, topScreen, rightScreen - leftScreen + 1,
              bottomScreen - topScreen + 1, value ? trueColour : falseColour);
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

