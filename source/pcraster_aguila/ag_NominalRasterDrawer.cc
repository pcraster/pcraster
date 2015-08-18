#include "ag_NominalRasterDrawer.h"

// External headers.
#include <QPainter>

// Project headers.

// Module headers.
#include "ag_Raster.h"



/*!
  \file
  This file contains the implementation of the NominalRasterDrawer class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC NOMINALRASTERDRAWER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF NOMINALRASTERDRAWER MEMBERS
//------------------------------------------------------------------------------

NominalRasterDrawer::NominalRasterDrawer(
         Raster const* raster,
         dal::SpaceDimensions const& dimensions,
         NominalDrawProps const& properties)

  : RasterDrawer(dimensions, raster),
    _raster(raster),
    _properties(properties)

{
}



NominalRasterDrawer::~NominalRasterDrawer()
{
}



void NominalRasterDrawer::draw(
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  if(!_raster->isRead() || _raster->allMV()) {
    return;
  }

  size_t nrCellsPerPixel = this->nrCellsPerPixel(xMapper);
  double leftScreen, topScreen, rightScreen, bottomScreen;
  double leftWorld, topWorld, rightWorld, bottomWorld;

  dal::Matrix matrix(_raster->dimensions().nrRows(),
         _raster->dimensions().nrCols(), dal::TypeTraits<INT4>::typeId);
  matrix.transfer(const_cast<INT4*>(_raster->cells<INT4>()),
         dal::Matrix::DoNotTakeOwnerShip);
  INT4 value;

  size_t firstRow = static_cast<size_t>(indices.top());
  size_t lastRow = static_cast<size_t>(indices.bottom());
  size_t firstCol = static_cast<size_t>(indices.left());
  size_t lastCol = static_cast<size_t>(indices.right());

  QColor colour;

  painter.setRenderHint(QPainter::Antialiasing, false);
  painter.setPen(Qt::NoPen);

  for(size_t row = firstRow; row <= lastRow; row += nrCellsPerPixel) {
    for(size_t col = firstCol; col <= lastCol; col += nrCellsPerPixel) {
      if(!pcr::isMV(matrix.cell<INT4>(row, col))) {
        value = matrix.cell<INT4>(row, col);
        colour = _properties.colourByIndex(
              _properties.classifier().index(value));

        _raster->dimensions().coordinates(row, col, leftWorld, topWorld);
        leftScreen = xMapper.transform(leftWorld);
        topScreen = yMapper.transform(topWorld);

        // Determine if the next cells should be drawn in the same colour.
        col += nrCellsPerPixel;

        while(col <= lastCol && !pcr::isMV(matrix.cell<INT4>(row, col)) &&
              _properties.colourByIndex(_properties.classifier().index(
                   matrix.cell<INT4>(row, col))) == colour) {
          col += nrCellsPerPixel;
        }

        col -= nrCellsPerPixel;

        _raster->dimensions().coordinates(row + nrCellsPerPixel,
              col + nrCellsPerPixel, rightWorld, bottomWorld);
        rightScreen = xMapper.transform(rightWorld);
        bottomScreen = yMapper.transform(bottomWorld);

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

