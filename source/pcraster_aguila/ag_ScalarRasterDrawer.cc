#include "ag_ScalarRasterDrawer.h"

// External headers.
#include <QPainter>

// Project headers.

// Module headers.
#include "qt_ColourLib.h"
#include "ag_Raster.h"
#include "com_rawpalette.h"



/*!
  \file
  This file contains the implementation of the ScalarRasterDrawer class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SCALARRASTERDRAWER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SCALARRASTERDRAWER MEMBERS
//------------------------------------------------------------------------------

ScalarRasterDrawer::ScalarRasterDrawer(
         Raster const* raster,
         dal::SpaceDimensions const& dimensions,
         RangeDrawProps const& properties)

  : RasterDrawer(dimensions, raster),
    _raster(raster),
    _properties(properties)

{
}



ScalarRasterDrawer::~ScalarRasterDrawer()
{
}



void ScalarRasterDrawer::drawSingleColour(
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  size_t nrCellsPerPixel = this->nrCellsPerPixel(xMapper);
  double leftScreen, topScreen, rightScreen, bottomScreen;
  double leftWorld, topWorld, rightWorld, bottomWorld;

  dal::Matrix matrix(_raster->dimensions().nrRows(),
         _raster->dimensions().nrCols(), dal::TypeTraits<REAL4>::typeId);
  matrix.transfer(const_cast<REAL4*>(_raster->cells<REAL4>()),
         dal::Matrix::DoNotTakeOwnerShip);

  size_t firstRow = static_cast<size_t>(indices.top());
  size_t lastRow = static_cast<size_t>(indices.bottom());
  size_t firstCol = static_cast<size_t>(indices.left());
  size_t lastCol = static_cast<size_t>(indices.right());

  assert(_properties.palette()->nrColours() > 0);
  QColor colour = qt::RgbTupleToQColor(*_properties.palette()->begin(),
         _properties.palette()->max());

  painter.setRenderHint(QPainter::Antialiasing, false);
  painter.setPen(Qt::NoPen);

  for(size_t row = firstRow; row <= lastRow; row += nrCellsPerPixel) {
    for(size_t col = firstCol; col <= lastCol; col += nrCellsPerPixel) {

      if(!pcr::isMV(matrix.cell<REAL4>(row, col))) {
        _raster->dimensions().coordinates(row, col, leftWorld, topWorld);
        leftScreen = xMapper.transform(leftWorld);
        topScreen = yMapper.transform(topWorld);

        // Determine if the next cells should be drawn in the same colour.
        col += nrCellsPerPixel;

        while(col <= lastCol && !pcr::isMV(matrix.cell<REAL4>(row, col))) {
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



void ScalarRasterDrawer::drawMultipleColours(
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  size_t nrCellsPerPixel = this->nrCellsPerPixel(xMapper);
  double leftScreen, topScreen, rightScreen, bottomScreen;
  double leftWorld, topWorld, rightWorld, bottomWorld;

  dal::Matrix matrix(_raster->dimensions().nrRows(),
         _raster->dimensions().nrCols(), dal::TypeTraits<REAL4>::typeId);
  matrix.transfer(const_cast<REAL4*>(_raster->cells<REAL4>()),
         dal::Matrix::DoNotTakeOwnerShip);

  size_t firstRow = static_cast<size_t>(indices.top());
  size_t lastRow = static_cast<size_t>(indices.bottom());
  size_t firstCol = static_cast<size_t>(indices.left());
  size_t lastCol = static_cast<size_t>(indices.right());

  QColor colour;

  painter.setRenderHint(QPainter::Antialiasing, false);
  painter.setPen(Qt::NoPen);

  for(double row = firstRow; row <= lastRow; row += nrCellsPerPixel) {
    for(double col = firstCol; col <= lastCol; col += nrCellsPerPixel) {

      // Loop over (part of) a row. Determine colour of each subsequent cell.
      // Draw rectangles of cells with equal colour.
      if(!pcr::isMV(matrix.cell<REAL4>(row, col))) {
        colour = _properties.colour(matrix.cell<REAL4>(row, col));

        _raster->dimensions().coordinates(row, col, leftWorld, topWorld);
        leftScreen = xMapper.transform(leftWorld);
        topScreen = yMapper.transform(topWorld);

        col += nrCellsPerPixel;

        while(col <= lastCol && !pcr::isMV(matrix.cell<REAL4>(row, col)) &&
              _properties.colour(matrix.cell<REAL4>(row, col)) == colour) {
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



void ScalarRasterDrawer::drawColourFill(
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  if(_properties.nrClasses() == 0) {
    drawSingleColour(painter, indices, xMapper, yMapper);
  }
  else {
    drawMultipleColours(painter, indices, xMapper, yMapper);
  }
}



void ScalarRasterDrawer::drawContours(
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  //  Method:
  //
  //    +-----+-----+-----+
  //    |     |     |     |
  //    |  8  |  5  |  3  |
  //    |     |     |     |
  //    +-----A-----B-----+
  //    |     |     |     |
  //    |  6  |  9  |  7  |
  //    |     |     |     |
  //    +-----C-----D-----+
  //    |     |     |     |
  //    |     |     |     |
  //    |     |     |     |
  //    +-----+-----+-----+
  //
  //  We draw around centers between cells (like A, B, C, D). So we need 1 less
  //  then the number of columns and rows (decrease 1).
  //  For A we have a mean of 7 ((8 + 5 + 6 + 9) / 4). We draw contours from the
  //  diagonal of the middle to the bottom right of the cell with value 8, to
  //  the horizontal from the middle of 8 to the middle of 5. Etc.

  size_t nrCellsPerPixel = this->nrCellsPerPixel(xMapper);

  dal::Matrix matrix(_raster->dimensions().nrRows(),
         _raster->dimensions().nrCols(), dal::TypeTraits<REAL4>::typeId);
  matrix.transfer(const_cast<REAL4*>(_raster->cells<REAL4>()),
         dal::Matrix::DoNotTakeOwnerShip);

  size_t firstRow = static_cast<size_t>(indices.top());
  size_t lastRow = static_cast<size_t>(indices.bottom());
  size_t firstCol = static_cast<size_t>(indices.left());
  size_t lastCol = static_cast<size_t>(indices.right());

  firstRow = firstRow > 1
         ? firstRow - 2
         : firstRow;
  lastRow = lastRow < _raster->dimensions().nrRows() - 1
         ? lastRow + 1
         : lastRow;
  firstCol = firstCol > 1
         ? firstCol - 2
         : firstCol;
  lastCol = lastCol < _raster->dimensions().nrCols() - 1
         ? lastCol + 1
         : lastCol;

  REAL4  values[4];
  REAL4  mean;
  double cxWld, cyWld;
  double cxPix, cyPix;
  int    nrOfContours  = _properties.nrClasses();
  int    contourNr;
  REAL4  contourValue;
  std::vector<double> contourValues = _properties.classBorders();
  REAL4  contourIntersection;
  double currX = 0.0; // Shut up compiler.
  double currY = 0.0; // Shut up compiler.
  double newX, newY;
  bool   currentSet;
  size_t index, index2;    // Loop index.

  // Size of cell in pixels.
  double cellSize  = cellSizeInPixels(xMapper);
  double halfCellSize = 0.5 * cellSize;

  painter.setRenderHint(QPainter::Antialiasing);

  for(size_t row = firstRow; row <= lastRow - nrCellsPerPixel;
         row += nrCellsPerPixel) {
    for(size_t col = firstCol; col <= lastCol - nrCellsPerPixel;
         col += nrCellsPerPixel) {

      // Get the cellvalues. The values[X] values will be ordered like:
      //   +---+---+
      //   | 0 | 1 |
      //   +---+---+
      //   | 3 | 2 |
      //   +---+---+

      // We only consider windows with four non-mv's.
      if(!pcr::isMV(matrix.cell<REAL4>(row, col)) &&
         !pcr::isMV(matrix.cell<REAL4>(row, col + nrCellsPerPixel)) &&
         !pcr::isMV(matrix.cell<REAL4>(row + nrCellsPerPixel, col + nrCellsPerPixel)) &&
         !pcr::isMV(matrix.cell<REAL4>(row + nrCellsPerPixel, col))) {

        values[0] = matrix.cell<REAL4>(row, col);
        values[1] = matrix.cell<REAL4>(row, col + nrCellsPerPixel);
        values[2] = matrix.cell<REAL4>(row + nrCellsPerPixel, col + nrCellsPerPixel);
        values[3] = matrix.cell<REAL4>(row + nrCellsPerPixel, col);

        // Calculate the pixelcoordinates of the centre.
        _raster->dimensions().coordinates(row + 1, col + 1, cxWld, cyWld);
        cxPix = xMapper.transform(cxWld);
        cyPix = yMapper.transform(cyWld);

        // Calculate the mean of the 4 cells.
        mean = (values[0] + values[1] + values[2] + values[3]) / 4;

        // For each contour to draw.
        for(contourNr = 0; contourNr < nrOfContours; contourNr++) {

          // Determine contour value (== class border).
          contourValue = contourValues[contourNr + 1];
          currentSet = false;

          // Check whether there's a point from which we have to draw a line.
          // Check intersection on line values0 -- values3. In case of an
          // intersection, store the cursor as the current position.
          if((values[0] != values[3]) &&
             ((values[0] <= contourValue && contourValue <= values[3]) ||
              (values[0] >= contourValue && contourValue >= values[3]))) {
            contourIntersection = std::fabs((contourValue - values[0]) /
                 (values[3] - values[0]));
            currX = cxPix - halfCellSize;
            currY = cyPix + ((contourIntersection - 0.5) * cellSize);
            currentSet = true;
          }
          // Check intersection on line mean -- values[3].
          else if((mean != values[3]) &&
               ((mean <= contourValue && contourValue <= values[3]) ||
                (mean >= contourValue && contourValue >= values[3]))) {
            contourIntersection = std::fabs((contourValue - mean) /
               (values[3] - mean));
            currX = cxPix - (halfCellSize * contourIntersection);
            currY = cyPix + (halfCellSize * contourIntersection);
            currentSet = true;
          }

          // Check all four borders and diagonals for intersection.
          for(index = 0; index < 4; index++) {

            // Check intersection on line mean -- values[index].
            if((mean != values[index]) &&
               ((mean <= contourValue && contourValue <= values[index]) ||
                (mean >= contourValue && contourValue >= values[index]))) {
              contourIntersection = std::fabs((contourValue - mean) /
                 (values[index] - mean));

              if(index == 0 || index == 3) {
                newX = cxPix - (halfCellSize * contourIntersection);
              }
              else {
                newX = cxPix + (halfCellSize * contourIntersection);
              }

              if(index == 0 || index == 1) {
                newY = cyPix - (halfCellSize * contourIntersection);
              }
              else {
                newY = cyPix + (halfCellSize * contourIntersection);
              }

              // If a line can be drawn from a previously set point.
              if(currentSet) {
                painter.setPen(_properties.colour(contourNr));
                painter.drawLine(currX, currY, newX, newY);
              }

              currX = newX;
              currY = newY;
              currentSet = true;
            }
            else {
              // If the diagonal isn't intersected, no previous point has to
              // be saved. It is impossible to draw a line from a previous
              // point to a future point without crossing the diagonal we
              // have just checked.
              currentSet = false;
            }

            index2 = (index < 3) ? index + 1: 0;

            // Check intersection on line values[index] -- values[index2]
            if((values[index] != values[index2]) &&
               ((values[index] <= contourValue &&
                 contourValue <= values[index2]) ||
                (values[index] >= contourValue &&
                 contourValue >= values[index2]))) {

              contourIntersection = std::fabs(
                 (contourValue - values[index]) /
                 (values[index2] - values[index]));

              switch(index) {
                case 0: {
                  newX = cxPix + (cellSize * (contourIntersection - 0.5));
                  newY = cyPix - halfCellSize;
                  break;
                }
                case 1: {
                  newX = cxPix + halfCellSize;
                  newY = cyPix + (cellSize * (contourIntersection - 0.5));
                  break;
                }
                case 2: {
                  newX = cxPix - (cellSize * (contourIntersection - 0.5));
                  newY = cyPix + halfCellSize;
                  break;
                }
                case 3:
                default: {
                  newX = cxPix - halfCellSize;
                  newY = cyPix - (cellSize * (contourIntersection - 0.5));
                  break;
                }
              }

              // If a line can be drawn from a previously set point.
              if(currentSet) {
                painter.drawLine(currX, currY, newX, newY);
              }

              currX = newX;
              currY = newY;
              currentSet = true;
            }
          } // End for all border sides and diagonals.
        }
      }
    }
  }
}



void ScalarRasterDrawer::draw(
         QPainter& painter,
         QRect const& indices,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  if(!_raster->isRead() || _raster->allMV()) {
    return;
  }

  switch(_properties.drawerType()) {
    case COLOURFILL: {
      drawColourFill(painter, indices, xMapper, yMapper);
      break;
    }
    case CONTOUR: {
      drawContours(painter, indices, xMapper, yMapper);
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

