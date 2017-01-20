#!/usr/bin/env python
import argparse
import sys
import PIL.Image
import PIL.ImageDraw
import numpy
import osgeo.gdal
import osgeo.gdal_array



def _drawGrid(
  array,
  cellWidth,
  image,
  draw):
  colour = "gray"
  for r in range(array.shape[0] + 1):
    y = cellWidth * r
    draw.line((0, y, image.size[0], y), colour)

  for r in range(array.shape[1] + 1):
    x = cellWidth * r
    draw.line((x, 0, x, image.size[1]), colour)



def _drawLdd(
  array,
  cellWidth,
  image,
  draw):
  colour = "blue"

  # size of the pit box = boxWidth * 2
  boxWidth = cellWidth / 10.0

  # ldd code, change in row, col
  lddDir = {
    1: (-1,1),
    2: (0,1),
    3: (1,1),
    4: (-1,0),
    6: (1,0),
    7: (-1,-1),
    8: (0,-1),
    9: (1,-1)
  }

  for r in range(array.shape[0]):
    yCoord = (cellWidth) / 2.0 + r * cellWidth

    for c in range(array.shape[1]):
      res = array[r][c]

      if not numpy.isnan(res):
        xCoord = (cellWidth * c) + (cellWidth / 2.0)

        if res == 5:
          # Draw a pit.
          box = (xCoord - boxWidth, yCoord + boxWidth, xCoord + boxWidth,
            yCoord - boxWidth)
          draw.rectangle(box, fill=colour)
        else:
          # Draw a line.
          directions = lddDir[res]
          x1 = xCoord
          y1 = yCoord
          x2 = xCoord + directions[0] * cellWidth
          y2 = yCoord + directions[1] * cellWidth
          draw.line((x1, y1, x2, y2), colour)



def _drawMap(
  dataset,
  array,
  cellWidth,
  image,
  draw):
  # Assume that default font has size 10? Used for y pos calculation in cell.
  fontSize = 10

  for ro in range(array.shape[0]):
    yCoord = (cellWidth - fontSize) / 2.0 + ro * cellWidth
    for c in range(array.shape[1]):
      res = array[ro][c]

      if numpy.isnan(res):
        res = "MV"
      else:
        res = "%0.3G" % (res)

      strLen = draw.textsize(res)
      xCoord = cellWidth * c + (cellWidth - strLen[0]) / 2.0
      draw.text((xCoord, yCoord), res, fill=1)



def rasterToPNG(
  rasterFileName,
  imageFileName):

  # Read values and replace missing values by NaN's.
  # TODO Use numpy 1.7's built in support for NA masked arrays.
  dataset = osgeo.gdal.Open(rasterFileName)
  assert not dataset is None
  band = dataset.GetRasterBand(1)
  assert band
  array = osgeo.gdal_array.BandReadAsArray(band).astype("float")
  array[array == band.GetNoDataValue()] = numpy.nan

  # Default width/height of a cell. Should be enough for %0.3G format with
  # default font.
  cellWidth = 55

  imageWidth = dataset.RasterXSize * cellWidth + 1
  imageHeight = dataset.RasterYSize * cellWidth + 1

  image = PIL.Image.new("RGB",(imageWidth, imageHeight), "white")
  draw = PIL.ImageDraw.Draw(image)

  _drawGrid(array, cellWidth, image, draw)

  metadata = dataset.GetMetadata()
  if "PCRASTER_VALUESCALE" in metadata and \
      metadata["PCRASTER_VALUESCALE"] == "VS_LDD":
    _drawLdd(array, cellWidth, image, draw)
  else:
    _drawMap(dataset, array, cellWidth, image, draw)

  image.save(imageFileName, "PNG", optimize=1)



if __name__ == "__main__":
  parser = argparse.ArgumentParser(
    description="Create a PNG image from a raster")
  parser.add_argument("raster", type=str, help="Raster to convert")
  parser.add_argument("image", type=str, help="PNG image to write")
  args = parser.parse_args()

  rasterToPNG(args.raster, args.image)
  sys.exit(0)

