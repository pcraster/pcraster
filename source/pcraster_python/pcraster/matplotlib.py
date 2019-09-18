import os
import inspect

import numpy

from matplotlib import pyplot as plt
from matplotlib.colors import ListedColormap
import matplotlib.patches as mpatches

import pcraster


def _guess_var_name(variable):
  # Make a guess of the name of the provided variable
  # First, check whether a 'global' was used
  callers_local_vars = inspect.currentframe().f_back.f_back.f_locals.items()
  result = [var_name for var_name, var_val in callers_local_vars if var_val is variable]

  if len(result) == 1:
      return result[0]

  # Second, if a class member was used
  for p in callers_local_vars:

          try:
            result =  [k for k, v in p[1].__dict__.items() if v is variable]
            return 'self.{}'.format(result[0])
          except AttributeError as err:
            return ''


def _get_raster(raster):
  # Return masked array and colour scheme based on the raster type
  data = None
  colour = None

  if raster.dataType() == pcraster.Scalar:
    data = pcraster.pcr2numpy(raster, numpy.nan)
    mask = numpy.where(data == numpy.nan, True, False)
    data = numpy.ma.masked_array(data, mask)
    colour = 'gist_rainbow_r'
  elif raster.dataType() == pcraster.Nominal:
    nan_val = -2147483648
    data = pcraster.pcr2numpy(raster, nan_val)
    mask = numpy.where(data == nan_val, True, False)
    data = numpy.ma.masked_array(data, mask)
    colour = 'tab20'
  elif raster.dataType() == pcraster.Boolean:
    nan_val = 255
    data = pcraster.pcr2numpy(raster, nan_val)
    mask = numpy.where(data == nan_val, True, False)
    data= numpy.ma.masked_array(data, mask)
    colours = ['#FF6666', '#66FF66']
    colour = ListedColormap(colours)
  elif raster.dataType() == pcraster.Ldd:
    nan_val = 9
    data = pcraster.pcr2numpy(raster, nan_val)
    # Do not paint raster values, thus white
    colour = ListedColormap(['#FFFFFF'])
  else:
    msg = 'Plotting of rasters with data type "{}" is not supported'.format(str(raster.dataType()).split('.')[1])
    raise NotImplementedError(msg)

  return data, colour


def _test_labels(legends, values):
  # Perform a simple check if provided legend items can be matched to raster values

  if len(legends) != len(values):
    msg = 'Number of items in legend ({}) and raster ({}) are different.'.format(len(legends), len(values))
    raise ValueError(msg)

  for legend in legends:
    if not legend in values:
      msg = 'Legend item value "{}" not found in the raster.'.format(legend)
      raise ValueError(msg)





def plot(raster, labels=None, title=None, filename=None):
  """

  ``raster``: Raster map with type of either Boolean, Nominal, Scalar, or Ldd.

  ``labels``: Optional. Dictionary of labels that should be used for the legend, cell values will be used otherwise.

  ``title``: Optional. Legend title, tries to identify the variable name otherwise.

  ``filename``: Optional. If provided plot will be written to disk.

  Creates a plot of a PCRaster map using matplotlib. The plot is either opened in a separate window, or written to disk in case a filename is provided.

.. versionadded:: 4.3

.. note::
   This function is only available when the ``matplotlib`` module is installed.
  """

  plot_title = None
  plot_labels = ''
  plot_filename = ''

  if not isinstance(raster, pcraster.Field):
    msg = 'First argument must be a PCRaster field, not {}'.format(type(raster))
    raise NotImplementedError(msg)

  # Guess plot title in case no user defined given
  if title is None:
    plot_title = _guess_var_name(raster)
    assert plot_title is not None
  else:
    plot_title = title

  data, colour = _get_raster(raster)
  assert data is not None
  assert colour is not None

  minx = pcraster.clone().west()
  maxy = pcraster.clone().north()
  maxx = minx + pcraster.clone().nrCols() * pcraster.clone().cellSize()
  miny = maxy - pcraster.clone().nrRows() * pcraster.clone().cellSize()

  # Create the figure
  figure, axes = plt.subplots(1)
  # Plot the raster with the coordinates
  im = axes.imshow(data, extent=(minx, maxx, miny, maxy), cmap=colour, interpolation='None', aspect='equal')


  if raster.dataType() == pcraster.Boolean or raster.dataType() == pcraster.Nominal:
    values = numpy.unique(data.compressed().ravel())
    nr_values = len(values)
    colors = [ im.cmap(im.norm(value)) for value in values]

    patches = [ mpatches.Patch(color=colors[idx], label="{l}".format(l=value) ) for idx,value in enumerate(values) ]

    # User provided labels
    if labels is not None:
      _test_labels(labels, values)

      patches = [ mpatches.Patch(color=colors[idx], label="{l}".format(l=labels.get(value)) ) for idx,value in enumerate(values) ]


    # Place those patched as legend-handles into the legend
    legend = plt.legend(handles=patches, bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.0 , title=plot_title)

    bbox_extra_artists=(legend,)

  elif raster.dataType() == pcraster.Scalar:

    # add the legend
    cb = plt.colorbar(im, cmap=colour)
    cb.ax.set_title(plot_title)

  elif raster.dataType() == pcraster.Ldd:

    data = numpy.array([[3,2,1],[6,5,4],[9,8,7]])
    # Ideas obtained from
    # https://stackoverflow.com/questions/33828780/matplotlib-display-array-values-with-imshow

    size_x = 3
    size_y = 3
    x_start = minx
    x_end = maxx
    y_start = maxy
    y_end= miny

    jump_x = (x_end - x_start) / (2.0 * size_x)
    jump_y = (y_end - y_start) / (2.0 * size_y)
    x_positions = numpy.linspace(start=x_start, stop=x_end, num=size_x, endpoint=False)
    y_positions = numpy.linspace(start=y_start, stop=y_end, num=size_y, endpoint=False)

    for y_index, y in enumerate(y_positions):
      for x_index, x in enumerate(x_positions):
        value = data[y_index, x_index]
        x1 = x + jump_x
        y1 = y + jump_y
        x2 = x1
        y2 = y1

        if value == 5:
          plt.plot([x1], [y1], color='black', marker = 'o')
        elif value == 2:
          x2 = x1
          y2 = y1 + 2.0 * jump_y
        elif value == 8:
          x2 = x1
          y2 = y1 - 2.0 * jump_y
        elif value == 4:
          x2 = x1 - 2.0 * jump_x
          y2 = y1
        elif value == 6:
          x2 = x1 + 2.0 * jump_x
          y2 = y1
        elif value == 1:
          x2 = x1 - 2.0 * jump_x
          y2 = y1 + 2.0 * jump_y
        elif value == 7:
          x2 = x1 - 2.0 * jump_x
          y2 = y1 - 2.0 * jump_y
        elif value == 3:
          x2 = x1 + 2.0 * jump_x
          y2 = y1 + 2.0 * jump_y
        elif value == 9:
          x2 = x1 + 2.0 * jump_x
          y2 = y1 - 2.0 * jump_y

        plt.plot([x1, x2], [y1, y2], color='black')

        plot_labels = ('Flow direction' if labels is None else labels)

        patches = [ mpatches.Patch(color='black', label=plot_labels ) ]

        legend = plt.legend(handles=patches, bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0. , title=plot_title)

        bbox_extra_artists=(legend,)




  if filename is None:
    plt.show()
  else:
    plt.savefig(filename, bbox_extra_artists=bbox_extra_artists, bbox_inches='tight', dpi=150)


  plt.close(figure)


