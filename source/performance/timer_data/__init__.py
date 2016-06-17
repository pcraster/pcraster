import os
import numpy
from osgeo import gdal
from osgeo.gdalconst import *


nr_rows = 6000
nr_cols = 80000

string_by_dtype = {
    numpy.float32: "float32"
}


extension_by_format = {
    "pcraster": "map"
}


def dataset_pathname(
        data_prefix,
        format_,
        dtype,
        kind):

    return os.path.join(data_prefix, "{}_{}.{}".format(
        string_by_dtype[dtype], kind, extension_by_format[format_]))


class TimerData(object):

    nr_rows = 6000
    nr_cols = 80000
    data_prefix = os.getcwd()


    @classmethod
    def dataset_pathname(cls,
            format_,
            dtype,
            kind):
        return dataset_pathname(cls.data_prefix, format_, dtype, kind)


def write_array_to_raster(
        array,
        pathname):
    nr_rows, nr_cols = array.shape

    assert array.dtype == numpy.float32

    driver = gdal.GetDriverByName("PCRaster")
    dataset = driver.Create(pathname, nr_cols, nr_rows, 1, gdal.GDT_Float32,
        ["PCRASTER_VALUESCALE=VS_SCALAR"])
    dataset.GetRasterBand(1).WriteArray(array)


def generate_random_raster(
        data_prefix,
        format_,
        dtype,
        kind):

    pathname = dataset_pathname(data_prefix, format_, dtype, kind)

    if not os.path.exists(pathname):
        # - Generate a 2D numpy array with random values.
        # - Pass the array to GDAL and create a PCRaster raster from it.
        array = numpy.random.rand(nr_rows, nr_cols).astype(dtype)
        write_array_to_raster(array, pathname)


def create_datasets(
        data_prefix):

    assert os.path.exists(data_prefix), data_prefix

    format_ = "pcraster"
    dtype = numpy.float32

    generate_random_raster(data_prefix, format_, dtype, "random_1")
    generate_random_raster(data_prefix, format_, dtype, "random_2")
