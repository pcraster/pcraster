import os
# import shlex
# import subprocess
import numpy
from osgeo import gdal
from osgeo.gdalconst import *
import pcraster


nr_rows = 6000
nr_cols = 80000

extension_by_format = {
    "pcraster": "map"
}

string_by_type = {
    numpy.bool8: "bool8",
    numpy.int32: "int32",
    numpy.float32: "float32"
}

gdal_type_by_dtype = {
    numpy.dtype(numpy.bool8): gdal.GDT_Byte,
    numpy.dtype(numpy.int32): gdal.GDT_Int32,
    numpy.dtype(numpy.float32): gdal.GDT_Float32
}

value_scale_by_dtype = {
    numpy.dtype(numpy.bool8): "VS_BOOLEAN",
    numpy.dtype(numpy.int32): "VS_NOMINAL",
    numpy.dtype(numpy.float32): "VS_SCALAR"
}


def dataset_pathname(
        data_prefix,
        format_,
        type_,
        kind):

    return os.path.join(data_prefix, "{}_{}.{}".format(
        string_by_type[type_], kind, extension_by_format[format_]))


class TimerData(object):

    nr_rows = 6000
    nr_cols = 80000
    data_prefix = os.getcwd()


    @classmethod
    def dataset_pathname(cls,
            format_,
            type_,
            kind):
        return dataset_pathname(cls.data_prefix, format_, type_, kind)


def move_raster_to_boolean(
        pathname):

    raster = pcraster.readmap(pathname)
    raster = pcraster.boolean(raster)
    pcraster.report(raster, pathname)


def write_array_to_raster(
        array,
        pathname):
    nr_rows, nr_cols = array.shape
    driver = gdal.GetDriverByName("PCRaster")

    hack_boolean = False

    if array.dtype == numpy.dtype(numpy.bool8):
        hack_boolean = True
        array = array.astype(numpy.int32)

    dataset = driver.Create(pathname, nr_cols, nr_rows, 1,
        gdal_type_by_dtype[array.dtype],
        ["PCRASTER_VALUESCALE={}".format(
            value_scale_by_dtype[array.dtype])])
    dataset.GetRasterBand(1).WriteArray(array)

    if hack_boolean:
        del dataset
        move_raster_to_boolean(pathname)


def generate_random_raster(
        data_prefix,
        format_,
        type_,
        kind):

    pathname = dataset_pathname(data_prefix, format_, type_, kind)

    if not os.path.exists(pathname):
        # - Generate a 2D numpy array with random values.
        # - Pass the array to GDAL and create a PCRaster raster from it.
        array = numpy.random.rand(nr_rows, nr_cols)

        if type_ == numpy.bool8:
            array = array >= 0.5
        else:
            array *= 100

        array = array.astype(type_)

        assert array.dtype == numpy.dtype(type_), "{} != {}".format(
            array.dtype, numpy.dtype(type_))

        write_array_to_raster(array, pathname)


def create_datasets(
        data_prefix):

    assert os.path.exists(data_prefix), data_prefix

    format_ = "pcraster"

    generate_random_raster(data_prefix, format_, numpy.float32, "random_1")
    generate_random_raster(data_prefix, format_, numpy.float32, "random_2")

    generate_random_raster(data_prefix, format_, numpy.int32, "random_1")

    generate_random_raster(data_prefix, format_, numpy.bool8, "random_1")
