import inspect
import numpy
import pcraster as pcr
from performance.timer_case import timer_case_base


class OperationTimerCase(timer_case_base.TimerCase):

    @classmethod
    def set_up_class(cls):
        format_ = "pcraster"

        cls.bool8_random_1_raster = pcr.readmap(
            cls.data.dataset_pathname(format_, numpy.bool8, "random_1"))
        cls.int32_random_1_raster = pcr.readmap(
            cls.data.dataset_pathname(format_, numpy.int32, "random_1"))
        cls.float32_random_1_raster = pcr.readmap(
            cls.data.dataset_pathname(format_, numpy.float32, "random_1"))
        cls.float32_random_2_raster = pcr.readmap(
            cls.data.dataset_pathname(format_, numpy.float32, "random_2"))

        cls.raster_by_value_scale = {
            pcr.VALUESCALE.Boolean: [
                cls.bool8_random_1_raster],
            pcr.VALUESCALE.Nominal: [
                cls.int32_random_1_raster],
            pcr.VALUESCALE.Scalar: [
                cls.float32_random_1_raster,
                cls.float32_random_2_raster]
        }


    @classmethod
    def tear_down_class(cls):
        del cls.raster_by_value_scale
        del cls.float32_random_2_raster
        del cls.float32_random_1_raster
        del cls.int32_random_1_raster
        del cls.bool8_random_1_raster


    @classmethod
    def case_names(cls):
        return ["{}_{}".format(cls.__name__, tuple[0]) for tuple in
            inspect.getmembers(cls, inspect.ismethod)
                if tuple[0].find("time_") == 0]


def operation_timer_name(
        operation):
    return "time_{}".format(operation.name)


def create_operation_timer_case(
        operation,
        function):

    value_scales = [argument.value_scale for argument in operation.arguments]
    indices = []

    for i in range(len(value_scales)):
        indices.append(value_scales[:i].count(value_scales[i]))


    def method(self):
        rasters = []

        for i in range(len(value_scales)):
            rasters.append(
                self.raster_by_value_scale[value_scales[i]][indices[i]])

        result = function(*rasters)


    method.__name__ = operation_timer_name(operation)
    method.repeat = timer_case_base.TimerCase.repeat


    return method


def add_operation_timer_cases(
    timer_suite,
    operation_module,
    operations):

    for operation in operations:
        function = getattr(operation_module, operation.name)
        timer_suite.add_method(create_operation_timer_case(operation,
            function))
