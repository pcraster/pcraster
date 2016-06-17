import inspect
import numpy
import pcraster as pcr
from performance.timer_case import timer_case_base


class OperationTimerCase(timer_case_base.TimerCase):

    @classmethod
    def set_up_class(cls):
        format_ = "pcraster"
        dtype = numpy.float32

        cls.f32_random_1_raster = pcr.readmap(
            cls.data.dataset_pathname(format_, dtype, "random_1"))
        cls.f32_random_2_raster = pcr.readmap(
            cls.data.dataset_pathname(format_, dtype, "random_2"))

        cls.raster_by_value_scale = {
            pcr.VALUESCALE.Scalar: [
                cls.f32_random_1_raster,
                cls.f32_random_2_raster]
        }


    @classmethod
    def tear_down_class(cls):
        del cls.raster_by_value_scale
        del cls.f32_random_2_raster
        del cls.f32_random_1_raster


    @classmethod
    def case_names(cls):
        return ["{}_{}".format(cls.__name__, tuple[0]) for tuple in
            inspect.getmembers(cls, inspect.ismethod)
                if tuple[0].find("time_") == 0]


def operation_timer_name(
        operation):
    return "time_{}".format(operation.name)


def create_unary_operation_timer_case(
        operation,
        function):

    def method(self):
        result = function(
            self.raster_by_value_scale[operation.arguments[0].value_scale][0])

    method.__name__ = operation_timer_name(operation)
    method.repeat = timer_case_base.TimerCase.repeat

    return method


def create_binary_operation_timer_case(
        operation,
        function):

    def method(self):
        result = function(
            self.raster_by_value_scale[operation.arguments[0].value_scale][0],
            self.raster_by_value_scale[operation.arguments[1].value_scale][1])

    method.__name__ = operation_timer_name(operation)
    method.repeat = timer_case_base.TimerCase.repeat

    return method


def add_unary_operation_timer_cases(
    timer_suite,
    operation_module,
    operations):

    for operation in operations:
        function = getattr(operation_module, operation.name)
        timer_suite.add_method(create_unary_operation_timer_case(
            operation, function))


def add_binary_operation_timer_cases(
    timer_suite,
    operation_module,
    operations):

    for operation in operations:
        function = getattr(operation_module, operation.name)
        timer_suite.add_method(create_binary_operation_timer_case(
            operation, function))
