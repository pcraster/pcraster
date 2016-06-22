import multiprocessing
import numpy
import pcraster.multicore as pcrmc
from performance.timer_case import multicore_operation, operation_timer_case, \
    timer_case_base


class ScalabilityTimerCase(operation_timer_case.OperationTimerCase):

    def set_up(self):
        pcrmc.set_nr_cpus(1)
        operation_timer_case.OperationTimerCase.set_up(self)


def operation_timer_name(
        operation,
        nr_threads):
    return "time_{}_{}".format(operation.name, nr_threads)


def create_operation_timer_case(
        operation,
        function,
        nr_threads):

    value_scales = [argument.value_scale for argument in operation.arguments]
    indices = []

    for i in xrange(len(value_scales)):
        indices.append(value_scales[:i].count(value_scales[i]))


    def method(self):
        pcrmc.set_nr_cpus(nr_threads)

        rasters = []

        for i in xrange(len(value_scales)):
            rasters.append(
                self.raster_by_value_scale[value_scales[i]][indices[i]])

        result = function(*rasters)


    method.__name__ = operation_timer_name(operation, nr_threads)
    method.repeat = timer_case_base.TimerCase.repeat

    return method


def add_operation_timer_cases():

    timer_suite = ScalabilityTimerCase
    operation_module = pcrmc
    operations = multicore_operation.operations
    nr_threads = ScalabilityTimerCase.max_nr_threads

    for operation in operations:
        function = getattr(operation_module, operation.name)

        for n in xrange(1, nr_threads + 1):
            timer_suite.add_method(create_operation_timer_case(
                operation, function, n))


add_operation_timer_cases()
