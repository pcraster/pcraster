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


def create_unary_operation_timer_case(
        operation,
        function,
        nr_threads):

    def method(self):
        pcrmc.set_nr_cpus(nr_threads)
        result = function(
            self.raster_by_value_scale[operation.arguments[0].value_scale][0])

    method.__name__ = operation_timer_name(operation, nr_threads)
    method.repeat = timer_case_base.TimerCase.repeat

    return method


def create_binary_operation_timer_case(
        operation,
        function,
        nr_threads):

    def method(self):
        pcrmc.set_nr_cpus(nr_threads)
        result = function(
            self.raster_by_value_scale[operation.arguments[0].value_scale][0],
            self.raster_by_value_scale[operation.arguments[1].value_scale][1])

    method.__name__ = operation_timer_name(operation, nr_threads)
    method.repeat = timer_case_base.TimerCase.repeat

    return method


def add_unary_operation_timer_cases():

    timer_suite = ScalabilityTimerCase
    operation_module = pcrmc
    operations = multicore_operation.unary_operations
    nr_cores = multiprocessing.cpu_count()

    for operation in operations:
        function = getattr(operation_module, operation.name)

        for n in xrange(1, nr_cores + 1):
            timer_suite.add_method(create_unary_operation_timer_case(
                operation, function, n))


def add_binary_operation_timer_cases():

    timer_suite = ScalabilityTimerCase
    operation_module = pcrmc
    operations = multicore_operation.binary_operations
    nr_cores = multiprocessing.cpu_count()

    for operation in operations:
        function = getattr(operation_module, operation.name)

        for n in xrange(1, nr_cores + 1):
            timer_suite.add_method(create_binary_operation_timer_case(
                operation, function, n))


add_unary_operation_timer_cases()
add_binary_operation_timer_cases()
