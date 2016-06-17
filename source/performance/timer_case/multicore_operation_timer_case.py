import numpy
import pcraster.multicore as pcrmc
from performance.timer_case import multicore_operation, operation_timer_case


class MulticoreOperationTimerCase(operation_timer_case.OperationTimerCase):

    def set_up(self):
        pcrmc.set_nr_cpus(1)
        operation_timer_case.OperationTimerCase.set_up(self)


operation_timer_case.add_unary_operation_timer_cases(
    MulticoreOperationTimerCase, pcrmc, multicore_operation.unary_operations)
operation_timer_case.add_binary_operation_timer_cases(
    MulticoreOperationTimerCase, pcrmc, multicore_operation.binary_operations)
