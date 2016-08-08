import numpy
import pcraster.multicore as pcrmc
from performance.timer_case import multicore_operation, operation_timer_case


class MulticoreOperationTimerCase(operation_timer_case.OperationTimerCase):

    def set_up(self):

        # Make sure the Fern algorithm instantiation with the sequential
        # execution policy is used. We don't want any threading stuff to
        # be used. The goals is to be able to compare the classic PCRaster
        # implementation (unicore) with the new Fern implementation
        # (also unicore).
        pcrmc.set_nr_worker_threads(0)

        operation_timer_case.OperationTimerCase.set_up(self)


operation_timer_case.add_operation_timer_cases(
    MulticoreOperationTimerCase, pcrmc, multicore_operation.operations)
