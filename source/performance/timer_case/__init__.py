import multiprocessing
import os
import performance_analyst as pa
import performance.timer_data
import performance.timer_case.timer_case_base


def hack_settings_in(
        data_prefix,
        repeat,
        max_nr_worker_threads=multiprocessing.cpu_count()):

    performance.timer_data.TimerData.data_prefix = data_prefix
    performance.timer_case.timer_case_base.TimerCase.repeat = repeat
    performance.timer_case.timer_case_base.TimerCase.max_nr_worker_threads = \
        max_nr_worker_threads


def measure_classic_operation_performance(
        data_prefix,
        repeat,
        runner):

    hack_settings_in(data_prefix, repeat)

    loader = pa.TimerLoader()

    names = [
        "classic_operation_timer_case.ClassicOperationTimerCase"
    ]
    cases = [loader.load_timers_from_name(name, [os.path.split(__file__)[0]])
        for name in names]
    suites = pa.TimerSuite(cases)
    runner.run(suites)


def measure_multicore_operation_performance(
        data_prefix,
        repeat,
        runner):

    hack_settings_in(data_prefix, repeat)

    loader = pa.TimerLoader()

    names = [
        "multicore_operation_timer_case.MulticoreOperationTimerCase"
    ]
    cases = [loader.load_timers_from_name(name, [os.path.split(__file__)[0]])
        for name in names]
    suites = pa.TimerSuite(cases)
    runner.run(suites)


def measure_multicore_operation_scalability(
        data_prefix,
        repeat,
        runner,
        max_nr_worker_threads):

    hack_settings_in(data_prefix, repeat, max_nr_worker_threads)

    loader = pa.TimerLoader()

    names = [
        "scalability_timer_case.ScalabilityTimerCase"
    ]
    cases = [loader.load_timers_from_name(name, [os.path.split(__file__)[0]])
        for name in names]
    suites = pa.TimerSuite(cases)
    runner.run(suites)
