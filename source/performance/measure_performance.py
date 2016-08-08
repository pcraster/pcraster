#!/usr/bin/env python
import multiprocessing
import os
import sys
import docopt
import devbase
import performance_analyst as pa
import timer_case
import performance


doc_string = """\
Measure the performance of a PCRaster installation and store the results in
a database

Usage:
    {command} [--repeat=<count>] [--max-nr-worker-threads=<count>]
        [--print | --database=<name>]
        [--skip-classic] [--skip-multicore] [--skip-scalability]
        <data_prefix>

Options:
    -h --help           Show this screen
    --version           Show version
    --repeat=<count>    Number of times to run each case [default: 3]
    --max-nr-worker-threads=<count>  Max number of threads used for scalability
    --print             Output results to screen instead of database
    --database=<name>   Overwrite default database name
    --skip-classic      Skip measurements for classic operations
    --skip-multicore    Skip measurements for multicore operations
    --skip-scalability  Skip scalability measurements
    data_prefix         Pathname of directory containing input data

By default
- All performance measurements will be run
- Results will be written to a database
- The name of the database will be generated
""".format(
        command = os.path.basename(sys.argv[0]))


def measure_classic_operation_performance(
        data_prefix,
        repeat,
        runner):
    timer_case.measure_classic_operation_performance(data_prefix, repeat,
        runner)


def measure_multicore_operation_performance(
        data_prefix,
        repeat,
        runner):
    timer_case.measure_multicore_operation_performance(data_prefix, repeat,
        runner)


def measure_operation_performance(
        data_prefix,
        repeat,
        runner,
        skip_classic,
        skip_multicore):

    if not skip_classic:
        measure_classic_operation_performance(data_prefix, repeat, runner)

    if not skip_multicore:
        measure_multicore_operation_performance(data_prefix, repeat, runner)


def measure_multicore_operation_scalability(
        data_prefix,
        repeat,
        runner,
        max_nr_worker_threads):
    timer_case.measure_multicore_operation_scalability(data_prefix, repeat,
        runner, max_nr_worker_threads)


def measure_operation_scalability(
        data_prefix,
        repeat,
        runner,
        max_nr_worker_threads):
    measure_multicore_operation_scalability(data_prefix, repeat, runner,
        max_nr_worker_threads)


@devbase.checked_call
def measure_performance(
        data_prefix,
        repeat,
        runner,
        max_nr_worker_threads,
        skip_classic,
        skip_multicore,
        skip_scalability):
    measure_operation_performance(data_prefix, repeat, runner,
        skip_classic, skip_multicore)

    if not skip_scalability:
        measure_operation_scalability(data_prefix, repeat, runner,
            max_nr_worker_threads)


if __name__ == '__main__':
    arguments = docopt.docopt(doc_string)
    data_prefix = arguments["<data_prefix>"]
    repeat = int(arguments["--repeat"])
    max_nr_worker_threads = multiprocessing.cpu_count()
    skip_classic = arguments["--skip-classic"]
    skip_multicore = arguments["--skip-multicore"]
    skip_scalability = arguments["--skip-scalability"]
    print_to_screen = arguments["--print"]

    if print_to_screen:
        output_runner = pa.StreamTimerRunner()
    else:
        database_name = performance.determine_database_name()

        if arguments["--database"] is not None:
            database_name = arguments["--database"]

        output_runner = pa.SQLiteTimerRunner(database_name)

    if arguments["--max-nr-worker-threads"] is not None:
        max_nr_worker_threads = min(max_nr_worker_threads,
            int(arguments["--max-nr-worker-threads"]))

    assert 0 < max_nr_worker_threads <= multiprocessing.cpu_count(), \
        max_nr_worker_threads

    progress_runner = pa.ProgressTimerRunner()

    runner = pa.CompositeTimerRunner([output_runner, progress_runner])

    sys.exit(measure_performance(data_prefix, repeat, runner,
        max_nr_worker_threads,
        skip_classic=skip_classic, skip_multicore=skip_multicore,
        skip_scalability=skip_scalability))
