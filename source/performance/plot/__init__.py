import re
import shlex
import sqlite3
import subprocess
import matplotlib.pyplot as pyplot
import matplotlib.ticker as ticker
import pylab
import performance_analyst.pa.util as pa_util
from performance.timer_case.classic_operation_timer_case import \
    ClassicOperationTimerCase
from performance.timer_case.multicore_operation_timer_case import \
    MulticoreOperationTimerCase
from performance.timer_case.scalability_timer_case import ScalabilityTimerCase


def execute_command(
        command):

    messages = subprocess.check_output(shlex.split(command),
        stderr=subprocess.STDOUT)

    return messages


def plot_classic_operations(
        database_name):

    for name in ClassicOperationTimerCase.case_names():
        command = "pa.py plot --real {plot}.pdf {database} {timer_case}".format(
            plot=name,
            database=database_name,
            timer_case=name)
        execute_command(command)


def plot_multicore_operations(
        database_name):

    for name in MulticoreOperationTimerCase.case_names():
        command = "pa.py plot --real {plot}.pdf {database} {timer_case}".format(
            plot=name,
            database=database_name,
            timer_case=name)
        execute_command(command)


def plot_alternative_operations(
        database_name):

    # patterns: (classic, multicore)
    patterns = [
        ("abs", "abs"),
        ("acos", "acos"),
        ("pcrbadd", "add"),
        ("asin", "asin"),
        ("atan", "atan"),
        # ("boolean", "boolean"),
        ("_cos", "_cos"),
        # ("cover", "cover"),
        # ("defined", "defined"),
        # ("pcrfdiv", "div"),
        # ("equal", "equal"),
        # ("fac", "fac"),
        # ("pcrgt", "greater"),
        # ("pcrge", "greater_equal"),
        ("ifthen", "ifthen"),
        ("ifthenelse", "ifthenelse"),
        # ("pcrlt", "less"),
        # ("pcrle", "less_equal"),
        # ("ln", "ln"),
        # ("log10", "log10"),
        # ("mapmaximum", "mapmaximum"),
        # ("mapminimum", "mapminimum"),
        # ("max", "max"),
        # ("min", "min"),
        ("pcrmul", "mul"),
        # ("nominal", "nominal"),
        # ("ordinal", "ordinal"),
        ("pcrpow", "power"),
        # ("rounddown", "rounddown"),
        # ("roundoff", "roundoff"),
        # ("roundup", "roundup"),
        # ("scalar", "scalar"),
        # ("_sin", "_sin"),
        ("slope", "slope"),
        # ("sqr", "sqr"),
        ("sqrt", "sqrt"),
        ("pcrbmin", "sub"),
        # ("_tan", "_tan"),
        # ("pcrne", "unequal"),
        ("window4total", "window4total"),
    ]


    classic_operation_timer_case_names = \
        ClassicOperationTimerCase.case_names()
    multicore_operation_timer_case_names = \
        MulticoreOperationTimerCase.case_names()


    for pattern in patterns:
        classic_pattern = pattern[0]
        multicore_pattern = pattern[1]

        classic_names = [name for name in
            classic_operation_timer_case_names if name.endswith(
                classic_pattern) ]
        assert len(classic_names) == 1, "{}: {}".format(classic_pattern,
            classic_names)

        multicore_names = [name for name in
            multicore_operation_timer_case_names if name.endswith(
                multicore_pattern)]
        assert len(multicore_names) == 1, "{}: {}".format(multicore_pattern,
            multicore_names)

        command = "pa.py plot --real {plot}.pdf {database} " \
                "--timestamp=2016-01-01 " \
                "{classic_timer_case} {multicore_timer_case}".format(
            plot=multicore_pattern.lstrip("_"),
            database=database_name,
            classic_timer_case=classic_names[0],
            multicore_timer_case=multicore_names[0])
        execute_command(command)


def plot_scalability(
        database_name,
        max_nr_worker_threads):

    connection = sqlite3.connect(database_name)
    cursor = connection.cursor()

    expression = re.compile(r"^ScalabilityTimerCase_time_(\w+)_(\d+)$")

    # Per operation the number of threads for which we have data.
    timer_cases = {}


    for name in ScalabilityTimerCase.case_names():
        match = re.match(expression, name)
        assert match, name

        operation_name = match.group(1)
        nr_worker_threads = int(match.group(2))

        if nr_worker_threads <= max_nr_worker_threads:

            timestamps = pa_util.timestamps(cursor, name)
            assert timestamps

            # Pick most recent timings.
            timestamp = timestamps[-1]
            real_times, _ = pa_util.timings(cursor, name, timestamp)

            timer_cases.setdefault(operation_name, [])

            timer_cases[operation_name].append((nr_worker_threads, min(real_times)))


    for name in timer_cases:

        # Sort coordinates by nr_worker_threads.
        timer_cases[name] = sorted(timer_cases[name], key=lambda tpl: tpl[0])


    for name in timer_cases:

        figure = pyplot.figure(figsize=(15, 10))
        axis = figure.add_subplot(111)

        nr_plots = 1
        labels = [name]

        sorted_coordinates = timer_cases[name]
        nr_worker_threads = [tpl[0] for tpl in sorted_coordinates]
        real_times = [tpl[1] for tpl in sorted_coordinates]

        assert nr_worker_threads[0] == 1
        theoretical_times = [real_times[0] / nr_worker_threads[t] for t in
            range(len(nr_worker_threads))]

        axis.plot(nr_worker_threads, real_times, "o--")
        axis.plot(nr_worker_threads, theoretical_times)

        axis.set_title("{}: {} (real)".format(database_name, name))

        axis.set_xlabel("Threads")
        axis.set_ylabel("Time (s)")

        limits = axis.get_xlim()
        axis.set_xlim(limits[0] - 0.1, limits[1] + 0.1)
        axis.grid(True)

        pylab.savefig("scalability_{}.pdf".format(name))


def create_plots(
        database_name,
        max_nr_worker_threads):

    plot_classic_operations(database_name)
    plot_multicore_operations(database_name)
    plot_alternative_operations(database_name)
    plot_scalability(database_name, max_nr_worker_threads)
