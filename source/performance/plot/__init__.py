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

    classic_operation_timer_case_names = \
        ClassicOperationTimerCase.case_names()
    multicore_operation_timer_case_names = \
        MulticoreOperationTimerCase.case_names()

    patterns = [ "abs", "acos", "asin", "atan", "_cos", "slope", "sqrt",
        "window4total"]

    for pattern in patterns:
        classic_names = [name for name in
            classic_operation_timer_case_names if pattern in name]
        assert len(classic_names) == 1, classic_names
        multicore_names = [name for name in
            multicore_operation_timer_case_names if pattern in name]
        assert len(multicore_names) == 1, multicore_names

        command = "pa.py plot --real {plot}.pdf {database} " \
                "{classic_timer_case} {multicore_timer_case}".format(
            plot=pattern,
            database=database_name,
            classic_timer_case=classic_names[0],
            multicore_timer_case=multicore_names[0])
        execute_command(command)


def plot_scalability(
        database_name):

    connection = sqlite3.connect(database_name)
    cursor = connection.cursor()

    expression = re.compile(r"^ScalabilityTimerCase_time_(\w+)_(\d+)$")

    # Per operation the number of threads for which we have data.
    timer_cases = {}

    for name in ScalabilityTimerCase.case_names():
        match = re.match(expression, name)
        assert match, name

        operation_name = match.group(1)
        nr_threads = int(match.group(2))

        timestamps = pa_util.timestamps(cursor, name)
        assert timestamps

        timestamp = timestamps[-1]

        real_times, _ = pa_util.timings(cursor, name, timestamp)

        timer_cases.setdefault(operation_name, [[], []])

        timer_cases[operation_name][0].append(nr_threads)
        timer_cases[operation_name][1].append(min(real_times))

    for name in timer_cases:

        figure = pyplot.figure(figsize=(15, 10))
        axis = figure.add_subplot(111)

        nr_plots = 1
        # colormap = pyplot.cm.brg
        # colors = [colormap(i) for i in numpy.linspace(0, 0.9, nr_plots)]
        labels = [name]

        axis.plot(timer_cases[name][0], timer_cases[name][1], "o--")
            # colors=colors[0],
            # label="threads")

        axis.set_title("{}: {} (real)".format(database_name, name))

        axis.set_xlabel("Threads")
        axis.set_ylabel("Time (s)")

        limits = axis.get_xlim()
        axis.set_xlim(limits[0] - 0.1, limits[1] + 0.1)
        axis.grid(True)

        pylab.savefig("scalability_{}.pdf".format(name))


def create_plots(
        database_name):

    plot_classic_operations(database_name)
    plot_multicore_operations(database_name)
    plot_alternative_operations(database_name)
    plot_scalability(database_name)
