import shlex
import subprocess
from performance.timer_case.classic_operation_timer_case import \
    ClassicOperationTimerCase
from performance.timer_case.multicore_operation_timer_case import \
    MulticoreOperationTimerCase


def execute_command(
        command):

    messages = subprocess.check_output(shlex.split(command),
        stderr=subprocess.STDOUT)

    return messages


def plot_classic_operations(
        database_name):

    for name in ClassicOperationTimerCase.timer_case_names():
        command = "pa.py plot --real {plot}.pdf {database} {timer_case}".format(
            plot=name,
            database=database_name,
            timer_case=name)
        execute_command(command)


def plot_multicore_operations(
        database_name):

    for name in MulticoreOperationTimerCase.timer_case_names():
        command = "pa.py plot --real {plot}.pdf {database} {timer_case}".format(
            plot=name,
            database=database_name,
            timer_case=name)
        execute_command(command)


def plot_alternative_operations(
        database_name):

    classic_operation_timer_case_names = \
        ClassicOperationTimerCase.timer_case_names()
    multicore_operation_timer_case_names = \
        MulticoreOperationTimerCase.timer_case_names()

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



def create_plots(
        database_name):

    plot_classic_operations(database_name)
    plot_multicore_operations(database_name)
    plot_alternative_operations(database_name)
