#!/usr/bin/env python
import os
import sys
import docopt
import gdal
import devbase
import performance.timer_data


doc_string = """\
Create datasets to be used by the performance measurements

Usage:
    {command} <data_prefix>

Options:
    -h --help               Show this screen
    --version               Show version
    data_prefix             Pathname of directory containing input data
""".format(
        command = os.path.basename(sys.argv[0]))


@devbase.checked_call
def create_datasets(
        data_prefix):

    performance.timer_data.create_datasets(data_prefix)


if __name__ == '__main__':
    gdal.UseExceptions()

    arguments = docopt.docopt(doc_string)

    data_prefix = arguments["<data_prefix>"]

    sys.exit(create_datasets(data_prefix))
