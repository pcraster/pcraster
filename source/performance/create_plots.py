#!/usr/bin/env python
import os
import sys
import docopt
import devbase
import performance.plot


doc_string = """\
Create plots showing the performance measurement results

Usage:
    {command} [--database=<name>]

Options:
    -h --help               Show this screen
    --version               Show version
    --database=<name>       Overwrite default database name
""".format(
        command = os.path.basename(sys.argv[0]))


@devbase.checked_call
def create_plots(
        database_name):

    performance.plot.create_plots(database_name)


if __name__ == '__main__':
    arguments = docopt.docopt(doc_string)

    database_name = performance.determine_database_name()

    if arguments["--database"] is not None:
        database_name = arguments["--database"]

    sys.exit(create_plots(database_name))
