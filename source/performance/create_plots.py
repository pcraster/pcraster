#!/usr/bin/env python
import multiprocessing
import os
import sys
import docopt
import devbase
import performance.plot


doc_string = """\
Create plots showing the performance measurement results

Usage:
    {command} [--max-nr-worker-threads=<count>] [--database=<name>]

Options:
    -h --help          Show this screen
    --version          Show version
    --max-nr-worker-threads=<count>  Max number of threads used for scalability
    --database=<name>  Overwrite default database name
""".format(
        command = os.path.basename(sys.argv[0]))


@devbase.checked_call
def create_plots(
        database_name,
        max_nr_worker_threads):

    performance.plot.create_plots(database_name, max_nr_worker_threads)


if __name__ == '__main__':
    arguments = docopt.docopt(doc_string)
    max_nr_worker_threads = multiprocessing.cpu_count()

    if arguments["--max-nr-worker-threads"] is not None:
        max_nr_worker_threads = min(max_nr_worker_threads,
            int(arguments["--max-nr-worker-threads"]))

    assert 0 < max_nr_worker_threads <= multiprocessing.cpu_count(), \
        max_nr_worker_threads

    database_name = performance.determine_database_name()

    if arguments["--database"] is not None:
        database_name = arguments["--database"]

    sys.exit(create_plots(database_name, max_nr_worker_threads))
