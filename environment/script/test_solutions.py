#!/usr/bin/env python
import json
import logging
import os
import shlex
import subprocess
import sys
import docopt
import devbase


doc_string = """\
Test running user commands in a new provisioned OS

Usage:
    {command} [--prefix=<directory>] [--cpus=<amount>]
        [--memory=<amount>] <configuration>

Options:
    -h --help               Show this screen
    --version               Show version
    --prefix=<directory>    Directory to store results (default: current dir)
    --cpus=<amount>         Number of CPUs to allocate for virtual machines
    --memory=<amount>       Amount of memory to allocate for virtual machines
    configuration           Pathname of file containing configuration

The prefix directory will be used to store directories and files. It
is probably a good idea if this directory is empty.
""".format(
        command = os.path.basename(sys.argv[0]))


def log_debug(
        logger,
        message):
    logger.debug(message)
    logging.getLogger("screen").debug(message)


def log_info(
        logger,
        message):
    logger.info(message)
    logging.getLogger("screen").info(message)


def log_error(
        logger,
        message):
    logger.error(message)
    logging.getLogger("screen").error(message)


def log_critical(
        logger,
        message):
    logger.critical(message)
    logging.getLogger("screen").critical(message)


def execute_command(
        logger,
        command):

    try:
        log_debug(logger, "execute: {}".format(command))
        messages = subprocess.check_output(shlex.split(command),
            stderr=subprocess.STDOUT, universal_newlines=True)
        log_debug(logger, messages)
    except subprocess.CalledProcessError, exception:
        log_error(logger, exception.output)
        raise

    return messages


def box_name_to_filename(
        box_name):

    return "".join(c if (c.isalnum() or c in "._- ") else "_"
        for c in box_name)


def vagrant_box_exists(
        logger,
        box_name):
    command = "vagrant box list"
    log_debug(logger, "list vagrant boxes")
    messages = execute_command(logger, command)
    messages = messages.strip()
    box_names = [line.split()[0] for line in messages.split("\n")]

    return box_name in box_names


def add_vagrant_box(
        logger,
        box_name):
    command = "vagrant box add --provider virtualbox {}".format(box_name)
    log_info(logger, "add vagrant box")
    execute_command(logger, command)


def initialize_vagrant_box(
        logger,
        box_name):
    command = "vagrant init --minimal".format(box_name)
    log_info(logger, "initialize vagrant box")
    execute_command(logger, command)


def configure_vagrant_box(
        logger,
        box_name,
        nr_cpus,
        amount_of_memory,
        provisions):

    provisions = ["config.vm.provision \"shell\", inline: \"{}\"".format(
        provision) for provision in provisions]

    # Replace the default Vagrant configuration file with a new one.
    vagrant_configuration = """\
Vagrant.configure(2) do |config|
    config.vm.box = "{box_name}"

    config.vm.provider "virtualbox" do |virtual_box|
        # virtual_box.name = "{box_name}"  # TODO Not unique
        # virtual_box.cpus = "{nr_cpus}"
        # virtual_box.memory = "{amount_of_memory}"
        # virtual_box.gui = true

        virtual_box.customize [
            "modifyvm", :id,
            "--cpus", "{nr_cpus}",
            "--memory", "{amount_of_memory}",
            "--ioapic", "on",
            "--nictype1", "virtio",
            "--nictype2", "virtio",
            "--natdnshostresolver1", "on",
            "--natdnsproxy1", "on",
            "--ioapic", "on"
        ]

    end

    {provisions}
end
""".format(
            box_name=box_name,
            nr_cpus=nr_cpus,
            amount_of_memory=amount_of_memory,
            provisions="\n    ".join(provisions)
        )

    file("Vagrantfile", "w").write(vagrant_configuration)

    # TODO Add provisions to the VagrantFile


def create_vagrant_box(
        logger,
        box_name,
        nr_cpus,
        amount_of_memory,
        provisions):

    if not vagrant_box_exists(logger, box_name):
        add_vagrant_box(logger, box_name)

    initialize_vagrant_box(logger, box_name)
    configure_vagrant_box(logger, box_name, nr_cpus, amount_of_memory,
        provisions)


def start_vagrant_box(
        logger,
        box_name):

    command = "vagrant up --provider virtualbox --destroy-on-error"
    log_info(logger, "start vagrant box")
    execute_command(logger, command)


def suspend_vagrant_box(
        logger):

    command = "vagrant suspend"
    log_info(logger, "suspend vagrant box")
    execute_command(logger, command)


def run_commands(
        logger,
        box_name,
        commands):

    commands = ["vagrant ssh -c '{}'".format(command) for command in commands]

    log_info(logger, "execute user commands")

    for command in commands:
        execute_command(logger, command)


def halt_vagrant_box(
        logger,
        box_name):

    command = "vagrant halt"
    log_info(logger, "halt vagrant box")
    execute_command(logger, command)


def reboot_vagrant_box(
        logger,
        box_name):

    log_info(logger, "reboot vagrant box")
    halt_vagrant_box(logger, box_name)
    start_vagrant_box(logger, box_name)


def destroy_vagrant_box(
        logger,
        box_name):

    command = "vagrant destroy --force"
    log_info(logger, "destroy vagrant box")
    execute_command(logger, command)


def test_solution(
        prefix_pathname,
        nr_cpus,
        amount_of_memory,
        solution):
    label = solution["label"]
    box_name = solution["box"]
    provisions = solution["provisions"]
    commands = solution["commands"]

    logger = logging.getLogger(box_name)
    log_info(logger, "test solution")
    log_debug(logger, json.dumps(solution))

    cwd = os.getcwd()
    os.chdir(os.path.join(prefix_pathname, box_name_to_filename(box_name)))
    status = 1

    try:

        create_vagrant_box(logger, box_name, nr_cpus, amount_of_memory,
            provisions)

        try:

            # Start and reboot the virtual machine. Starting it the first
            # time may install a new kernel, which should be allowed to
            # start.
            start_vagrant_box(logger, box_name)
            reboot_vagrant_box(logger, box_name)

            # Run the user-commands obtained from the solution.
            run_commands(logger, box_name, commands)

            # All is well: get rid of the virtual machine.
            halt_vagrant_box(logger, box_name)
            destroy_vagrant_box(logger, box_name)

            status = 0

        except subprocess.CalledProcessError, exception:

            # Log the exception to the screen. The command's output is
            # already logged (to the file and to the screen).
            # Stop propagating the exception. We are done testing this
            # solution, others can still be tested.
            logging.getLogger("screen").error(exception)

            # Something went wrong: keep the virtual machine for inspection.
            suspend_vagrant_box(logger)

    except Exception, exception:

        # If we ever end up here, we must fix the script.
        log_critical(logger, "uncaught exception: {}".format(exception))

    finally:

        os.chdir(cwd)

    return status


def initialize_prefix(
        prefix_pathname,
        box_names):
    """
    Create a new directory for each box

    - Directory `prefix_pathname` must exist
    - For none of the boxes a directory must already exist
    """
    assert os.path.exists(prefix_pathname)

    for box_name in box_names:
        os.mkdir(os.path.join(prefix_pathname, box_name_to_filename(
            box_name)))


def setup_logger(
        logger_name,
        log_pathname=None,
        level=logging.INFO):

    # See also:
    # http://stackoverflow.com/questions/17035077/python-logging-to-multiple-log-files-from-different-classes

    logger = logging.getLogger(logger_name)

    if log_pathname is not None:
        file_handler = logging.FileHandler(log_pathname, mode="w")
        logger.addHandler(file_handler)
    else:
        stream_handler = logging.StreamHandler()
        logger.addHandler(stream_handler)

    logger.setLevel(level)


def setup_loggers(
        prefix_pathname,
        box_names):

    setup_logger("screen", level=logging.INFO)

    for box_name in box_names:
        logger_pathname = "{}.log".format(os.path.join(prefix_pathname,
            box_name_to_filename(box_name)))
        setup_logger(box_name, logger_pathname, logging.DEBUG)


@devbase.checked_call
def test_solutions(
        prefix_pathname,
        nr_cpus,
        amount_of_memory,
        configuration_pathname):

    configuration = open(configuration_pathname).read()
    configuration = json.loads(configuration)
    solutions = configuration["solutions"]
    box_names = [solution["box"] for solution in solutions]

    initialize_prefix(prefix_pathname, box_names)
    setup_loggers(prefix_pathname, box_names)

    failures = []

    for solution in solutions:
        failures.append(test_solution(prefix_pathname, nr_cpus,
            amount_of_memory, solution) != 0)

    if not any(failures):
        logging.getLogger("screen").info("All solutions succeeded")
    else:
        logging.getLogger("screen").error("The folowing solutions failed:")

        for i in xrange(len(failures)):
            if failures[i]:
                logging.getLogger("screen").error("    {}".format(
                    solutions[i]["label"]))

        logging.getLogger("screen").error(
            "See the log files for details")
        logging.getLogger("screen").error(
            "The relevant virtual machines were suspended")

    return 0 if any(failures) else 1


if __name__ == "__main__":
    arguments = docopt.docopt(doc_string)

    prefix = os.getcwd()

    if arguments["--prefix"] is not None:
        prefix = arguments["--prefix"]

    nr_cpus = 2
    if arguments["--cpus"] is not None:
        nr_cpus = arguments["--cpus"]
        assert nr_cpus > 0, nr_cpus

    amount_of_memory = 4096
    if arguments["--memory"] is not None:
        amount_of_memory = int(arguments["--memory"])
        assert amount_of_memory > 0, amount_of_memory

    configuration_pathname = arguments["<configuration>"]

    sys.exit(test_solutions(prefix, nr_cpus, amount_of_memory,
        configuration_pathname))
