import os
import sys


# On Windows prepend the path to our dlls to the PATH environment variable.
# Otherwise our dlls won't be found when our Python extensions are loaded
# by Python.
if sys.platform == "win32":
    path_ = os.environ["PATH"]
    pcraster_installation_root = os.path.abspath(os.path.join(
        os.path.dirname(__file__), "..", ".."))
    pcraster_dll_pathname = os.path.join(pcraster_installation_root, "lib")
    if os.path.exists(pcraster_dll_pathname):
        os.environ["PATH"] = pcraster_dll_pathname + os.pathsep + path_

try:
    from .operations import *
    from . import operators
    from ._pcraster import *
    from ._pcraster_modflow import *
    from .aguila import *
    from .numpy_operations import *

    try:
        _var = "PCRASTER_NR_WORKER_THREADS"
        _workers = int(os.environ[_var])
        from .multicore import *
        set_nr_worker_threads(_workers)
    except ValueError as err:
        msg = "Could not obtain the number of worker threads. The environment variable {} is set to '{}'; use an integer value instead.".format(
            _var, os.environ[_var])
        raise RuntimeError(msg)
    except KeyError:
        pass # No multicore module use intended
    except ImportError:
        pass # Multicore module was not installed

finally:
    if sys.platform == "win32":
        os.environ["PATH"] = path_
