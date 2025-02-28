import os
import pathlib
import shutil
import sys


# On Windows prepend the path to our dlls to the PATH environment variable.
# Otherwise our dlls won't be found when our Python extensions are loaded
# by Python.
if sys.platform == "win32":
    _pcraster_app_path = shutil.which("pcrcalc.exe")
    if _pcraster_app_path:
        _pcraster_bin_pathname = pathlib.Path(_pcraster_app_path).parent
        os.add_dll_directory(_pcraster_bin_pathname)
        del _pcraster_bin_pathname
    del _pcraster_app_path


from ._pcraster_mldd import initialise

__all__ = ["initialise"]

# Remove symbols imported for internal use
del os, pathlib, shutil, sys
