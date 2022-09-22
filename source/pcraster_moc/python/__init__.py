import os
import pathlib
import shutil
import sys


# On Windows prepend the path to our dlls to the PATH environment variable.
# Otherwise our dlls won't be found when our Python extensions are loaded
# by Python.
if sys.platform == "win32":
    pcraster_app_path = shutil.which("pcrcalc.exe")
    if pcraster_app_path:
        pcraster_bin_pathname = pathlib.Path(pcraster_app_path).parent
        os.add_dll_directory(pcraster_bin_pathname)

from ._pcraster_moc import *
