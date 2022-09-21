import os
import pathlib
import sys


# On Windows prepend the path to our dlls to the PATH environment variable.
# Otherwise our dlls won't be found when our Python extensions are loaded
# by Python.
if sys.platform == "win32":
    pcraster_installation_root = pathlib.Path(__file__).parent.parent.parent.parent
    pcraster_dll_pathname = pathlib.Path(pcraster_installation_root, "bin")
    if pcraster_dll_pathname.exists():
        os.add_dll_directory(pcraster_dll_pathname)


from ._pcraster_mldd import *
