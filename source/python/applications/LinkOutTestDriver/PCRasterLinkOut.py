import sys
import platform
import os.path
import os

# c means the low level C functions of pcraster_model_engine
c = None

assert sys.version_info[0] == 2
assert sys.version_info[1] >= 5, "ctypes need python 2.5"

from ctypes import *

if platform.system() == "Linux":
   from ctypes import CDLL
   _libraryName = "libpcraster_model_engine.so"
   c = CDLL(_libraryName)
elif platform.system() == "Windows":
   from ctypes import WinDLL
   _libraryName = "pcraster_model_engine"
   c = WinDLL(_libraryName)
else:
  raise Exception("Platform %s not supported" % platform.system())

if not c:
  raise Exception("%s not found" % _libraryName)

c.pcr_createScriptFromTextFile.argtypes = [ c_char_p ]
c.pcr_createScriptFromTextFile.restype = c_void_p

c.pcr_createScriptFromXMLFile.argtypes = [ c_char_p ]
c.pcr_createScriptFromXMLFile.restype = c_void_p

c.pcr_ScriptError.argtypes = [ c_void_p ]
c.pcr_ScriptError.restype = c_int

c.pcr_ScriptErrorMessage.argtypes = [ c_void_p ]
c.pcr_ScriptErrorMessage.restype = c_char_p

c.pcr_ScriptXMLReflection.argtypes = [ c_void_p ]
c.pcr_ScriptXMLReflection.restype = c_char_p

c.pcr_ScriptExecute.argtypes = [ c_void_p ]
c.pcr_ScriptExecute.restype = c_char_p

c.pcr_ScriptExecuteInitialStepMemory.argtypes = [ c_void_p, POINTER(c_void_p) ]
c.pcr_ScriptExecuteInitialStepMemory.restype = c_int

c.pcr_ScriptExecuteNextTimeStepMemory.argtypes = [ c_void_p, c_void_p ]
c.pcr_ScriptExecuteNextTimeStepMemory.restype = c_int

c.pcr_ScriptExecuteFinish.argtypes = [ c_void_p ]
c.pcr_ScriptExecuteFinish.restype = c_char_p

c.pcr_destroyScript.argtypes = [ c_void_p ]
# c.pcr_destroyScript.restype  is void
