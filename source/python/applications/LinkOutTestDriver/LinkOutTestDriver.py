import sys

# simple application using
# the low level C functions of PCRasterModelEngine
from PCRasterLinkOut import c

print("PCRaster link out test driver")
if len(sys.argv) != 2:
  print("USAGE: LinkOutTestDriver inputModelFile.xml")
  sys.exit(1)

# argv[1] is an xml file
script = c.pcr_createScriptFromXMLFile(sys.argv[1])
if script == None:
  raise Exception(sys.argv[1]+":PANIC allocation of a few bytes failed")

try:
  # typical error: sys.argv[1] is not existant
  if c.pcr_ScriptError(script):
    raise Exception(sys.argv[1]+":"+c.pcr_ScriptErrorMessage(script))

  c.pcr_ScriptExecute(script)

  # typical errors:
  #  xml is malformed
  #  some inputs are not found
  #  resource error: memory/disk full
  if c.pcr_ScriptError(script):
    raise(Exception, sys.argv[1]+":"+c.pcr_ScriptErrorMessage(script))

finally:
  c.pcr_destroyScript(script)
  script = None

if script != None:
  c.pcr_destroyScript(script)
