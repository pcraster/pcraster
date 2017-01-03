import exceptions, getpass, os, popen2, stat, string, subprocess, sys, tempfile, types


def fileExists(filename):
  return os.path.exists(filename)

def fileIsRegular(filename, mode):
  return stat.S_ISREG(mode)

def fileIsDirectory(filename, mode):
  return stat.S_ISDIR(mode)

def fileIsReadable(filename):
  result = False

  if fileExists(filename):
    mode = os.stat(filename)[stat.ST_MODE]
    result = fileIsRegular(filename, mode) and \
         (mode & stat.S_IRUSR or mode & stat.S_IRGRP or mode & stat.S_IROTH)

  return result

def fileIsWriteable(filename):
  result = False

  if fileExists(filename):
    mode = os.stat(filename)[stat.ST_MODE]
    result = fileIsRegular(filename, mode) and \
         (mode & stat.S_IWUSR or mode & stat.S_IWGRP or mode & stat.S_IWOTH)
  else:
    result = directoryIsWriteable(os.path.dirname(filename))

  return result

def directoryIsWriteable(filename):
  result = False

  if fileExists(filename):
    mode = os.stat(filename)[stat.ST_MODE]
    result = fileIsDirectory(filename, mode) and \
         (mode & stat.S_IWUSR or mode & stat.S_IWGRP or mode & stat.S_IWOTH)

  return result

def testFileExists(filename):
  if not fileExists(filename):
    raise exceptions.IOError(u"file does not exist: %s" % filename)

def testFileIsRegular(filename, mode):
  if not fileIsRegular(filename, mode):
    raise exceptions.IOError(u"file is not a regular file: %s" % filename)

def testFileIsDirectory(filename, mode):
  if not fileIsDirectory(filename, mode):
    raise exceptions.IOError(u"file is not a directory: %s" % filename)

def testFileIsReadable(filename):
  testFileExists(filename)
  mode = os.stat(filename)[stat.ST_MODE]
  testFileIsRegular(filename, mode)

  if not (mode & stat.S_IRUSR or mode & stat.S_IRGRP or mode & stat.S_IROTH):
    raise exceptions.IOError(u"file is not readable: %s" % filename)

def testDirectoryIsReadable(filename):
  testFileExists(filename)
  mode = os.stat(filename)[stat.ST_MODE]
  testFileIsDirectory(filename, mode)

  if not (mode & stat.S_IRUSR or mode & stat.S_IRGRP or mode & stat.S_IROTH):
    raise exceptions.IOError(u"directory is not readable: %s" % filename)

def testDirectoryIsWritable(filename):
  testFileExists(filename)
  mode = os.stat(filename)[stat.ST_MODE]
  testFileIsDirectory(filename, mode)

  if not (mode & stat.S_IWUSR or mode & stat.S_IWGRP or mode & stat.S_IWOTH):
    raise exceptions.IOError(u"directory is not writable: %s" % filename)

def testFileIsWritable(filename):
  directoryName = os.path.dirname(filename)
  testDirectoryIsWritable(directoryName)

  if os.path.exists(filename):
    mode = os.stat(filename)[stat.ST_MODE]
    testFileIsRegular(filename, mode)

    if not (mode & stat.S_IWUSR or mode & stat.S_IWGRP or mode & stat.S_IWOTH):
      raise exceptions.IOError(u"file is not writable: %s" % filename)

def testStringIsIntegral(string):
  value = int(string)

def testStringIsPositiveIntegral(string):
  value = int(string)

  if value < 0:
    raise exceptions.ValueError(u"value must be positive integral: %s" %
         (string))

def testStringIsPositiveFloat(string):
  value = float(string)

  if value < 0:
    raise exceptions.ValueError(u"value must be positive float: %s" % (string))

def testNumberWithinRange(number, min, max):
  if not (number >= min and number <= max):
    raise exceptions.ValueError(u"value must be in range [%s, %s]: %s" %
         (min, max, number))

def raiseException(message, exception):
  raise Exception(u"%s\n%s" % (message, str(exception)))

def raiseInvalidOptionException(optionName, exception):
  raiseException(u"option: %s" % (optionName), exception)

def raiseUnsupportedOptionException(
         name,
         section):
  raise Exception(u"unsupported option %s used in section %s" % (name, section))

def developAccountKor():
  return sys.platform == "linux2" and getpass.getuser() == "kor"

def execute(command, logFilename=None):

  if logFilename:
    file(logFilename, "a").write("execute: %s\n" % (command))

  stdoutFilename = os.tempnam()
  stderrFilename = os.tempnam()

  try:
    try:
      process = subprocess.Popen(command.split(),
           bufsize=-1,
           stdout=file(stdoutFilename, "wb"),
           stderr=file(stderrFilename, "wb"))
      result = process.wait()
    except AssertionError, exception:
      raise
    except OSError, exception:
      raise exceptions.OSError(
           u"error executing command: %s\nfrom directory: %s\n%s" %
           (command, os.getcwd(), exception))
  finally:
    outputMessages = errorMessages = ""
    if os.path.getsize(stdoutFilename):
      outputMessages = file(stdoutFilename, "r").readlines()

    if os.path.getsize(stderrFilename):
      errorMessages = file(stderrFilename, "r").readlines()

  # This doesn't seem to work in de finally clause.
  os.remove(stdoutFilename)
  os.remove(stderrFilename)

  if result:
    raise exceptions.OSError(
         u"error executing command: %s\nfrom directory: %s\n%s" %
         (command, os.getcwd(),
         "\n".join(errorMessages)))

  return outputMessages

def executePCRCalc(statement,
         logFilename=None):
  command = u"pcrcalc --nothing %s" % (statement)

  return execute(command, logFilename=logFilename)

def executePCRCalcScript(script,
         logFilename=None):
  filename = os.tempnam()
  scriptfile = file(filename, "w")
  scriptfile.write(script)
  scriptfile.close()

  statement = u"-f %s" % (filename)
  command = u"pcrcalc --nothing %s" % (statement)

  result = ""

  try:
    result = execute(command, logFilename=logFilename)
  except AssertionError, exception:
      raise
  except Exception, exception:
    raiseException(u"error executing pcrcalc script: %s" %
         (filename), exception)

  os.remove(filename)

  if logFilename:
    file(logFilename, "a").write("contents of PCRaster script %s:\n%s" % (filename, script))

  return result

def executeTable(
         arguments,
         logFilename=None):
  command = u"table %s" % (arguments)

  return execute(command, logFilename=logFilename)

def executeMap2Col(
         arguments,
         logFilename=None):
  command = u"map2col --nothing %s" % (arguments)

  return execute(command, logFilename=logFilename)

def executeInfluence(
         arguments,
         logFilename=None):
  command = u"influence %s" % (arguments)

  return execute(command, logFilename=logFilename)

def executeMapAttr(
         arguments,
         logFilename=None):
  command = u"mapattr --nothing %s" % (arguments)

  return execute(command, logFilename=logFilename)

def rasterMaximum(
         filename):
  # Assumes filename exists and is readable.
  # Run mapattr and analyse its output.
  arguments = u"-p %s" % (filename)
  output = executeMapAttr(arguments)
  result = None

  for line in output:
    line = line.split()
    if len(line) >= 2 and line[0] == "max_val":
      result = line[1]
      break

  # Probably.
  assert result != "mv"

  return result

def testRasterHasValueScale(filename, valueScales):
  # Assumes filename exists and is readable.
  # Run mapattr and analyse its output.
  arguments = u"-p %s" % (filename)
  output = executeMapAttr(arguments)
  valueScaleFound = False

  for line in output:
    line = line.split()
    if len(line) >= 2 and line[0] == "data_type" and line[1] in valueScales:
      valueScaleFound = True
      break

  if not valueScaleFound:
    raise Exception(u"file %s must have a valid value scale (%s)" %
         (filename, ",".join(valueScales)))

def testRasterIsCategorical(filename):
  testRasterHasValueScale(filename, ["nominal"])

def testRasterIsContinuous(filename):
  testRasterHasValueScale(filename, ["scalar"])

def filenameToPCRasterStatement(filename):
  if sys.platform == "win32":
    return "\"%s\"" % (filename)
  elif sys.platform == "linux2":
    return "\"%s\"" % (filename)
  else:
    return "%s" % (filename)

def executeRScript(
         scriptFilename,
         outputFilename,
         logFilename=None):
  try:
    if sys.platform == "win32":
      # Escape backslash.
      scriptFilename = scriptFilename.replace("\\", "/")
      outputFilename = outputFilename.replace("\\", "/")

    execute(u"R CMD BATCH %s %s" % (scriptFilename, outputFilename),
         logFilename=logFilename)
  except AssertionError, exception:
    raise
  except Exception, exception:
    message = "unknown error, please run the script yourself"

    if fileIsReadable(outputFilename):
      message = u"last 10 lines from %s:\n%s" % \
         (outputFilename,
         " ".join(file(outputFilename).readlines()[-10:]).rstrip(u"\n"))

    raiseException(u"error executing R script %s:\n%s" %
         (scriptFilename, message), exception)


def show(
       file,
       messages,
       appendNewLineIfNeeded=True):
  """
  Prints the messages in file.

  Default each message printed is appended with a newline.

  @type file: file
  @param file: File to print the message in.
  @type messages: String or list of strings.
  @param messages: Strings to print to file.
  """
  if isinstance(messages, types.ListType):
    for message in messages:
      self.show(stream, message)
  else:
    file.write(messages)

    if appendNewLineIfNeeded:
      file.write("\n")

    file.flush()

def showWarning(
         messages,
         appendNewLineIfNeeded=True):
  if isinstance(messages, types.ListType):
    for i in range(len(messages)):
      messages[i] = "warning: %s" % (messages[i])
  else:
    messages = "warning: %s" % (messages)

  show(sys.stdout, messages, appendNewLineIfNeeded)

def parseOptions(
         value):
  return value.split()
