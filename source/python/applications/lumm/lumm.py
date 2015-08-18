#!/usr/bin/env python2.4

"""
This module contains the application class and the code for starting lumm.
"""

import ConfigParser, exceptions, optparse, os, random, sys, warnings
import binaryregression, independentvariables, \
         multinomialregression, splitdataset, taskrunner, utils


debug = False

if utils.developAccountKor():
  debug = True

# Filter out this warning.
warnings.filterwarnings(u"ignore",
         u"tempnam is a potential security risk to your program",
         RuntimeWarning)

class Lumm:
  """
  Main application class.
  """

  def __init__(self):
    """
    Constructor.

    Checks whether LUMMDIR environment variable is set.
    """

    self.__configurationFilename = None
    self.__configurationFileParser = None
    self.__tempDirectoryName = None
    self.__outputDirectoryName = None
    self.__logFilename = None
    self.__options = []
    self.__taskRunner = None
    self.__lummDirectoryName = None

    if u"LUMMDIR" in os.environ.keys():
      self.__lummDirectoryName = os.environ[u"LUMMDIR"]
    else:
      utils.showWarning("""\
LUMMDIR environment variable is not set!""")
      utils.showWarning("""\
We assume the directory of the configuration file contains the""")
      utils.showWarning("""\
LUMM software. This might not be the case. Set LUMMDIR to get rid""")
      utils.showWarning("""\
of this message.""")

    if utils.developAccountKor():
      developBinariesPath = u"/home/kor/pcrtree/bin/linux-develop"

      # Expand LD_LIBRARY_PATH.
      if not u"LD_LIBRARY_PATH" in os.environ.keys():
        os.environ[u"LD_LIBRARY_PATH"] = developBinariesPath
      else:
        os.environ[u"LD_LIBRARY_PATH"] = "%s:%s" % \
              (developBinariesPath, os.environ[u"LD_LIBRARY_PATH"])

      # Expand PATH.
      if not u"PATH" in os.environ.keys():
        os.environ[u"PATH"] = developBinariesPath
      else:
        os.environ[u"PATH"] = "%s:%s" % \
              (developBinariesPath, os.environ[u"PATH"])

  def __del__(self):
    """
    Destructor
    """

    # Make sure the tasks are destructed before removing the root temp
    # directory. Tasks create subdirs in the temp directory and remove these
    # when they are destructed.
    del self.__taskRunner
    assert os.path.exists(self.tempDirectoryName())
    os.rmdir(self.tempDirectoryName())

  def lummDirectoryName(self):
    return self.__lummDirectoryName

  def configurationFilename(self):
    return self.__configurationFilename

  def configurationFileParser(self):
    return self.__configurationFileParser

  def tempDirectoryName(self):
    return self.__tempDirectoryName

  def outputDirectoryName(self):
    return self.__outputDirectoryName

  def logFilename(self):
    return self.__logFilename

  def options(self):
    return self.__options

  def skipPreprocessing(self):
    return u"SkipPreprocessing" in self.__options

  def taskRunner(self):
    return self.__taskRunner

  def setDefaults(self,
         defaults,
         section):
    for key in defaults.keys():
      if not self.configurationFileParser().has_option(section, key):
        self.configurationFileParser().set(section, key, defaults[key])

  def expandEnvironmentVariables(self,
         section):
    for item in self.configurationFileParser().items(section):
      self.configurationFileParser().set(section,
         item[0], os.path.expandvars(item[1]))

  def parseCommandLine(self):
    parser = optparse.OptionParser()
    # parser.add_option("-d", "--debug", help="show stacktrace information")
    (options, args) = parser.parse_args()

    if len(args) != 1:
      parser.error(u"please provide one configuration filename")

    self.__configurationFilename = os.path.abspath(args[0])

    if not self.lummDirectoryName():
      self.__lummDirectoryName = os.path.dirname(self.configurationFilename())

    # Since the configuration file might contain relative path names we need
    # to chdir to that directory. Relative path names are relative to the
    # location of the configuration file.
    os.chdir(os.path.dirname(self.configurationFilename()))
    self.__configurationFilename = os.path.basename(
         self.configurationFilename())

  def parseConfigurationFile(self):
    self.__configurationFileParser = ConfigParser.SafeConfigParser()
    # Make the parser case sensitive.
    self.configurationFileParser().optionxform = str

    filenames = self.configurationFileParser().read(
         self.configurationFilename())

    if not filenames:
      raise exceptions.IOError(u"unable to read configuration file: %s" %
           self.configurationFilename())

    # Set missing options in section Default.
    section = u"DEFAULT"

    # Create defaults dictionary.
    defaults = {
      u"inputDirectory" : os.getcwd(),
      u"outputDirectory": os.getcwd(),
    }

    # Default section is always present (but might be empty).
    self.setDefaults(defaults, section)
    self.expandEnvironmentVariables(section)

    # for key in defaults.keys():
    #   if not self.configurationFileParser().has_option(section, key):
    #     self.configurationFileParser().set(section, key, defaults[key])

    # for item in self.configurationFileParser().items(section):
    #   self.configurationFileParser().set(section,
    #      item[0], os.path.expandvars(item[1]))

    section = u"Global"

    defaults = {
      u"tempDirectory"  : os.path.dirname(os.tempnam())
    }

    # Global section is not garanteed to be present.
    if not self.configurationFileParser().has_section(section):
      self.configurationFileParser().add_section(section)

    assert self.configurationFileParser().has_section(section)
    self.setDefaults(defaults, section)
    self.expandEnvironmentVariables(section)

    # for key in defaults.keys():
    #   if not self.configurationFileParser().has_option(section, key):
    #     self.configurationFileParser().set(section, key, defaults[key])

    # for item in self.configurationFileParser().items(section):
    #   self.configurationFileParser().set(section,
    #      item[0], os.path.expandvars(item[1]))

  def parseGlobalSections(self):
    # Allow for multiple processes to share the same temp directory by
    # appending a unique number to the temp directory given.
    # TODO in general check for paths given by the user whether upper level
    # TODO parts of the path exists.
    while not self.tempDirectoryName() or os.path.exists(self.tempDirectoryName()):
      self.__tempDirectoryName = os.path.join(
         self.configurationFileParser().get(u"Global", u"tempDirectory"), "%d" %
         (random.randint(0, 1000000)))

    os.mkdir(self.tempDirectoryName())

    if sys.platform == "win32":
      # Unset TMP because it results in paths which cannot be used with
      # pcraster (they contain a ~). See also docs of tempnam. Set TMP to our
      # own temp directory name.
      os.putenv("TMP", self.tempDirectoryName())

    self.__outputDirectoryName = self.configurationFileParser().get(
         u"DEFAULT", u"outputDirectory")

    if not os.path.exists(self.outputDirectoryName()):
      os.mkdir(self.outputDirectoryName())

    if self.configurationFileParser().has_option(u"Global", u"logFile"):
      self.__logFilename = self.configurationFileParser().get(
         u"Global", u"logFile")

    if self.configurationFileParser().has_option(u"Global", u"options"):
      self.__options = utils.parseOptions(self.configurationFileParser().get(
         u"Global", u"options"))

    supportedOptions = [u"SkipPreprocessing"]

    for option in self.options():
      if not option in supportedOptions:
        utils.raiseUnsupportedOptionException(option, u"Global")

  def createTaskList(self):
    self.__taskRunner = taskrunner.TaskRunner()

    if self.logFilename():
      self.taskRunner().setLogFilename(self.logFilename())

    assert self.configurationFileParser().has_section(u"Split Dataset")

    configurationItems = {};
    configurationItems["Split Dataset"] = \
         self.configurationFileParser().items(u"Split Dataset")
    splitDatasetTask = splitdataset.SplitDataset(
         self.lummDirectoryName(),
         tempDirectoryName=self.tempDirectoryName(),
         outputDirectoryName=self.outputDirectoryName(),
         configurationItems=configurationItems)
    if self.skipPreprocessing():
      splitDatasetTask.addOption(u"Skip")
    self.taskRunner().append(splitDatasetTask)

    if self.configurationFileParser().has_section(u"Independent Variables"):
      configurationItems = {};
      configurationItems["Independent Variables"] = \
           self.configurationFileParser().items(u"Independent Variables")
      independentVariables = independentvariables.IndependentVariables(
           self.lummDirectoryName(),
           tempDirectoryName=self.tempDirectoryName(),
           outputDirectoryName=self.outputDirectoryName(),
           configurationItems=configurationItems)
      independentVariables.setSplitMapFilename(
           splitDatasetTask.splitMapFilename())
      if self.skipPreprocessing():
        independentVariables.addOption(u"Skip")
      self.taskRunner().append(independentVariables)

    if self.configurationFileParser().has_section(u"Binary Regression"):
      configurationItems = {};
      configurationItems["Binary Regression"] = \
           self.configurationFileParser().items(u"Binary Regression")
      binaryRegression = binaryregression.BinaryRegression(
           self.lummDirectoryName(),
           tempDirectoryName=self.tempDirectoryName(),
           outputDirectoryName=self.outputDirectoryName(),
           configurationItems=configurationItems)
      binaryRegression.setLandUseMap1Filename(
           splitDatasetTask.landUseMap1Filename())
      binaryRegression.setLandUseMap2Filename(
           splitDatasetTask.landUseMap2Filename())
      binaryRegression.setSplitMapFilename(splitDatasetTask.splitMapFilename())
      binaryRegression.setIndependentVariablesTableBaseName(
           independentVariables.outputTableBaseName())
      binaryRegression.setCategoricalVariables(
           independentVariables.categoricalVariableNames())
      self.taskRunner().append(binaryRegression)

    if self.configurationFileParser().has_section(u"Multinomial Regression"):
      configurationItems = {};
      configurationItems["Multinomial Regression"] = \
           self.configurationFileParser().items(u"Multinomial Regression")
      multinomialRegression = multinomialregression.MultinomialRegression(
           self.lummDirectoryName(),
           tempDirectoryName=self.tempDirectoryName(),
           outputDirectoryName=self.outputDirectoryName(),
           configurationItems=configurationItems)
      multinomialRegression.setLandUseMap1Filename(
           splitDatasetTask.landUseMap1Filename())
      multinomialRegression.setLandUseMap2Filename(
           splitDatasetTask.landUseMap2Filename())
      multinomialRegression.setSplitMapFilename(splitDatasetTask.splitMapFilename())
      multinomialRegression.setIndependentVariablesTableBaseName(
           independentVariables.outputTableBaseName())
      multinomialRegression.setCategoricalVariables(
           independentVariables.categoricalVariableNames())
      self.taskRunner().append(multinomialRegression)

  def run(self):
    try:
      self.parseCommandLine()
      self.parseConfigurationFile()
      self.parseGlobalSections()
      self.createTaskList()
      self.taskRunner().initialise();
      self.taskRunner().run();
      self.taskRunner().clean();
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException(u"error while executing configuration file: %s" %
         (self.configurationFilename()), exception)



if __name__ == '__main__':
  status = 0

  try:
    Lumm().run()
  except AssertionError, exception:
    raise
  except Exception, exception:
    if debug:
      raise
    sys.stderr.write(str(exception) + u"\n")
    status = 1

  sys.exit(status)


# defaults = {
#          u"inputDirectory" : os.getcwd(),
#          u"outputDirectory": os.getcwd(),
#          u"tempDirectory"  : os.path.dirname(os.tempnam())}

  # utils.execute("""\
# /bin/ls                                                                                                                     out""")

