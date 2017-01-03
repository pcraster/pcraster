import copy, os, re, string
import constants, task, types, utils



class IndependentVariables(task.Task):
  """
  Class for the Independent Variables task.
  """

  def __init__(self,
         scriptDirectoryName,
         tempDirectoryName=None,
         outputDirectoryName=None,
         configurationItems=None):
    task.Task.__init__(self, u"Preprocess independent variables",
         scriptDirectoryName,
         tempDirectoryName=tempDirectoryName,
         outputDirectoryName=outputDirectoryName,
         configurationItems=configurationItems)
    self.__categoricalVariables = None
    self.__continuousVariables = None
    self.__modelTableFilename = None
    self.__validationTableFilename = None
    self.__splitMapFilename = None
    self.__percentageUnchangedCellsInModelSampleSet = None

    self.setSectionTitle(u"Independent Variables")

    # TODO either one should be present, they are not both required.
    self.testRequiredOption(u"categoricalVariables")
    self.testRequiredOption(u"continuousVariables")
    # self.testRequiredOption(u"validationTable")
    # self.testRequiredOption(u"modelTable")

    self.__categoricalVariables = \
         self.option(u"categoricalVariables").split()

    # Parse the selection specification. This list will contain lists whos
    # first element is the filename and the second might contain a list of
    # selected classes, or None if no specification is given. Later on a third
    # element might be added with the filename of the reclassified variable
    # (in case a selection specification was given).
    for i in range(len(self.__categoricalVariables)):
      self.__categoricalVariables[i] = \
         self.splitCategoricalVariableName(self.__categoricalVariables[i])
      assert len(self.__categoricalVariables[i]) == 3

    self.__continuousVariables = \
         self.option(u"continuousVariables").split()
    self.__modelTableFilename = os.path.join(
         self.modelOutputDirectoryName(),
         self.outputTableBaseName())
         # self.option(u"modelTable"))
    self.__validationTableFilename = os.path.join(
         self.validationOutputDirectoryName(),
         self.outputTableBaseName())
         # self.option(u"validationTable"))

    if self.hasOption(u"modelTable"):
      self.showDeprecationWarning(
         "modelTable in section %s is not used anymore" %
         (self.sectionTitle()))

    if self.hasOption(u"validationTable"):
      self.showDeprecationWarning(
         "validationTable in section %s is not used anymore" %
         (self.sectionTitle()))

  def categoricalVariables(self):
    return self.__categoricalVariables

  def categoricalVariableNames(self):
    result = []

    for list in self.categoricalVariables():
      assert len(list) == 3
      if not list[1]:
        # No selection specification, use original classes.
        result.append(list[0])
      else:
        # Selection specification, use reclassified map.
        result.append(list[2])

    return result

  def continuousVariables(self):
    return self.__continuousVariables

  def variables(self):
    return self.categoricalVariableNames() + self.continuousVariables()

  def outputTableBaseName(self):
    return "independent.col"

  def modelTableFilename(self):
    return self.__modelTableFilename

  def validationTableFilename(self):
    return self.__validationTableFilename

  def setSplitMapFilename(self, filename):
    assert filename
    # assume this is ok
    self.__splitMapFilename = filename

  def splitMapFilename(self):
    return self.__splitMapFilename

  # def setPercentageUnchangedCellsInModelSampleSet(self, percentage):
  #   assert percentage > 0.0 and percentage <= 1.0
  #   self.__percentageUnchangedCellsInModelSampleSet = percentage

  # def percentageUnchangedCellsInModelSampleSet(self):
  #   return self.__percentageUnchangedCellsInModelSampleSet

  # def rScriptFilename(self):
  #   # FIXME configured output dir gebruiken
  #   return os.path.join("outputs", "lumm.R")

  def splitCategoricalVariableName(self,
         variable):
    """
    This function returns a list of the filename of the variable, a list of
    selected classes and a filename for the map with selected classes if
    variable contains a selection specification. If not, the second and third
    elements of the tuple will be None.
    """

    filename = variable
    selection = None
    selectionMapFilename = None

    pattern = re.compile("\A([^{]+)[{]((?:[0-9]+,)*[0-9]+)[}]?\Z")
    match = pattern.match(variable)

    if match:
      groups = pattern.match(variable).groups()

      if len(groups) > 0:
        filename = groups[0]

      if len(groups) > 1:
        selection = groups[1].split(",")

    if selection:
      # Create a name for the new raster. In createSelectionMaps this raster
      # will be created.
      selectionMapFilename = os.path.join(self.tempDirectoryName(),
         "%s.%s" % (os.path.basename(filename), ".".join(selection)))

    return [filename, selection, selectionMapFilename]

  def testVariablesOptions(self):
    try:
      for tuple in self.categoricalVariables():
        utils.testFileIsReadable(tuple[0])
        utils.testRasterIsCategorical(tuple[0])
      for variable in self.continuousVariables():
        utils.testFileIsReadable(variable)
        utils.testRasterIsContinuous(variable)
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseInvalidOptionException(u"variables", exception)

  # def testTableOptions(self):
  #   try:
  #     utils.testFileIsWritable(self.validationTableFilename())
  #     utils.testFileIsWritable(self.modelTableFilename())
  #   except Exception, exception:
  #     utils.raiseInvalidOptionException(u"table", exception)

  def initialise(self):
    self.decorateOptionException(self.testVariablesOptions)
    # self.decorateOptionException(self.testTableOptions)

  def validationSampleMapFilename(self, variable):
    return os.path.join(self.validationTempDirectoryName(),
         os.path.basename(variable))

  def modelSampleMapFilename(self, variable):
    return os.path.join(self.modelTempDirectoryName(),
         os.path.basename(variable))


  def createSelectionMaps(self):
    """
    Each categorical variable can contain a selection specification. If so,
    create a copy of the variable which contains the selected classes. Not
    selected classes get a value -9999.
    """

    for variable in self.categoricalVariables():
      assert isinstance(variable, types.ListType)
      assert len(variable) == 3

      if variable[1]:
        assert not os.path.exists(variable[2])

        # Reclassify the original map.
        selection = []
        for classId in variable[1]:
          selection.append("\"%s\" == %s" % (variable[0], classId))

        script = """\
binding
  input = "%s";
  result = "%s";

initial
  report result = if(%s, input, nominal(-9999));
""" % (variable[0], variable[2], " or ".join(selection))

        try:
          utils.executePCRCalcScript(script, logFilename=self.logFilename())
        except AssertionError, exception:
          raise
        except Exception, exception:
          utils.raiseException(u"error reclassifying classification variable",
              exception)

        assert os.path.exists(variable[2])

  def removeSelectionMaps(self):
    # If a categorical variable was reclassified because a selection
    # specification was given we remove the created map here.
    for variable in self.categoricalVariables():
      if variable[1]:
        # Delete the map.
        assert len(variable) == 3
        assert os.path.exists(variable[2])
        os.remove(variable[2])

  def createSampleMaps(self):
    assert os.path.exists(self.splitMapFilename())

    for variable in self.variables():
      validationStatement = "%s = if(%s == %d, %s)" % \
         (utils.filenameToPCRasterStatement(self.validationSampleMapFilename(variable)),
         utils.filenameToPCRasterStatement(self.splitMapFilename()),
         constants.validationSetClass,
         utils.filenameToPCRasterStatement(variable))
      modelStatement = "%s = if(%s == %d, %s)" % \
         (utils.filenameToPCRasterStatement(self.modelSampleMapFilename(variable)),
         utils.filenameToPCRasterStatement(self.splitMapFilename()),
         constants.modelSetSampleClass,
         utils.filenameToPCRasterStatement(variable))
      try:
        utils.executePCRCalc(validationStatement, logFilename=self.logFilename())
        utils.executePCRCalc(modelStatement, logFilename=self.logFilename())
      except AssertionError, exception:
        raise
      except Exception, exception:
        utils.raiseException(u"error drawing sample for %s" %
           (variable), exception)

  def removeSampleMaps(self):
    for variable in self.variables():
      try:
        os.remove(self.validationSampleMapFilename(variable))
        os.remove(self.modelSampleMapFilename(variable))
      except AssertionError, exception:
        raise
      except Exception, exception:
        utils.raiseException(u"error removing sample map", exception)

  def createTables(self):

    currentWorkingDirectory = os.getcwdu()

    try:
      os.chdir(self.validationTempDirectoryName())
      utils.executeMap2Col("--unittrue --coorcentre -g %s %s" %
         (" ".join(map(os.path.basename,
         self.variables())), self.validationTableFilename()), logFilename=self.logFilename())
      os.chdir(self.modelTempDirectoryName())
      utils.executeMap2Col("--unittrue --coorcentre -g %s %s" %
         (" ".join(map(os.path.basename,
         self.variables())), self.modelTableFilename()), logFilename=self.logFilename())
    except AssertionError, exception:
      raise
    except Exception, exception:
      os.chdir(currentWorkingDirectory)
      utils.raiseException(u"error creating independent variables table",
         exception)

    os.chdir(currentWorkingDirectory)

    assert os.path.exists(self.validationTableFilename())
    assert os.path.exists(self.modelTableFilename())

  # def createRScript(self):
  #   scriptFile = file(self.rScriptFilename(), "w")

  #   def quote(string):
  #     return "\"%s\"" % (string)

  #   scriptFile.write("""\
  # catVars = c(%s)
  # percentageUnchangedCellsInModelSampleSet = %g
  # """ % (" ".join(map(quote, self.categoricalVariables())),
  #        self.percentageUnchangedCellsInModelSampleSet()))

  def run(self):
    if not self.optionIsSet("Skip"):
      self.createSelectionMaps()
      self.createSampleMaps()
      self.createTables()
      # self.createRScript()
      self.removeSampleMaps()
      self.removeSelectionMaps()
