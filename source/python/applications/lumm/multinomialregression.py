import constants, os, regression, sys, task, utils



class MultinomialRegression(regression.Regression):
  """
  Base class for the MultinomialRegression task.
  """

  def __init__(self,
         scriptDirectoryName,
         tempDirectoryName=None,
         outputDirectoryName=None,
         configurationItems=None):
    regression.Regression.__init__(self,
         u"Perform multinomial regression",
         scriptDirectoryName,
         tempDirectoryName=tempDirectoryName,
         outputDirectoryName=outputDirectoryName,
         sectionTitle=u"Multinomial Regression",
         configurationItems=configurationItems)

  def baseWithoutExtOfDependentVariable(self):
    result = os.path.splitext(os.path.basename(self.landUseMap2Filename()))[0]
    assert len(result)
    assert result[-1:] != "."

    return result

  def mapFilename(self):
    return self.baseWithoutExtOfDependentVariable() + ".pcrmap"

  def tableFilename(self):
    return self.baseWithoutExtOfDependentVariable() + ".eas"

  def modelSampleMapFilename(self):
    return os.path.join(self.modelTempDirectoryName(), self.mapFilename())

  def validationSampleMapFilename(self):
    return os.path.join(self.validationTempDirectoryName(), self.mapFilename())

  def modelSampleTableFilename(self):
    return os.path.join(self.modelOutputDirectoryName(), self.tableFilename())

  def validationSampleTableFilename(self):
    return os.path.join(self.validationOutputDirectoryName(), self.tableFilename())

  def regressionScriptFilename(self):
    return os.path.join(self.outputDirectoryName(), u"multinomialRegressionConfig.R")

  def createMaps(self):
    assert not os.path.exists(self.modelSampleMapFilename())
    assert not os.path.exists(self.validationSampleMapFilename())

    statements = []

    selection = []
    for classId in self.classes():
      selection.append("\"%s\" == %s" % (self.landUseMap2Filename(), classId))

    # Model.
    statements.append(
       "%s = if(%s == %d, if(%s != %s and (%s), %s, nominal(-9999)))" %
       (utils.filenameToPCRasterStatement(self.modelSampleMapFilename()),
       utils.filenameToPCRasterStatement(self.splitMapFilename()),
       constants.modelSetSampleClass,
       utils.filenameToPCRasterStatement(self.landUseMap1Filename()),
       utils.filenameToPCRasterStatement(self.landUseMap2Filename()),
       " or ".join(selection),
       utils.filenameToPCRasterStatement(self.landUseMap2Filename())))

    # Validation.
    statements.append(
       "%s = if(%s == %d, if(%s != %s and (%s), %s, nominal(-9999)))" %
       (utils.filenameToPCRasterStatement(self.validationSampleMapFilename()),
       utils.filenameToPCRasterStatement(self.splitMapFilename()),
       constants.validationSetClass,
       utils.filenameToPCRasterStatement(self.landUseMap1Filename()),
       utils.filenameToPCRasterStatement(self.landUseMap2Filename()),
       " or ".join(selection),
       utils.filenameToPCRasterStatement(self.landUseMap2Filename())))

    try:
      for statement in statements:
        utils.executePCRCalc("%s" % (statement), logFilename=self.logFilename())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException(u"error creating dependent variable map", exception)

    assert os.path.exists(self.modelSampleMapFilename())
    assert os.path.exists(self.validationSampleMapFilename())

  def removeMaps(self):
    os.remove(self.modelSampleMapFilename())
    os.remove(self.validationSampleMapFilename())

  def createTables(self):
    # currentWorkingDirectory = os.getcwdu()

    try:
      try:
        # Model.
        # os.chdir(self.modelOutputDirectoryName())
        utils.executeMap2Col("--unittrue --coorcentre -g %s %s" %
           (self.modelSampleMapFilename(),
           self.modelSampleTableFilename()), logFilename=self.logFilename())

        # Validation.
        # os.chdir(self.validationOutputDirectoryName())
        utils.executeMap2Col("--unittrue --coorcentre -g %s %s" %
           (self.validationSampleMapFilename(),
           self.validationSampleTableFilename()), logFilename=self.logFilename())
      except AssertionError, exception:
        raise
      except Exception, exception:
        utils.raiseException(u"error creating dependent variables table",
              exception)
    finally:
      # os.chdir(currentWorkingDirectory)
      pass

    assert os.path.exists(self.modelSampleTableFilename())
    assert os.path.exists(self.validationSampleTableFilename())

  def removeTables(self):
    os.remove(self.modelSampleTableFilename())
    os.remove(self.validationSampleTableFilename())

  def performRegression(self):

    def quoteBasename(string):
      return u"\"%s\"" % (os.path.basename(string))

    # Default is no.
    reportSummary = "FALSE"

    if self.hasOption(u"options"):
      if self.optionIsSet(u"ReportSummary"):
        reportSummary = "TRUE"

    script = u"""\
modelSetDirectory = "%s"
validationSetDirectory = "%s"
independentVariables = "%s"
categoricalVariables = c(%s)
dependentVariable = "%s"
fractionUnchangedCellsInModelSampleSet = %g
fractionChangedCellsInModelSampleSet = %g
summaryFile = "%s"
stepwise = "forward"
reportSummary = %s
""" % (    self.modelOutputDirectoryName(),
           self.validationOutputDirectoryName(),
           self.independentVariablesTableBaseName(),
           u", ".join(map(quoteBasename, self.categoricalVariables())),
           self.tableFilename(),
           self.fractionOfUnchangedCellsInModelSetSample(),
           self.fractionOfChangedCellsInModelSetSample(),
           u"multinomialregression.Rout",
           reportSummary)

    for name in [u"variogramBinWidth", u"variogramCutoff",
       u"maxNumberOfSteps"]:
      if self.hasOption(name):
        script += "%s = %s\n" % (name, self.option(name))

    if sys.platform == "win32":
      script = script.replace("\\", "\\\\")

    file(self.regressionScriptFilename(), "w").write(script)

    scriptFilename = os.path.join(self.scriptDirectoryName(),
         u"multinomialRegression.R")
    assert os.path.exists(scriptFilename)

    currentWorkingDirectory = os.getcwdu()
    outputFilename = u"multinomialregression_messages.Rout"

    try:
      os.chdir(self.outputDirectoryName())
      utils.executeRScript(scriptFilename, outputFilename,
         logFilename=self.logFilename())
    finally:
      os.chdir(currentWorkingDirectory)

    assert os.path.exists(self.regressionScriptFilename())
    os.remove(self.regressionScriptFilename())

  def run(self):
    self.determineFractionsInModelSetSample()
    self.createMaps()
    self.createTables()
    self.performRegression()
    self.removeTables()
    self.removeMaps()
