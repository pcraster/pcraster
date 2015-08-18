import os, string, sys
import constants, regression, utils



class BinaryRegression(regression.Regression):
  """
  Base class for the BinaryRegression task.
  """

  def __init__(self,
         scriptDirectoryName,
         tempDirectoryName=None,
         outputDirectoryName=None,
         configurationItems=None):
    regression.Regression.__init__(self,
         u"Perform binary regression",
         scriptDirectoryName,
         tempDirectoryName=tempDirectoryName,
         outputDirectoryName=outputDirectoryName,
         sectionTitle=u"Binary Regression",
         configurationItems=configurationItems)

  def classNumberToBirthMapFilename(self,
         aClass):
    return "birth%s.pcrmap" % (aClass)

  def classNumberToDeathMapFilename(self,
         aClass):
    return "death%s.pcrmap" % (aClass)

  def classNumberToDeathTableFilename(self,
         aClass):
    return "death%s.col" % (aClass)

  def classNumberToBirthTableFilename(self,
         aClass):
    return "birth%s.col" % (aClass)

  def classNumberToDeathValidationTableFilename(self,
         aClass):
    return os.path.join(self.validationOutputDirectoryName(),
         "death%s.col" % (aClass))

  def classNumberToBirthValidationTableFilename(self,
         aClass):
    return os.path.join(self.validationOutputDirectoryName(),
         "birth%s.col" % (aClass))

  def classNumberToDeathModelTableFilename(self,
         aClass):
    return os.path.join(self.modelOutputDirectoryName(),
         "death%s.col" % (aClass))

  def classNumberToBirthModelTableFilename(self,
         aClass):
    return os.path.join(self.modelOutputDirectoryName(),
         "birth%s.col" % (aClass))

  def validationSampleBirthMapFilename(self, aClass):
    return os.path.join(self.validationTempDirectoryName(),
         self.classNumberToBirthMapFilename(aClass))

  def validationSampleDeathMapFilename(self, aClass):
    return os.path.join(self.validationTempDirectoryName(),
         self.classNumberToDeathMapFilename(aClass))

  def modelSampleBirthMapFilename(self, aClass):
    return os.path.join(self.modelTempDirectoryName(),
         self.classNumberToBirthMapFilename(aClass))

  def modelSampleDeathMapFilename(self, aClass):
    return os.path.join(self.modelTempDirectoryName(),
         self.classNumberToDeathMapFilename(aClass))

  def changeMapFilename(self):
    return "luchanged.pcrmap"

  def changeTableFilename(self):
    return "luchanged.col"

  def modelSampleChangeMapFilename(self):
    return os.path.join(self.modelTempDirectoryName(),
         self.changeMapFilename())

  def validationChangeMapFilename(self):
    return os.path.join(self.validationTempDirectoryName(),
         self.changeMapFilename())

  def modelSampleChangeTableFilename(self):
    return os.path.join(self.modelOutputDirectoryName(),
         self.changeTableFilename())

  def validationChangeTableFilename(self):
    return os.path.join(self.validationOutputDirectoryName(),
         self.changeTableFilename())

  def createChangeMaps(self):
    statements = []
    statements.append(
         "%s = if(%s == %d, if(%s != %s, boolean(1), 0))" %
         (utils.filenameToPCRasterStatement(self.modelSampleChangeMapFilename()),
         utils.filenameToPCRasterStatement(self.splitMapFilename()),
         constants.modelSetSampleClass,
         utils.filenameToPCRasterStatement(self.landUseMap1Filename()),
         utils.filenameToPCRasterStatement(self.landUseMap2Filename())))
    statements.append(
         "%s = if(%s == %d, if(%s != %s, boolean(1), 0))" %
         (utils.filenameToPCRasterStatement(self.validationChangeMapFilename()),
         utils.filenameToPCRasterStatement(self.splitMapFilename()),
         constants.validationSetClass,
         utils.filenameToPCRasterStatement(self.landUseMap1Filename()),
         utils.filenameToPCRasterStatement(self.landUseMap2Filename())))

    try:
      for statement in statements:
        utils.executePCRCalc("%s" % (statement), logFilename=self.logFilename())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException("error creating change map", exception)

    assert os.path.exists(self.modelSampleChangeMapFilename())
    assert os.path.exists(self.validationChangeMapFilename())

  def removeChangeMaps(self):
    os.remove(self.modelSampleChangeMapFilename())
    os.remove(self.validationChangeMapFilename())

  def createChangeTables(self):
    # currentWorkingDirectory = os.getcwdu()

    try:
      try:
        # Model.
        # os.chdir(self.modelTempDirectoryName())
        utils.executeMap2Col("--unittrue --coorcentre -g %s %s" %
           (self.modelSampleChangeMapFilename(),
           self.modelSampleChangeTableFilename()),
           logFilename=self.logFilename())

        # Validation.
        # os.chdir(self.validationTempDirectoryName())
        utils.executeMap2Col("--unittrue --coorcentre -g %s %s" %
           (self.validationChangeMapFilename(),
           self.validationChangeTableFilename()),
           logFilename=self.logFilename())

      except AssertionError, exception:
        raise
      except Exception, exception:
        utils.raiseException(u"error creating change table", exception)
    finally:
      # os.chdir(currentWorkingDirectory)
      pass

    assert os.path.exists(self.modelSampleChangeTableFilename())
    assert os.path.exists(self.validationChangeTableFilename())

  def removeChangeTables(self):
    os.remove(self.modelSampleChangeTableFilename())
    os.remove(self.validationChangeTableFilename())

  def createMapsPerClass(self, aClass):
    statements = []

    # Model, birth.
    statements.append(
       "%s = if(%s == %d, if(%s != %s and %s == %s, boolean(1), 0))" %
       (utils.filenameToPCRasterStatement(self.modelSampleBirthMapFilename(aClass)),
       utils.filenameToPCRasterStatement(self.splitMapFilename()),
       constants.modelSetSampleClass,
       utils.filenameToPCRasterStatement(self.landUseMap1Filename()), aClass,
       utils.filenameToPCRasterStatement(self.landUseMap2Filename()), aClass))

    # Model, death.
    statements.append(
       "%s = if(%s == %d, if(%s == %s and %s != %s, boolean(1), 0))" %
       (utils.filenameToPCRasterStatement(self.modelSampleDeathMapFilename(aClass)),
       utils.filenameToPCRasterStatement(self.splitMapFilename()),
       constants.modelSetSampleClass,
       utils.filenameToPCRasterStatement(self.landUseMap1Filename()), aClass,
       utils.filenameToPCRasterStatement(self.landUseMap2Filename()), aClass))

    # Validation, birth.
    statements.append(
       "%s = if(%s == %d, if(%s != %s and %s == %s, boolean(1), 0))" %
       (utils.filenameToPCRasterStatement(self.validationSampleBirthMapFilename(aClass)),
       utils.filenameToPCRasterStatement(self.splitMapFilename()),
       constants.validationSetClass,
       utils.filenameToPCRasterStatement(self.landUseMap1Filename()), aClass,
       utils.filenameToPCRasterStatement(self.landUseMap2Filename()), aClass))

    # Validation, death.
    statements.append(
       "%s = if(%s == %d, if(%s == %s and %s != %s, boolean(1), 0))" %
       (utils.filenameToPCRasterStatement(self.validationSampleDeathMapFilename(aClass)),
       utils.filenameToPCRasterStatement(self.splitMapFilename()),
       constants.validationSetClass,
       utils.filenameToPCRasterStatement(self.landUseMap1Filename()), aClass,
       utils.filenameToPCRasterStatement(self.landUseMap2Filename()), aClass))

    try:
      for statement in statements:
        utils.executePCRCalc("%s" % (statement), logFilename=self.logFilename())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException("error creating dependent variable map", exception)

  def removeMapsPerClass(self, aClass):
    os.remove(self.validationSampleBirthMapFilename(aClass))
    os.remove(self.validationSampleDeathMapFilename(aClass))
    os.remove(self.modelSampleBirthMapFilename(aClass))
    os.remove(self.modelSampleDeathMapFilename(aClass))

  def createTablesPerClass(self, aClass):
    currentWorkingDirectory = os.getcwdu()

    try:
      try:
        # Model, birth.
        os.chdir(self.modelTempDirectoryName())
        utils.executeMap2Col("--unittrue --coorcentre -g %s %s" %
           (self.classNumberToBirthMapFilename(aClass),
           self.classNumberToBirthModelTableFilename(aClass)),
           logFilename=self.logFilename())

        # Model, death.
        utils.executeMap2Col("--unittrue --coorcentre -g %s %s" %
           (self.classNumberToDeathMapFilename(aClass),
           self.classNumberToDeathModelTableFilename(aClass)),
           logFilename=self.logFilename())

        # Validation, birth.
        os.chdir(self.validationTempDirectoryName())
        utils.executeMap2Col("--unittrue --coorcentre -g %s %s" %
           (self.classNumberToBirthMapFilename(aClass),
           self.classNumberToBirthValidationTableFilename(aClass)),
           logFilename=self.logFilename())

        # Validation, death.
        utils.executeMap2Col("--unittrue --coorcentre -g %s %s" %
           (self.classNumberToDeathMapFilename(aClass),
           self.classNumberToDeathValidationTableFilename(aClass)),
           logFilename=self.logFilename())

      except AssertionError, exception:
        raise
      except Exception, exception:
        utils.raiseException(u"error creating dependent variables table",
              exception)
    finally:
      os.chdir(currentWorkingDirectory)

    assert os.path.exists(self.classNumberToBirthModelTableFilename(aClass))
    assert os.path.exists(self.classNumberToDeathModelTableFilename(aClass))
    assert os.path.exists(self.classNumberToBirthValidationTableFilename(aClass))
    assert os.path.exists(self.classNumberToDeathValidationTableFilename(aClass))

  def removeTablesPerClass(self,
         aClass):
    assert os.path.exists(self.classNumberToBirthModelTableFilename(aClass))
    assert os.path.exists(self.classNumberToDeathModelTableFilename(aClass))
    assert os.path.exists(self.classNumberToBirthValidationTableFilename(aClass))
    assert os.path.exists(self.classNumberToDeathValidationTableFilename(aClass))

    os.remove(self.classNumberToBirthModelTableFilename(aClass))
    os.remove(self.classNumberToDeathModelTableFilename(aClass))
    os.remove(self.classNumberToBirthValidationTableFilename(aClass))
    os.remove(self.classNumberToDeathValidationTableFilename(aClass))

  def regressionScriptFilename(self):
    return os.path.join(self.outputDirectoryName(), u"binaryRegressionConfig.R")

  def performRegressionsPerClass(self, aClass):

    def quoteBasename(string):
      return u"\"%s\"" % (os.path.basename(string))

    def script(
         dependentVariableTableFilename,
         summaryFile):

      # Default is no.
      reportSummary = "FALSE"

      if self.hasOption(u"options"):
        if self.optionIsSet(u"ReportSummary"):
          reportSummary = "TRUE"

      result = u"""\
modelSetDirectory = "%s"
validationSetDirectory = "%s"
independentVariables = "%s"
categoricalVariables = c(%s)
dependentVariable = "%s"
landUseChanged = "%s"
fractionUnchangedCellsInModelSampleSet = %g
fractionChangedCellsInModelSampleSet = %g
summaryFile = "%s"
stepwise = "forward"
reportSummary = %s
""" % (  self.modelOutputDirectoryName(),
         self.validationOutputDirectoryName(),
         self.independentVariablesTableBaseName(),
         u", ".join(map(quoteBasename, self.categoricalVariables())),
         dependentVariableTableFilename,
         self.changeTableFilename(),
         self.fractionOfUnchangedCellsInModelSetSample(),
         self.fractionOfChangedCellsInModelSetSample(),
         summaryFile,
         reportSummary)

      for name in [u"variogramBinWidth", u"variogramCutoff",
         u"maxNumberOfSteps"]:
        if self.hasOption(name):
          result += "%s = %s\n" % (name, self.option(name))

      return result

    # Death
    aScript = script(self.classNumberToDeathTableFilename(aClass),
         u"binaryregression_death%s.Rout" % (aClass))

    if sys.platform == "win32":
      aScript = aScript.replace("\\", "\\\\")

    file(self.regressionScriptFilename(), "w").write(aScript)

    # TODO Test the existence of this script in the initialise function instead
    # TODO of asserting it here.
    scriptFilename = os.path.join(self.scriptDirectoryName(),
         u"binaryRegression.R")
    assert os.path.exists(scriptFilename)

    currentWorkingDirectory = os.getcwdu()

    try:
      outputFilename = u"binaryregression_death%s_messages.Rout" % (aClass)
      os.chdir(self.outputDirectoryName())
      utils.executeRScript(scriptFilename, outputFilename, logFilename=self.logFilename())
    finally:
      os.chdir(currentWorkingDirectory)

    assert os.path.exists(self.regressionScriptFilename())
    os.remove(self.regressionScriptFilename())

    # Birth
    aScript = script(self.classNumberToBirthTableFilename(aClass),
         u"binaryregression_birth%s.Rout" % (aClass))

    if sys.platform == "win32":
      aScript = aScript.replace("\\", "\\\\")

    file(self.regressionScriptFilename(), "w").write(aScript)

    scriptFilename = os.path.join(self.scriptDirectoryName(),
         u"binaryRegression.R")
    assert os.path.exists(scriptFilename)

    currentWorkingDirectory = os.getcwdu()

    try:
      outputFilename = u"binaryregression_birth%s_messages.Rout" % (aClass)
      os.chdir(self.outputDirectoryName())
      utils.executeRScript(scriptFilename, outputFilename, logFilename=self.logFilename())
    finally:
      os.chdir(currentWorkingDirectory)

    assert os.path.exists(self.regressionScriptFilename())
    os.remove(self.regressionScriptFilename())

  def run(self):
    for aClass in self.classes():
      self.determineFractionsInModelSetSample()
      self.createChangeMaps()
      self.createChangeTables()
      self.createMapsPerClass(aClass)
      self.createTablesPerClass(aClass)
      self.performRegressionsPerClass(aClass)
      self.removeTablesPerClass(aClass)
      self.removeMapsPerClass(aClass)
      self.removeChangeTables()
      self.removeChangeMaps()
