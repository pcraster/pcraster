import os, string, sys
import constants, task, utils



class Regression(task.Task):
  """
  Base class for the binaryregression.BinaryRegression and multinomialregression.MultinomialRegression tasks.
  """

  def __init__(self,
         description,
         scriptDirectoryName,
         tempDirectoryName=None,
         outputDirectoryName=None,
         sectionTitle=None,
         configurationItems=None):
    task.Task.__init__(self,
         description,
         scriptDirectoryName,
         tempDirectoryName=tempDirectoryName,
         outputDirectoryName=outputDirectoryName,
         configurationItems=configurationItems)
    self.__classes = None
    self.__splitMapFilename = None
    self.__landUseMap1Filename = None
    self.__landUseMap2Filename = None
    self.__independentVariablesTableBaseName = None
    self.__categoricalVariables = None
    self.__fractionUnchangedCellsInModelSetSample = None
    self.__fractionChangedCellsInModelSetSample = None

    self.setSectionTitle(sectionTitle)

    if self.hasOption(u"options"):
      self.parseOptions()

    self.testRequiredOption(u"classes")
    self.__classes = self.option(u"classes").split()

    if self.hasOption(u"modelTable"):
      self.showDeprecationWarning(
         "modelTable in section %s is not used anymore" %
         (self.sectionTitle()))

    if self.hasOption(u"validationTable"):
      self.showDeprecationWarning(
         "validationTable in section %s is not used anymore" %
         (self.sectionTitle()))

  def classes(self):
    return self.__classes

  # def outputTableBaseName(self):
  #   return "binaryregression.col"

  def modelTableFilename(self):
    return self.__modelTableFilename

  # def validationTableFilename(self):
  # return self.__validationTableFilename

  def landUseMap1Filename(self):
    return self.__landUseMap1Filename

  def setLandUseMap1Filename(self, filename):
    assert filename
    self.__landUseMap1Filename = filename

  def landUseMap2Filename(self):
    return self.__landUseMap2Filename

  def setLandUseMap2Filename(self, filename):
    assert filename
    self.__landUseMap2Filename = filename

  def independentVariablesTableBaseName(self):
    return self.__independentVariablesTableBaseName

  def setIndependentVariablesTableBaseName(self, filename):
    assert filename
    # assume this is ok
    self.__independentVariablesTableBaseName = filename

  def setSplitMapFilename(self, filename):
    assert filename
    # Assume this is ok.
    self.__splitMapFilename = filename

  def splitMapFilename(self):
    return self.__splitMapFilename

  def setCategoricalVariables(self, variables):
    self.__categoricalVariables = variables

  def categoricalVariables(self):
    return self.__categoricalVariables

  def fractionOfUnchangedCellsInModelSetSample(self):
    """
    @return: Fraction of unchanged cells in the model set sample.

    Return value is only valid after calling
    L{determineFractionsInModelSetSample}.
    """
    return self.__fractionUnchangedCellsInModelSetSample

  def fractionOfChangedCellsInModelSetSample(self):
    """
    @return: Fraction of changed cells in the model set sample.

    Return value is only valid after calling
    L{determineFractionsInModelSetSample}.
    """
    return self.__fractionChangedCellsInModelSetSample

  def testClass(self, aClass):
    try:
      utils.testStringIsIntegral(aClass)
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException(u"invalid class: %s" % (aClass), exception)

  def testClassesOption(self):
    try:
      for aClass in self.__classes:
        self.testClass(aClass)
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseInvalidOptionException(u"classes", exception)

  # def testTableOptions(self):
  #   try:
  #     utils.testFileIsWritable(self.modelTableFilename())
  #     utils.testFileIsWritable(self.validationTableFilename())
  #   except Exception, exception:
  #     utils.raiseInvalidOptionException(u"binary regression table", exception)

  def initialise(self):
    self.decorateOptionException(self.testClassesOption)
    # self.decorateOptionException(self.testTableOptions)

  def determineFractionsInModelSetSample(self):
    """
    Determines the fractions of cells in the model set which have changed and
    cells which have not changed compared to the model set.

    The model set sample is a subset of the model set. For the regression
    analyses it is important to now the fraction between the amount of cells
    which have changed in the model set sample compared to the ones in the
    total model set. The same goes for the unchanged cells.

    The way the sample of the model set is taken changes the fraction of
    changed to unchanged cells. The regression analyses needs to compensate
    for this.
    """
    fractionUnchangedCellsRasterFilename = os.tempnam(self.tempDirectoryName())
    fractionChangedCellsRasterFilename = os.tempnam(self.tempDirectoryName())
    script = u"""\
binding
  split = "%s";
  modelSetClass = %d;
  modelSetSampleClass = %d;
  landUse1 = "%s";
  landUse2 = "%s";
  fractionUnchangedCells = "%s";
  fractionChangedCells = "%s";

initial
  modelSetSample = split == modelSetSampleClass;
  modelSet = split == modelSetClass or split == modelSetSampleClass;

  unchangedCells = landUse1 == landUse2;
  unchangedCellsInModelSetSample = modelSetSample and unchangedCells;
  unchangedCellsInModelSet = modelSet and unchangedCells;

  changedCells = landUse1 != landUse2;
  changedCellsInModelSetSample = modelSetSample and changedCells;
  changedCellsInModelSet = modelSet and changedCells;

  # Use if to make false cells missing values.
  report fractionUnchangedCells = maparea(if(unchangedCellsInModelSetSample, boolean(1))) / maparea(if(unchangedCellsInModelSet, boolean(1)));
  report fractionChangedCells = maparea(if(changedCellsInModelSetSample, boolean(1))) / maparea(if(changedCellsInModelSet, boolean(1)));
""" % (self.splitMapFilename(),
         constants.modelSetClass,
         constants.modelSetSampleClass,
         self.landUseMap1Filename(), self.landUseMap2Filename(),
         fractionUnchangedCellsRasterFilename,
         fractionChangedCellsRasterFilename)

    try:
      utils.executePCRCalcScript(script, logFilename=self.logFilename())

      def determineFraction(resultFilename):
        assert os.path.exists(resultFilename)
        result = utils.rasterMaximum(resultFilename)
        assert result != "mv"
        result = float(result)
        utils.testNumberWithinRange(result, 0.0, 1.0)
        return result

      self.__fractionUnchangedCellsInModelSetSample = determineFraction(
         fractionUnchangedCellsRasterFilename)
      os.remove(fractionUnchangedCellsRasterFilename)

      self.__fractionChangedCellsInModelSetSample = determineFraction(
         fractionChangedCellsRasterFilename)
      os.remove(fractionChangedCellsRasterFilename)
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException(u"error determining fractions in model set sample", exception)

    self.writeLog("Fraction of unchanged cells in model set sample: %g\n" %
         (self.fractionOfUnchangedCellsInModelSetSample()))
    self.writeLog("Fraction of changed cells in model set sample: %g\n" %
         (self.fractionOfChangedCellsInModelSetSample()))
