import os
import constants, task, utils



class SplitDataset(task.Task):
  """
  Class for the Split Dataset task.
  """

  def __init__(self,
         scriptDirectoryName,
         tempDirectoryName=None,
         outputDirectoryName=None,
         configurationItems=None):
    """
    Constructor.

    Calls: L{task.Task.__init__}
    """
    task.Task.__init__(self, u"Assign cells to model set and validation set",
         scriptDirectoryName,
         tempDirectoryName=tempDirectoryName,
         outputDirectoryName=outputDirectoryName,
         configurationItems=configurationItems)
    self.__landUseMap1Filename = None
    self.__landUseMap2Filename = None
    self.__validationSet = None
    self.__validationMapFilenameBla = os.tempnam(self.tempDirectoryName())
    self.__modelSet = None
    self.__mask = None
    self.__modelSetSampleSize = None
    self.__splitMapFilename = None

    self.setSectionTitle(u"Split Dataset")

    self.testRequiredOption(u"landUseMap1")
    self.testRequiredOption(u"landUseMap2")
    self.testRequiredOption(u"validationSet")
    self.testRequiredOption(u"modelSet")
    self.testRequiredOption(u"splitMap")

    self.__landUseMap1Filename = self.option(u"landUseMap1")
    self.__landUseMap2Filename = self.option(u"landUseMap2")
    self.__validationSet = self.option(u"validationSet")
    # FIXME test whether option contains only a basename. Do this with
    # FIXME all output filenames from the ini file.
    # FIXME treat input same as output, use stuff from DEFAULT section.
    # FIXME KISS first
    # FIXME DEFAULT sectie vrij in te vullen???
    self.__modelSet = self.option(u"modelSet")
    if self.hasOption(u"mask"):
      self.__mask = self.option(u"mask")

    if self.hasOption(u"modelSetSampleSize"):
      self.__modelSetSampleSize = self.option(u"modelSetSampleSize")

    self.__splitMapFilename = os.path.join(self.outputDirectoryName(),
         self.option(u"splitMap"))

  def landUseMap1Filename(self):
    """
    FIXME
    """
    return self.__landUseMap1Filename

  def landUseMap2Filename(self):
    """
    FIXME
    """
    return self.__landUseMap2Filename

  def validationSet(self):
    """
    FIXME
    """
    return self.__validationSet

  def __validationMapFilename(self):
    """
    FIXME
    """
    return self.__validationMapFilenameBla

  def splitMapFilename(self):
    """
    FIXME
    """
    return self.__splitMapFilename

  def modelSet(self):
    """
    FIXME
    """
    return self.__modelSet

  def maskIsSet(self):
    """
    FIXME
    """
    return bool(self.__mask)

  def mask(self):
    """
    FIXME
    """
    assert self.maskIsSet()
    return self.__mask

  def modelSetSampleSizeIsSet(self):
    """
    FIXME
    """
    return bool(self.__modelSetSampleSize)

  def modelSetSampleSize(self):
    """
    FIXME
    """
    assert self.modelSetSampleSizeIsSet()
    return self.__modelSetSampleSize

  def testLandUseMapOptions(self):
    """
    FIXME
    """
    try:
      utils.testFileIsReadable(self.landUseMap1Filename())
      utils.testFileIsReadable(self.landUseMap2Filename())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseInvalidOptionException(u"land use map", exception)

  def validationSetIsFilename(self):
    """
    FIXME
    """
    return os.path.exists(self.validationSet())

  def validationSetIsPercentage(self):
    """
    FIXME
    """
    result = False

    if not self.validationSetIsFilename():
      try:
        float(self.validationSet())
        result = True
      except ValueError, exception:
        pass

    return result

  def validationSetIsStatement(self):
    """
    FIXME
    """
    return not self.validationSetIsFilename() and \
         not self.validationSetIsPercentage()

  def testValidationSetOption(self):
    """
    FIXME
    """
    assert self.validationSet()

    try:
      if self.validationSetIsFilename():
        utils.testFileIsReadable(self.validationSet())
      elif self.validationSetIsPercentage():
        utils.testStringIsPositiveFloat(self.validationSet())
        utils.testNumberWithinRange(float(self.validationSet()), 0.0, 100.0)
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseInvalidOptionException(u"validation set", exception)

  def testSplitMapOption(self):
    """
    FIXME
    """
    try:
      utils.testFileIsWritable(self.splitMapFilename())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseInvalidOptionException(u"split map", exception)

  def testModelSetOption(self):
    """
    FIXME
    """
    assert self.modelSet()

    try:
      utils.testStringIsPositiveFloat(self.modelSet())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseInvalidOptionException(u"model set", exception)

  def testMask(self):
    """
    FIXME
    """
    try:
      if self.maskIsSet():
        utils.testFileIsReadable(self.mask())
        utils.testRasterHasValueScale(self.mask(), ["boolean"])
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseInvalidOptionException(u"mask", exception)

  def testModelSetSampleSize(self):
    """
    FIXME
    """
    try:
      if self.modelSetSampleSizeIsSet():
        utils.testStringIsPositiveIntegral(self.modelSetSampleSize())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseInvalidOptionException(u"model set sample size", exception)

  def initialise(self):
    """
    FIXME
    """
    self.decorateOptionException(self.testLandUseMapOptions)
    self.decorateOptionException(self.testValidationSetOption)
    self.decorateOptionException(self.testModelSetOption)
    self.decorateOptionException(self.testMask)
    self.decorateOptionException(self.testModelSetSampleSize)
    self.decorateOptionException(self.testSplitMapOption)

  def createValidationSetMapUsingFilename(self,
         filename):
    """
    FIXME
    """

    # Default selection mask is the whole raster of course.
    mask = "1"

    if self.maskIsSet():
      mask = "%s" % (utils.filenameToPCRasterStatement(self.mask()))

    # self.__validationMapFilename = filename
    self.__validationMapFilenameBla = filename
    statement = "%s = if(%s, %s)" % (
         utils.filenameToPCRasterStatement(filename),
         mask,
         utils.filenameToPCRasterStatement(filename))

    try:
      # self.createValidationSetMapUsingStatement(statement)
      utils.executePCRCalc(statement, logFilename=self.logFilename())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException(u"error creating valset using filename: %s" %
         (filename), exception)

  def createValidationSetMapUsingStatement(self, statement):
    """
    FIXME
    """
    try:
      utils.executePCRCalc(statement, logFilename=self.logFilename())

      # createValidationSetMapUsingFilename will apply the mask for us.
      self.createValidationSetMapUsingFilename(self.__validationMapFilename())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException(u"error creating valset using statement: %s" %
         (statement), exception)

    # It is possible that the statement executed but didn't create the thing
    # we need.
    if not os.path.exists(self.__validationMapFilename()):
      raise exceptions.OSError(u"statement failed to create %s: %s" %
         (self.__validationMapFilename(), statement))

  def createValidationSetMapUsingPercentage(self, percentage):
    """
    FIXME
    """

    # Default selection mask is the whole raster of course.
    mask = "1"

    if self.maskIsSet():
      mask = "\"%s\"" % (self.mask())

    script = u"""\
binding
  mask = %s;
  landUse1 = "%s";
  landUse2 = "%s";
  percentage = %g;
  valset = "%s";

initial
  selection = mask and defined(landUse1) and defined(landUse2);
  order = areaorder(uniform(selection), landUse1);
  fractions = order / areamaximum(order, landUse1);
  report valset = fractions <= percentage / 100;
""" % (  mask,
         self.landUseMap1Filename(),
         self.landUseMap2Filename(),
         percentage,
         self.__validationMapFilename())

    try:
      utils.executePCRCalcScript(script, logFilename=self.logFilename())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException(u"error creating validation set using percentage", exception)
    assert os.path.exists(self.__validationMapFilename())

  def determineValidationSet(self):
    """
    FIXME
    """
    if self.validationSetIsFilename():
      filename = self.validationSet()
      self.createValidationSetMapUsingFilename(filename)
    elif self.validationSetIsPercentage():
      percentage = float(self.validationSet())
      self.createValidationSetMapUsingPercentage(percentage)
    else:
      statement = "%s = %s" % \
         (self.__validationMapFilename(), self.validationSet())
      self.createValidationSetMapUsingStatement(statement)
    assert(os.path.exists(self.__validationMapFilename()))

  def createSplitMap(self):
    """
    FIXME
    """

    # Default selection mask is the whole raster of course.
    mask = "1"

    if self.maskIsSet():
      mask = "\"%s\"" % (self.mask())

    script = u"""\
binding
  mask = %s;
  landUse1 = "%s";
  landUse2 = "%s";
  valset = "%s";
  split = "%s";

initial
  selection = mask and defined(landUse1) and defined(landUse2);
  report split = if(selection, if(valset, nominal(%d), nominal(%d)));
""" % ( mask, self.landUseMap1Filename(), self.landUseMap2Filename(),
         self.__validationMapFilename(), self.splitMapFilename(),
         constants.validationSetClass, constants.modelSetClass)

    try:
      utils.executePCRCalcScript(script, logFilename=self.logFilename())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException(u"error creating split map", exception)
    assert(os.path.exists(self.splitMapFilename()))

  def resampleModelSet(self):
    """
    FIXME
    """
    unchangedSamplePercentage = "%g * nrCellsChanged / nrCellsUnchanged" % \
       (float(self.modelSet()) / 100.0)
    modelSetSampleSize = "nrCells"

    if self.modelSetSampleSizeIsSet():
      modelSetSampleSize = self.modelSetSampleSize()

    script = u"""\
binding
  split = "%s";
  modelSetClass = %d;
  modelSetSampleClass = %d;
  landUse1 = "%s";
  landUse2 = "%s";

initial
  changed = split == modelSetClass and landUse1 != landUse2;
  unchanged = split == modelSetClass and landUse1 == landUse2;
  nrCellsChanged = maptotal(scalar(changed));
  nrCellsUnchanged = maptotal(scalar(unchanged));
  nrCells = nrCellsChanged + nrCellsUnchanged;

  # Take a subset of the unchanged cells in the model set.
  unchangedSamplePercentage = %s;
  order = areaorder(uniform(unchanged), landUse1);
  fractions = order / areamaximum(order, landUse1);
  unchanged = cover(fractions <= unchangedSamplePercentage, 0);

  # Model set sample contains all changed cells from the model set and the
  # subset of the unchanged cells from the model set.
  sample = changed or unchanged;
  nrCells = maptotal(scalar(sample));

  # Create a sample of the model set sample just created.
  sampleSize = %s;
  samplePercentage = sampleSize / nrCells;
  sample = cover(uniform(sample) <= samplePercentage, 0);

  report split = if(sample, modelSetSampleClass, split);
""" % (  self.splitMapFilename(),
         constants.modelSetClass,
         constants.modelSetSampleClass,
         self.landUseMap1Filename(),
         self.landUseMap2Filename(),
         unchangedSamplePercentage,
         modelSetSampleSize)

    try:
      utils.executePCRCalcScript(script, logFilename=self.logFilename())
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException(u"error dermining model set", exception)
    assert(os.path.exists(self.splitMapFilename()))

  def logSplitResults(self):
    try:
      tableName = os.tempnam(self.tempDirectoryName())

      utils.executeTable(
         "--unitcell -0 %s %s" % (self.splitMapFilename(), tableName),
         logFilename=self.logFilename())

      scores = {}

      for line in file(tableName).readlines():
        scores[int(line.split()[0])] = int(line.split()[1])

      os.remove(tableName)
    except AssertionError, exception:
      raise
    except Exception, exception:
      utils.raiseException(u"error dermining amount of cells", exception)

    assert scores.has_key(constants.validationSetClass), \
         "Validation set is empty"
    assert scores.has_key(constants.modelSetClass), \
         "Model set is empty"
    assert scores.has_key(constants.modelSetSampleClass), \
         "Model set sample is empty"
    validationSetSize = scores[constants.validationSetClass]
    modelSetSize = scores[constants.modelSetClass]
    modelSetSampleSize = scores[constants.modelSetSampleClass]

    self.writeLog("""\
split data set     : %d cells
validation set     : %d cells
model set          : %d cells
model set sample   : %d cells
"""  % ( validationSetSize + modelSetSize + modelSetSampleSize,
         validationSetSize,
         modelSetSize + modelSetSampleSize,
         modelSetSampleSize))

  def run(self):
    """
    FIXME
    """
    if not self.optionIsSet("Skip"):
      self.determineValidationSet()
      self.createSplitMap()
      self.resampleModelSet()

      if self.logShouldBeCreated():
        self.logSplitResults()

  def clean(self):
    """
    FIXME
    """
    if not self.validationSetIsFilename() and \
         os.path.exists(self.__validationMapFilename()):
      os.remove(self.__validationMapFilename())
