#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os
import unittest
import testcase
import runoff
from pcraster.framework import *


def getPath(dirname, filename):
  return os.path.join(dirname, filename)

def fileCompare(file1, file2):
  f1 = open(file1).read()
  f2 = open(file2).read()
  return f1.replace(" ","") == f2.replace(" ", "")

class TimeoutputTest(testcase.TestCase):
  def test_01(self):
    """ test demo execute dynamic script
    includes multiple calls of tss.sample()
    includes idMap as filename/field
    includes with/without header
    includes one mv sampling location
    """
    myModel = runoff.RunoffModel("mask.map")
    dynModelFw = DynamicFramework(myModel, lastTimeStep=28)
    dynModelFw.setQuiet(True)
    dynModelFw.run()

    self.assertEqual(True, fileCompare("runoff_file.tss", getPath("validated", "runoff.tss")))
    self.assertEqual(True, fileCompare("runoff_file.tss", getPath("validated", "runoff.tss")))
    self.assertEqual(True, fileCompare("runoff_field.tss", getPath("validated", "runoffNoHeader.tss")))
    self.assertEqual(True, fileCompare("runoff_field_mv.tss", getPath("validated", "runoff_mv.tss")))

  def test_02(self):
    """ test demo execute dynamic script with modified start time
    includes multiple calls calls of tss.sample()
    includes idMap as filename/field
    includes with/without header
    """
    myModel = runoff.RunoffModel("mask.map")
    dynModelFw = DynamicFramework(myModel, lastTimeStep=28, firstTimestep=9)
    dynModelFw.setQuiet(True)
    dynModelFw.run()
    self.assertEqual(True, fileCompare("runoff_file.tss", getPath("validated", "runoff_9_28.tss")))
    self.assertEqual(True, fileCompare("runoff_field.tss", getPath("validated", "runoffNoHeader_9_28.tss")))

  def test_03(self):
    """ test demo excript in MC mode"""
    myModel = runoff.RunoffModelMC("mask.map")
    dynModelFw = DynamicFramework(myModel, lastTimeStep=28)
    dynModelFw.setQuiet(True)
    mcModel = MonteCarloFramework(dynModelFw, 3)
    mcModel.setQuiet(True)
    mcModel.run()
    self.assertEqual(True, fileCompare(getPath("1", "runoff_file.tss"), getPath("validated", "runoff_1.tss")))
    self.assertEqual(True, fileCompare(getPath("2", "runoff_file.tss"), getPath("validated", "runoff_2.tss")))
    self.assertEqual(True, fileCompare(getPath("3", "runoff_file.tss"), getPath("validated", "runoff_3.tss")))
    self.assertEqual(True, fileCompare(getPath("1", "runoff_field.tss"), getPath("validated", "runoff_1_NoH.tss")))
    self.assertEqual(True, fileCompare(getPath("2", "runoff_field.tss"), getPath("validated", "runoff_2_NoH.tss")))
    self.assertEqual(True, fileCompare(getPath("3", "runoff_field.tss"), getPath("validated", "runoff_3_NoH.tss")))

  def test_04(self):
    """ test demo excript in MC mode, shorter timesteps """
    myModel = runoff.RunoffModelMC("mask.map")
    dynModelFw = DynamicFramework(myModel, 28, firstTimestep=9)
    dynModelFw.setQuiet(True)
    mcModel = MonteCarloFramework(dynModelFw, 3)
    mcModel.setQuiet(True)
    mcModel.run()
    self.assertEqual(True, fileCompare(getPath("1", "runoff_file.tss"), getPath("validated", "runoff_short_1.tss")))
    self.assertEqual(True, fileCompare(getPath("2", "runoff_file.tss"), getPath("validated", "runoff_short_2.tss")))
    self.assertEqual(True, fileCompare(getPath("3", "runoff_file.tss"), getPath("validated", "runoff_short_3.tss")))
    self.assertEqual(True, fileCompare(getPath("1", "runoff_field.tss"), getPath("validated", "runoff_short_1_NoH.tss")))
    self.assertEqual(True, fileCompare(getPath("2", "runoff_field.tss"), getPath("validated", "runoff_short_2_NoH.tss")))
    self.assertEqual(True, fileCompare(getPath("3", "runoff_field.tss"), getPath("validated", "runoff_short_3_NoH.tss")))

  def test_05(self):
    """ attempt to tss write native data types """
    myModel = runoff.FloatReport("mask.map")
    dynModelFw = DynamicFramework(myModel, lastTimeStep=28)
    dynModelFw.setQuiet(True)
    errorCatched=False
    try:
      dynModelFw.run()
    except AttributeError as e:
      errorCatched=True
      self.assertEqual(str(e), "Argument must be a PCRaster map, type 'float' given. If necessary use data conversion functions like scalar()")
    self.assert_(errorCatched)

  def test_06(self):
    """ bug: using a boolean map as idMap generates 1e31"""
    class Model(DynamicModel):
      def __init__(self, cloneMap):
        setclone(cloneMap)

      def initial(self):
        # initialise timeoutput
        self.tss = TimeoutputTimeseries("test6", self, "mask.map")

      def dynamic(self):
        self.tss.sample(spatial(scalar(1)))

    m = Model("mask.map")
    dynModelFw = DynamicFramework(m, lastTimeStep=4)
    dynModelFw.run()
    tssLines = open("test6.tss").readlines()
    self.assertEqual(tssLines[-2].strip(),"3              1")

  def test_07(self):
    """ test allowed data types  for idMap """
    class Model(DynamicModel):
      def __init__(self, cloneMap):
        setclone(cloneMap)
      def initial(self):
        self.tss = TimeoutputTimeseries("test7", self, scalar("mask.map"))
      def dynamic(self):
         pass

    errorCatched=False
    try:
     m = Model("mask.map")
     dynModelFw = DynamicFramework(m, lastTimeStep=4)
     dynModelFw.run()
    except Exception as e:
     errorCatched=True
     self.assertEqual(str(e), "idMap must be of type Nominal, Ordinal or Boolean")
    self.assert_(errorCatched)

  def test_08(self):
    """ bug: can not create tss in sub directory"""
    class Model(DynamicModel):
      def __init__(self, cloneMap):
        setclone(cloneMap)

      def initial(self):
        # initialise timeoutput
        os.mkdir("dirForTest8")
        self.tss = TimeoutputTimeseries(os.path.join("dirForTest8","test8"), self, "mask.map")

      def dynamic(self):
        self.tss.sample(spatial(scalar(1)))

    m = Model("mask.map")
    dynModelFw = DynamicFramework(m, lastTimeStep=4)
    dynModelFw.run()
    self.assert_(os.path.exists(os.path.join("dirForTest8","test8.tss")))

  def test_09(self):
    class Model(DynamicModel, MonteCarloModel):
      def __init__(self, cloneMap):
        DynamicModel.__init__(self)
        MonteCarloModel.__init__(self)
        setclone(cloneMap)

      def premcloop(self):
          pass

      def initial(self):
        # initialise timeoutput
        os.mkdir("{}/{}".format(self.currentSampleNumber(), "dirForTest9"))
        self.tss = TimeoutputTimeseries(os.path.join("dirForTest9","test9"),
            self, "mask.map")

      def dynamic(self):
        self.tss.sample(spatial(scalar(1)))

      def postmcloop(self):
          pass

    m = Model("mask.map")
    dynModelFw = DynamicFramework(m, lastTimeStep=4)
    mcModelFw = MonteCarloFramework(dynModelFw, nrSamples=2)
    mcModelFw.run()
    self.assertTrue(os.path.exists(os.path.join("1", "dirForTest9",
      "test9.tss")))
    self.assertTrue(os.path.exists(os.path.join("2", "dirForTest9",
      "test9.tss")))
    self.assertFalse(os.path.exists(os.path.join("3", "dirForTest9",
      "test9.tss")))

  def test_10(self):
    """ Model contains only one cell """
    class Model(DynamicModel):
      def __init__(self):
        nrRows = 1
        nrCols = 1
        cellSize = 5.0
        west = 7.0
        north = 8.0
        setclone(nrRows, nrCols, cellSize, west, north)
      def initial(self):
        locs = ordinal(1)
        self.tss = TimeoutputTimeseries("test10", self, locs)
      def dynamic(self):
        self.tss.sample(spatial(scalar(0.1 + self.currentTimeStep())))

    m = Model()
    dynModelFw = DynamicFramework(m, lastTimeStep=5)
    dynModelFw.run()
    self.assertEqual(True, fileCompare("test10.tss", getPath("validated", "one_cell.tss")))
