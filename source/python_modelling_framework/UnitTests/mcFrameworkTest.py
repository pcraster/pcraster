#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os
import shutil
import sys
import unittest
import pcraster
import pcraster.framework.staticFramework as sf
import pcraster.framework.dynamicFramework as df
import pcraster.framework.mcFramework as mf
import pcraster.framework.frameworkBase as fb
import staticTestModels
import dynamicTestModels
import mcTestModels


## \brief unit tests monte carlo framework
class mcFrameworkTestScript(unittest.TestCase):
  def test_1(self):
    """test type of user model"""
    myModel = mcTestModels.T0()
    try:
      mcFw = mf.MonteCarloFramework(myModel, 5)
    except fb.FrameworkError as e:
      self.assertEqual(str(e),"Cannot run MonteCarlo framework: User model must be type of StaticFramework or DynamicFramework")

  def test_2(self):
    """test existence of added methods and attributes"""
    myModel = mcTestModels.staticModel()
    statFrw = sf.StaticFramework(myModel)
    mcFw = mf.MonteCarloFramework(statFrw, 5)
    self.assert_(hasattr(myModel, "nrSamples"))
    self.assert_(hasattr(myModel, "_d_firstSampleNumber"))
    self.assert_(hasattr(myModel, "_d_lastSampleNumber"))
    self.assert_(hasattr(myModel, "_d_currentSampleNumber"))
    self.assert_(hasattr(myModel, "_d_inSample"))
    self.assert_(hasattr(myModel, "currentSampleNumber"))
    self.assert_(hasattr(myModel, "_lastSampleNumber"))
    self.assert_(hasattr(myModel, "_firstSampleNumber"))
    self.assert_(hasattr(myModel, "_setCurrentSample"))
    self.assert_(hasattr(myModel, "_inSample"))
    self.assert_(hasattr(myModel, "sampleNumbers"))
    self.assert_(hasattr(myModel, "report"))
    self.assert_(hasattr(myModel, "readmap"), "todo")



  def test_3(self):
    """test framework methods"""
    myModel = mcTestModels.dynamicModel()
    dynFrw = df.DynamicFramework(myModel, 10)
    mcFw = mf.MonteCarloFramework(dynFrw, 5)

    self.assert_(myModel.sampleNumbers() == range(1,6))


  def test_4(self):
    """test generation of sample directories"""
    myModel = mcTestModels.staticModel()
    statFrw = sf.StaticFramework(myModel)
    mcFw = mf.MonteCarloFramework(statFrw, 5)
    directoriesCreated = True
    for directory in range(1,6):
      if not os.path.isdir(str(directory)):
        directoriesCreated = False
    self.assert_(directoriesCreated)

    for directory in range(1,6):
      shutil.rmtree(str(directory))

    myModel = mcTestModels.dynamicModel()
    dynFrw = df.DynamicFramework(myModel, 10)
    mcFw = mf.MonteCarloFramework(dynFrw, 5)
    directoriesCreated = True
    for directory in range(1,6):
      if not os.path.isdir(str(directory)):
        directoriesCreated = False
    self.assert_(directoriesCreated)

    for directory in range(1,6):
      shutil.rmtree(str(directory))


  def test_5(self):
    """test execution of sections, report, readmap for a static model"""
    myModel = mcTestModels.staticModel()
    statFrw = sf.StaticFramework(myModel)
    mcFw = mf.MonteCarloFramework(statFrw, 5)
    mcFw.setQuiet(True)
    mcFw.run()

    filesInitialCreated = True
    filesPremcCreated = True
    filesPostmcCreated = True

    for sample in range(1,6):
      nameInit = "mcsi%d.map" % (sample)
      namePre = "premc%d.map" % (sample)
      namePost ="postmc%d.map" % (sample)
      if not os.path.isfile(os.path.join(str(sample), nameInit)):
        filesInitialCreated = False
      if not os.path.isfile(namePre):
        filesPremcCreated = False
      if not os.path.isfile(namePost):
        filesPostmcCreated = False

    self.assert_(filesInitialCreated)
    self.assert_(filesPremcCreated)
    self.assert_(filesPostmcCreated)


  def test_6(self):
    """test execution of sections, report, readmap for a static model"""
    myModel = mcTestModels.dynamicModel()
    dynFrw = df.DynamicFramework(myModel, 10)
    dynFrw.setQuiet(True)
    mcFw = mf.MonteCarloFramework(dynFrw, 5)
    mcFw.setQuiet(True)
    # see if existing directories were emptied
    for directory in range(1,6):
      assert len(os.listdir(str(directory))) == 0

    mcFw.run()

    filesInitialCreated = True
    filesDynamicCreated = True
    filesPremcCreated = True
    filesPostmcCreated = True


    for sample in range(1,6):
      nameInit = "mcdi%d.map" % (sample)
      if not os.path.isfile(os.path.join(str(sample), nameInit)):
        filesInitialCreated = False

      for timestep in range(1,11):
        nameDyn = fb.generateNameT("mcdd%d" % (sample), timestep)
        if not os.path.isfile(os.path.join(str(sample), nameDyn)):
          filesDynamicCreated = False

      for timestep in range(1,11):
        namePre = "premc_%d_%d.map" % (sample, timestep)
        namePost ="postmc_%d_%d.map" % (sample, timestep)

        if not os.path.isfile(namePre):
          filesPremcCreated = False
        if not os.path.isfile(namePost):
          filesPostmcCreated = False

    self.assert_(filesInitialCreated)
    self.assert_(filesPremcCreated)
    self.assert_(filesPostmcCreated)
    self.assert_(filesDynamicCreated)


  # It is important to reset the seed values for the random number generators
  # when a model is used in combination with forking, as clones of the processes
  # are created.
  # Reset of seed is done in the forkscript.py in DEVENV
  #
  # forking creates clones of the processes, 
  def test_7(self):
    """ test random seed reset while forking """
    myModel = mcTestModels.randomModel()
    dynFrw = df.DynamicFramework(myModel, 2)
    dynFrw.setQuiet(True)
    mcFw = mf.MonteCarloFramework(dynFrw, 2)
    mcFw.setQuiet(True)
    mcFw.setForkSamples(True)
    mcFw.run()
    pyVal1 = pcraster.cellvalue(pcraster.readmap(os.path.join("1","pyVal.map")),1,1)[0]
    pyVal2 = pcraster.cellvalue(pcraster.readmap(os.path.join("2","pyVal.map")),1,1)[0]
    pcrVal1 = pcraster.cellvalue(pcraster.readmap(os.path.join("1","pcrVal.map")),1,1)[0]
    pcrVal2 = pcraster.cellvalue(pcraster.readmap(os.path.join("2","pcrVal.map")),1,1)[0]
    npVal1 = pcraster.cellvalue(pcraster.readmap(os.path.join("1","npVal.map")),1,1)[0]
    npVal2 = pcraster.cellvalue(pcraster.readmap(os.path.join("2","npVal.map")),1,1)[0]
    self.assertNotEqual(pyVal1, pyVal2)
    self.assertNotEqual(pcrVal1, pcrVal2)
    self.assertNotEqual(npVal1, npVal2)
